/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. Madsen <petermm@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include "atomics.h"

#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if defined(HAVE_ATOMIC)
#include <stdatomic.h>
#endif

#include "context.h"
#include "defaultatoms.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "intn.h"
#include "memory.h"
#include "resources.h"
#include "smp.h"
#include "utils.h"

#if defined(HAVE_ATOMIC) && ATOMIC_LLONG_LOCK_FREE == 2
#define ATOMICS_USE_C11_64 1
#endif

#if !defined(AVM_NO_SMP) && !defined(ATOMICS_USE_C11_64)
#define ATOMICS_USE_LOCK 1
#endif

#if defined(ATOMICS_USE_C11_64)
typedef _Atomic uint64_t AtomicsCell;
#else
typedef uint64_t AtomicsCell;
#endif

struct AtomicsRef
{
    bool is_signed;
    size_t size;
    size_t memory;
    size_t cells_offset;
};

const ErlNifResourceTypeInit atomics_resource_type_init = {
    .members = 0
};

#define OPT_SIGNED (1 << 0)

static term raise_badarg(Context *ctx)
{
    context_set_exception_class_use_live_flag(ctx, ERROR_ATOM);
    ctx->exception_reason = BADARG_ATOM;
    return term_invalid_term();
}

static term raise_error(Context *ctx, term reason)
{
    context_set_exception_class(ctx, ERROR_ATOM);
    ctx->exception_reason = reason;
    return term_invalid_term();
}

static bool get_resource(term ref, Context *ctx, struct AtomicsRef **atomics)
{
    void *obj;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), ref, ctx->global->atomics_resource_type, &obj))) {
        return false;
    }
    *atomics = (struct AtomicsRef *) obj;
    return true;
}

static bool get_resource_index(term ref, term index, Context *ctx, struct AtomicsRef **atomics, size_t *zero_based_index)
{
    uint64_t index_value;
    if (UNLIKELY(!get_resource(ref, ctx, atomics) || !term_is_uint64(index))) {
        return false;
    }
    index_value = term_to_uint64(index);
    if (UNLIKELY(index_value == 0 || index_value > (*atomics)->size)) {
        return false;
    }
    *zero_based_index = (size_t) (index_value - 1);
    return true;
}

static bool get_value(struct AtomicsRef *atomics, term value, uint64_t *out)
{
    if (atomics->is_signed) {
        if (UNLIKELY(!term_is_int64(value))) {
            return false;
        }
        *out = (uint64_t) term_to_int64(value);
        return true;
    } else {
        if (UNLIKELY(!term_is_uint64(value))) {
            return false;
        }
        *out = term_to_uint64(value);
        return true;
    }
}

static bool get_incr(term value, uint64_t *out)
{
    if (term_is_int64(value)) {
        *out = (uint64_t) term_to_int64(value);
        return true;
    } else if (term_is_uint64(value)) {
        *out = term_to_uint64(value);
        return true;
    }
    return false;
}

static bool get_negated_incr(term value, uint64_t *out, term *error_reason)
{
    if (UNLIKELY(!term_is_number(value))) {
        *error_reason = BADARITH_ATOM;
        return false;
    }
    if (UNLIKELY(!term_is_any_integer(value))) {
        *error_reason = BADARG_ATOM;
        return false;
    }

    if (term_is_int64(value)) {
        int64_t decr = term_to_int64(value);
        *out = decr == INT64_MIN ? ((uint64_t) INT64_MAX) + 1 : (uint64_t) -decr;
        return true;
    }

    if (term_is_any_neg_integer(value)) {
        const intn_digit_t *digits;
        size_t digits_len;
        intn_integer_sign_t sign;
        term_to_bigint(value, &digits, &digits_len, &sign);
        UNUSED(sign);

        if (intn_is_uint64(digits, digits_len)) {
            *out = intn_to_uint64(digits);
            return true;
        }
    }

    *error_reason = BADARG_ATOM;
    return false;
}

static bool ensure_uint64_heap(Context *ctx, uint64_t value)
{
    if (value <= (uint64_t) INT64_MAX) {
        return memory_ensure_free(ctx, term_boxed_integer_size((int64_t) value)) == MEMORY_GC_OK;
    }

    intn_digit_t digits[INTN_UINT64_LEN];
    intn_from_uint64(value, digits);
    size_t digits_len = intn_count_digits(digits, INTN_UINT64_LEN);
    size_t intn_data_size;
    size_t rounded_digits_len;
    term_bigint_size_requirements(digits_len, &intn_data_size, &rounded_digits_len);
    UNUSED(rounded_digits_len);
    return memory_ensure_free(ctx, BOXED_BIGINT_HEAP_SIZE(intn_data_size)) == MEMORY_GC_OK;
}

static term make_uint64_prepared(Context *ctx, uint64_t value);

static term make_uint64(Context *ctx, uint64_t value)
{
    if (UNLIKELY(!ensure_uint64_heap(ctx, value))) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }
    return make_uint64_prepared(ctx, value);
}

static term make_uint64_prepared(Context *ctx, uint64_t value)
{
    if (value <= (uint64_t) INT64_MAX) {
        return term_make_maybe_boxed_int64((int64_t) value, &ctx->heap);
    }

    intn_digit_t digits[INTN_UINT64_LEN];
    intn_from_uint64(value, digits);
    size_t digits_len = intn_count_digits(digits, INTN_UINT64_LEN);
    size_t intn_data_size;
    size_t rounded_digits_len;
    term_bigint_size_requirements(digits_len, &intn_data_size, &rounded_digits_len);
    term bigint = term_create_uninitialized_bigint(intn_data_size, TermPositiveInteger, &ctx->heap);
    term_initialize_bigint(bigint, digits, digits_len, rounded_digits_len);
    return bigint;
}

static term make_atomic_value(Context *ctx, const struct AtomicsRef *atomics, uint64_t value)
{
    if (atomics->is_signed) {
        int64_t signed_value;
        memcpy(&signed_value, &value, sizeof(signed_value));
        if (UNLIKELY(memory_ensure_free(ctx, term_boxed_integer_size(signed_value)) != MEMORY_GC_OK)) {
            return raise_error(ctx, OUT_OF_MEMORY_ATOM);
        }
        return term_make_maybe_boxed_int64(signed_value, &ctx->heap);
    } else {
        return make_uint64(ctx, value);
    }
}

static bool ensure_atomic_value_heap(Context *ctx, const struct AtomicsRef *atomics)
{
    if (atomics->is_signed) {
        return memory_ensure_free(ctx, term_boxed_integer_size(INT64_MIN)) == MEMORY_GC_OK;
    } else {
        return ensure_uint64_heap(ctx, UINT64_MAX);
    }
}

static uintptr_t align_up_uintptr(uintptr_t value, size_t alignment)
{
    uintptr_t remainder = value % alignment;
    return remainder == 0 ? value : value + alignment - remainder;
}

static AtomicsCell *atomics_cells(struct AtomicsRef *atomics)
{
    return (AtomicsCell *) ((uint8_t *) atomics + atomics->cells_offset);
}

static uint64_t cell_read(struct AtomicsRef *atomics, size_t index)
{
    AtomicsCell *cells = atomics_cells(atomics);
#if defined(ATOMICS_USE_C11_64)
    return atomic_load_explicit(&cells[index], memory_order_seq_cst);
#else
    return cells[index];
#endif
}

static void cell_store(struct AtomicsRef *atomics, size_t index, uint64_t value)
{
    AtomicsCell *cells = atomics_cells(atomics);
#if defined(ATOMICS_USE_C11_64)
    atomic_store_explicit(&cells[index], value, memory_order_seq_cst);
#else
    cells[index] = value;
#endif
}

static uint64_t cell_add(struct AtomicsRef *atomics, size_t index, uint64_t incr)
{
    AtomicsCell *cells = atomics_cells(atomics);
#if defined(ATOMICS_USE_C11_64)
    return atomic_fetch_add_explicit(&cells[index], incr, memory_order_seq_cst) + incr;
#else
    cells[index] += incr;
    return cells[index];
#endif
}

static uint64_t cell_exchange(struct AtomicsRef *atomics, size_t index, uint64_t desired)
{
    AtomicsCell *cells = atomics_cells(atomics);
#if defined(ATOMICS_USE_C11_64)
    return atomic_exchange_explicit(&cells[index], desired, memory_order_seq_cst);
#else
    uint64_t old = cells[index];
    cells[index] = desired;
    return old;
#endif
}

static uint64_t cell_compare_exchange(struct AtomicsRef *atomics, size_t index, uint64_t expected, uint64_t desired)
{
    AtomicsCell *cells = atomics_cells(atomics);
#if defined(ATOMICS_USE_C11_64)
    uint64_t old = expected;
    atomic_compare_exchange_strong_explicit(
        &cells[index], &old, desired, memory_order_seq_cst, memory_order_seq_cst);
    return old;
#else
    uint64_t old = cells[index];
    if (old == expected) {
        cells[index] = desired;
    }
    return old;
#endif
}

#if defined(ATOMICS_USE_LOCK)
#define ATOMICS_LOCK(ctx) SMP_SPINLOCK_LOCK(&(ctx)->global->atomics_spinlock)
#define ATOMICS_UNLOCK(ctx) SMP_SPINLOCK_UNLOCK(&(ctx)->global->atomics_spinlock)
#else
#define ATOMICS_LOCK(ctx) UNUSED(ctx)
#define ATOMICS_UNLOCK(ctx) UNUSED(ctx)
#endif

static term atomics_new(Context *ctx, uint64_t size_value, bool is_signed)
{
    if (UNLIKELY(size_value == 0 || size_value > SIZE_MAX)) {
        return raise_badarg(ctx);
    }
    size_t size = (size_t) size_value;
    const size_t cells_alignment = _Alignof(AtomicsCell);
    if (UNLIKELY(sizeof(struct AtomicsRef) > SIZE_MAX - (cells_alignment - 1)
            || size > (SIZE_MAX - sizeof(struct AtomicsRef) - (cells_alignment - 1)) / sizeof(AtomicsCell))) {
        return raise_error(ctx, SYSTEM_LIMIT_ATOM);
    }

    size_t bytes = sizeof(struct AtomicsRef) + (cells_alignment - 1) + size * sizeof(AtomicsCell);
    if (UNLIKELY(bytes > UINT_MAX)) {
        return raise_error(ctx, SYSTEM_LIMIT_ATOM);
    }
    struct AtomicsRef *atomics = enif_alloc_resource(ctx->global->atomics_resource_type, bytes);
    if (UNLIKELY(atomics == NULL)) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    atomics->is_signed = is_signed;
    atomics->size = size;
    atomics->memory = bytes;
    atomics->cells_offset = (size_t) (align_up_uintptr((uintptr_t) atomics + sizeof(struct AtomicsRef), cells_alignment) - (uintptr_t) atomics);
    for (size_t i = 0; i < size; i++) {
        cell_store(atomics, i, 0);
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        enif_release_resource(atomics);
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    term resource = term_from_resource(atomics, &ctx->heap);
    enif_release_resource(atomics);
    return resource;
}

static bool parse_atom_option_list(term opts, bool *is_signed)
{
    *is_signed = true;
    while (term_is_nonempty_list(opts)) {
        term opt = term_get_list_head(opts);
        if (UNLIKELY(!term_is_tuple(opt) || term_get_tuple_arity(opt) != 2)) {
            return false;
        }
        if (UNLIKELY(term_get_tuple_element(opt, 0) != SIGNED_ATOM)) {
            return false;
        }
        term value = term_get_tuple_element(opt, 1);
        if (value == TRUE_ATOM) {
            *is_signed = true;
        } else if (value == FALSE_ATOM) {
            *is_signed = false;
        } else {
            return false;
        }
        opts = term_get_list_tail(opts);
    }
    return term_is_nil(opts);
}

term nif_atomics_new_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    if (UNLIKELY(!term_is_uint64(argv[0]))) {
        return raise_badarg(ctx);
    }
    bool is_signed;
    if (UNLIKELY(!parse_atom_option_list(argv[1], &is_signed))) {
        return raise_badarg(ctx);
    }
    return atomics_new(ctx, term_to_uint64(argv[0]), is_signed);
}

term nif_erts_internal_atomics_new_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    if (UNLIKELY(!term_is_uint64(argv[0]) || !term_is_uint64(argv[1]))) {
        return raise_badarg(ctx);
    }
    uint64_t opts = term_to_uint64(argv[1]);
    return atomics_new(ctx, term_to_uint64(argv[0]), (opts & OPT_SIGNED) != 0);
}

term nif_atomics_put_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct AtomicsRef *atomics;
    size_t index;
    uint64_t value;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &atomics, &index) || !get_value(atomics, argv[2], &value))) {
        return raise_badarg(ctx);
    }

    ATOMICS_LOCK(ctx);
    cell_store(atomics, index, value);
    ATOMICS_UNLOCK(ctx);
    return OK_ATOM;
}

term nif_atomics_get_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct AtomicsRef *atomics;
    size_t index;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &atomics, &index))) {
        return raise_badarg(ctx);
    }

    ATOMICS_LOCK(ctx);
    uint64_t value = cell_read(atomics, index);
    ATOMICS_UNLOCK(ctx);
    return make_atomic_value(ctx, atomics, value);
}

term nif_atomics_add_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct AtomicsRef *atomics;
    size_t index;
    uint64_t incr;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &atomics, &index) || !get_incr(argv[2], &incr))) {
        return raise_badarg(ctx);
    }

    ATOMICS_LOCK(ctx);
    cell_add(atomics, index, incr);
    ATOMICS_UNLOCK(ctx);
    return OK_ATOM;
}

term nif_atomics_add_get_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct AtomicsRef *atomics;
    size_t index;
    uint64_t incr;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &atomics, &index) || !get_incr(argv[2], &incr))) {
        return raise_badarg(ctx);
    }
    if (UNLIKELY(!ensure_atomic_value_heap(ctx, atomics))) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    ATOMICS_LOCK(ctx);
    uint64_t value = cell_add(atomics, index, incr);
    ATOMICS_UNLOCK(ctx);
    return make_atomic_value(ctx, atomics, value);
}

term nif_atomics_sub_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    uint64_t incr;
    term error_reason;
    if (UNLIKELY(!get_negated_incr(argv[2], &incr, &error_reason))) {
        return raise_error(ctx, error_reason);
    }

    struct AtomicsRef *atomics;
    size_t index;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &atomics, &index))) {
        return raise_badarg(ctx);
    }

    ATOMICS_LOCK(ctx);
    cell_add(atomics, index, incr);
    ATOMICS_UNLOCK(ctx);
    return OK_ATOM;
}

term nif_atomics_sub_get_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    uint64_t incr;
    term error_reason;
    if (UNLIKELY(!get_negated_incr(argv[2], &incr, &error_reason))) {
        return raise_error(ctx, error_reason);
    }

    struct AtomicsRef *atomics;
    size_t index;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &atomics, &index))) {
        return raise_badarg(ctx);
    }
    if (UNLIKELY(!ensure_atomic_value_heap(ctx, atomics))) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    ATOMICS_LOCK(ctx);
    uint64_t value = cell_add(atomics, index, incr);
    ATOMICS_UNLOCK(ctx);
    return make_atomic_value(ctx, atomics, value);
}

term nif_atomics_exchange_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct AtomicsRef *atomics;
    size_t index;
    uint64_t desired;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &atomics, &index) || !get_value(atomics, argv[2], &desired))) {
        return raise_badarg(ctx);
    }
    if (UNLIKELY(!ensure_atomic_value_heap(ctx, atomics))) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    ATOMICS_LOCK(ctx);
    uint64_t old = cell_exchange(atomics, index, desired);
    ATOMICS_UNLOCK(ctx);
    return make_atomic_value(ctx, atomics, old);
}

term nif_atomics_compare_exchange_4(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct AtomicsRef *atomics;
    size_t index;
    uint64_t expected;
    uint64_t desired;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &atomics, &index)
            || !get_value(atomics, argv[2], &expected)
            || !get_value(atomics, argv[3], &desired))) {
        return raise_badarg(ctx);
    }

    ATOMICS_LOCK(ctx);
    uint64_t old = cell_compare_exchange(atomics, index, expected, desired);
    ATOMICS_UNLOCK(ctx);
    return old == expected ? OK_ATOM : make_atomic_value(ctx, atomics, old);
}

term nif_atomics_info_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct AtomicsRef *atomics;
    if (UNLIKELY(!get_resource(argv[0], ctx, &atomics))) {
        return raise_badarg(ctx);
    }

    uint64_t memory = atomics->memory;
    uint64_t max = atomics->is_signed ? (uint64_t) INT64_MAX : UINT64_MAX;
    int64_t min = atomics->is_signed ? INT64_MIN : 0;

    size_t heap_needed = term_map_size_in_terms(4);
    heap_needed += term_boxed_integer_size((int64_t) atomics->size);
    heap_needed += term_boxed_integer_size(min);
    heap_needed += term_boxed_integer_size((int64_t) memory);
    if (max <= (uint64_t) INT64_MAX) {
        heap_needed += term_boxed_integer_size((int64_t) max);
    } else {
        intn_digit_t digits[INTN_UINT64_LEN];
        intn_from_uint64(max, digits);
        size_t digits_len = intn_count_digits(digits, INTN_UINT64_LEN);
        size_t intn_data_size;
        size_t rounded_digits_len;
        term_bigint_size_requirements(digits_len, &intn_data_size, &rounded_digits_len);
        UNUSED(rounded_digits_len);
        heap_needed += BOXED_BIGINT_HEAP_SIZE(intn_data_size);
    }

    if (UNLIKELY(memory_ensure_free(ctx, heap_needed) != MEMORY_GC_OK)) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    term max_key = globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "max"));
    term min_key = globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "min"));
    term size_key = globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "size"));
    if (UNLIKELY(term_is_invalid_term(max_key) || term_is_invalid_term(min_key) || term_is_invalid_term(size_key))) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    term map = term_alloc_map(4, &ctx->heap);
    term_set_map_assoc(map, 0, max_key, atomics->is_signed ? term_make_maybe_boxed_int64(INT64_MAX, &ctx->heap) : make_uint64_prepared(ctx, UINT64_MAX));
    term_set_map_assoc(map, 1, min_key, term_make_maybe_boxed_int64(min, &ctx->heap));
    term_set_map_assoc(map, 2, size_key, term_make_maybe_boxed_int64((int64_t) atomics->size, &ctx->heap));
    term_set_map_assoc(map, 3, MEMORY_ATOM, term_make_maybe_boxed_int64((int64_t) memory, &ctx->heap));
    return map;
}
