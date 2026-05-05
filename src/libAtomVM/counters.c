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

#include "counters.h"

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
#include "memory.h"
#include "resources.h"
#include "smp.h"
#include "utils.h"

#if defined(HAVE_ATOMIC) && ATOMIC_LLONG_LOCK_FREE == 2
#define COUNTERS_USE_C11_64 1
#endif

#if !defined(AVM_NO_SMP) && !defined(COUNTERS_USE_C11_64)
#define COUNTERS_USE_LOCK 1
#endif

#define COUNTERS_CACHE_LINE_SIZE 64

#if defined(COUNTERS_USE_C11_64)
typedef _Atomic uint64_t CountersCell;
#else
typedef uint64_t CountersCell;
#endif

#define COUNTERS_CELLS_PER_CACHE_LINE (COUNTERS_CACHE_LINE_SIZE / sizeof(CountersCell))

union CountersLine
{
    _Alignas(COUNTERS_CACHE_LINE_SIZE) CountersCell cells[COUNTERS_CELLS_PER_CACHE_LINE];
    uint8_t cache_line[COUNTERS_CACHE_LINE_SIZE];
};

_Static_assert(sizeof(union CountersLine) == COUNTERS_CACHE_LINE_SIZE, "counter line must be one cache line");
_Static_assert(_Alignof(union CountersLine) == COUNTERS_CACHE_LINE_SIZE, "counter line must be cache-line aligned");

struct CountersRef
{
    size_t size;
    size_t scheduler_count;
    size_t memory;
    size_t lines_offset;
};

const ErlNifResourceTypeInit counters_resource_type_init = {
    .members = 0
};

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

static size_t div_ceil_size(size_t dividend, size_t divisor)
{
    return (dividend + divisor - 1) / divisor;
}

static uintptr_t align_up_uintptr(uintptr_t value, size_t alignment)
{
    uintptr_t remainder = value % alignment;
    return remainder == 0 ? value : value + alignment - remainder;
}

static bool get_resource(term ref, Context *ctx, struct CountersRef **counters)
{
    void *obj;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), ref, ctx->global->counters_resource_type, &obj))) {
        return false;
    }
    *counters = (struct CountersRef *) obj;
    return true;
}

static bool get_resource_index(term ref, term index, Context *ctx, struct CountersRef **counters, size_t *zero_based_index)
{
    uint64_t index_value;
    if (UNLIKELY(!get_resource(ref, ctx, counters) || !term_is_uint64(index))) {
        return false;
    }
    index_value = term_to_uint64(index);
    if (UNLIKELY(index_value == 0 || index_value > (*counters)->size)) {
        return false;
    }
    *zero_based_index = (size_t) (index_value - 1);
    return true;
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

static bool get_put_value(term value, uint64_t *out)
{
    if (UNLIKELY(!term_is_int64(value))) {
        return false;
    }
    *out = (uint64_t) term_to_int64(value);
    return true;
}

static bool is_positive_integer(term value)
{
    return term_is_any_integer(value) && !term_is_any_neg_integer(value);
}

static union CountersLine *counter_lines(struct CountersRef *counters)
{
    return (union CountersLine *) ((uint8_t *) counters + counters->lines_offset);
}

static CountersCell *counter_cell(struct CountersRef *counters, size_t index, size_t slot)
{
    union CountersLine *lines = counter_lines(counters);
    size_t line_index = (index / COUNTERS_CELLS_PER_CACHE_LINE) * (counters->scheduler_count + 1) + slot;
    size_t cell_index = index % COUNTERS_CELLS_PER_CACHE_LINE;
    return &lines[line_index].cells[cell_index];
}

static void cell_init(CountersCell *cell, uint64_t value)
{
#if defined(COUNTERS_USE_C11_64)
    atomic_init(cell, value);
#else
    *cell = value;
#endif
}

static uint64_t cell_read(struct CountersRef *counters, size_t index, size_t slot)
{
    CountersCell *cell = counter_cell(counters, index, slot);
#if defined(COUNTERS_USE_C11_64)
    return atomic_load_explicit(cell, memory_order_seq_cst);
#else
    return *cell;
#endif
}

static void cell_store(struct CountersRef *counters, size_t index, size_t slot, uint64_t value)
{
    CountersCell *cell = counter_cell(counters, index, slot);
#if defined(COUNTERS_USE_C11_64)
    atomic_store_explicit(cell, value, memory_order_seq_cst);
#else
    *cell = value;
#endif
}

static void cell_add(struct CountersRef *counters, size_t index, size_t slot, uint64_t incr)
{
    CountersCell *cell = counter_cell(counters, index, slot);
#if defined(COUNTERS_USE_C11_64)
    atomic_fetch_add_explicit(cell, incr, memory_order_seq_cst);
#else
    *cell += incr;
#endif
}

static size_t current_scheduler_slot(Context *ctx, const struct CountersRef *counters)
{
#ifndef AVM_NO_SMP
    int scheduler_id = smp_current_scheduler_id(ctx->global);
    if (UNLIKELY(scheduler_id < 1)) {
        return 1;
    }
    size_t slot = (size_t) scheduler_id;
    if (UNLIKELY(slot > counters->scheduler_count)) {
        slot = ((slot - 1) % counters->scheduler_count) + 1;
    }
    return slot;
#else
    UNUSED(ctx);
    UNUSED(counters);
    return 1;
#endif
}

#if defined(COUNTERS_USE_LOCK)
#define COUNTERS_LOCK(ctx) SMP_SPINLOCK_LOCK(&(ctx)->global->counters_spinlock)
#define COUNTERS_UNLOCK(ctx) SMP_SPINLOCK_UNLOCK(&(ctx)->global->counters_spinlock)
#else
#define COUNTERS_LOCK(ctx) UNUSED(ctx)
#define COUNTERS_UNLOCK(ctx) UNUSED(ctx)
#endif

static bool ensure_counter_value_heap(Context *ctx)
{
    return memory_ensure_free(ctx, term_boxed_integer_size(INT64_MIN)) == MEMORY_GC_OK;
}

static term make_counter_value(Context *ctx, uint64_t value)
{
    int64_t signed_value;
    memcpy(&signed_value, &value, sizeof(signed_value));
    if (UNLIKELY(memory_ensure_free(ctx, term_boxed_integer_size(signed_value)) != MEMORY_GC_OK)) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }
    return term_make_maybe_boxed_int64(signed_value, &ctx->heap);
}

term nif_erts_internal_counters_new_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    if (UNLIKELY(!term_is_uint64(argv[0]))) {
        return is_positive_integer(argv[0]) ? raise_error(ctx, SYSTEM_LIMIT_ATOM) : raise_badarg(ctx);
    }

    uint64_t size_value = term_to_uint64(argv[0]);
    if (UNLIKELY(size_value == 0)) {
        return raise_badarg(ctx);
    }
    if (UNLIKELY(size_value > SIZE_MAX)) {
        return raise_error(ctx, SYSTEM_LIMIT_ATOM);
    }
    size_t size = (size_t) size_value;

#ifndef AVM_NO_SMP
    int configured_schedulers = smp_get_online_processors();
    if (UNLIKELY(configured_schedulers < 1)) {
        configured_schedulers = 1;
    }
    size_t scheduler_count = (size_t) configured_schedulers;
#else
    size_t scheduler_count = 1;
#endif

    size_t slots = scheduler_count + 1;
    size_t line_groups = div_ceil_size(size, COUNTERS_CELLS_PER_CACHE_LINE);
    if (UNLIKELY(slots == 0 || line_groups > SIZE_MAX / slots)) {
        return raise_error(ctx, SYSTEM_LIMIT_ATOM);
    }
    size_t line_count = line_groups * slots;
    if (UNLIKELY(sizeof(struct CountersRef) > SIZE_MAX - (COUNTERS_CACHE_LINE_SIZE - 1)
            || line_count > (SIZE_MAX - sizeof(struct CountersRef) - (COUNTERS_CACHE_LINE_SIZE - 1)) / sizeof(union CountersLine))) {
        return raise_error(ctx, SYSTEM_LIMIT_ATOM);
    }
    size_t bytes = sizeof(struct CountersRef) + (COUNTERS_CACHE_LINE_SIZE - 1) + line_count * sizeof(union CountersLine);
    if (UNLIKELY(bytes > UINT_MAX)) {
        return raise_error(ctx, SYSTEM_LIMIT_ATOM);
    }

    struct CountersRef *counters = enif_alloc_resource(ctx->global->counters_resource_type, bytes);
    if (UNLIKELY(counters == NULL)) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    counters->size = size;
    counters->scheduler_count = scheduler_count;
    counters->memory = bytes;
    counters->lines_offset = (size_t) (align_up_uintptr((uintptr_t) counters + sizeof(struct CountersRef), COUNTERS_CACHE_LINE_SIZE) - (uintptr_t) counters);
    union CountersLine *lines = counter_lines(counters);
    for (size_t line = 0; line < line_count; line++) {
        for (size_t cell = 0; cell < COUNTERS_CELLS_PER_CACHE_LINE; cell++) {
            cell_init(&lines[line].cells[cell], 0);
        }
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        enif_release_resource(counters);
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    term resource = term_from_resource(counters, &ctx->heap);
    enif_release_resource(counters);
    return resource;
}

term nif_erts_internal_counters_get_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct CountersRef *counters;
    size_t index;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &counters, &index))) {
        return raise_badarg(ctx);
    }
    if (UNLIKELY(!ensure_counter_value_heap(ctx))) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    COUNTERS_LOCK(ctx);
    uint64_t value = 0;
    for (size_t slot = 0; slot <= counters->scheduler_count; slot++) {
        value += cell_read(counters, index, slot);
    }
    COUNTERS_UNLOCK(ctx);
    return make_counter_value(ctx, value);
}

term nif_erts_internal_counters_add_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct CountersRef *counters;
    size_t index;
    uint64_t incr;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &counters, &index) || !get_incr(argv[2], &incr))) {
        return raise_badarg(ctx);
    }

    size_t slot = current_scheduler_slot(ctx, counters);
    COUNTERS_LOCK(ctx);
    cell_add(counters, index, slot, incr);
    COUNTERS_UNLOCK(ctx);
    return OK_ATOM;
}

term nif_erts_internal_counters_put_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct CountersRef *counters;
    size_t index;
    uint64_t value;
    if (UNLIKELY(!get_resource_index(argv[0], argv[1], ctx, &counters, &index) || !get_put_value(argv[2], &value))) {
        return raise_badarg(ctx);
    }

    COUNTERS_LOCK(ctx);
    uint64_t write_sum = 0;
    for (size_t slot = 1; slot <= counters->scheduler_count; slot++) {
        write_sum += cell_read(counters, index, slot);
    }
    cell_store(counters, index, 0, value - write_sum);
    COUNTERS_UNLOCK(ctx);
    return OK_ATOM;
}

term nif_erts_internal_counters_info_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct CountersRef *counters;
    if (UNLIKELY(!get_resource(argv[0], ctx, &counters))) {
        return raise_badarg(ctx);
    }

    size_t heap_needed = term_map_size_in_terms(2);
    heap_needed += term_boxed_integer_size((int64_t) counters->size);
    heap_needed += term_boxed_integer_size((int64_t) counters->memory);
    if (UNLIKELY(memory_ensure_free(ctx, heap_needed) != MEMORY_GC_OK)) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    term size_key = globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "size"));
    if (UNLIKELY(term_is_invalid_term(size_key))) {
        return raise_error(ctx, OUT_OF_MEMORY_ATOM);
    }

    term map = term_alloc_map(2, &ctx->heap);
    term_set_map_assoc(map, 0, MEMORY_ATOM, term_make_maybe_boxed_int64((int64_t) counters->memory, &ctx->heap));
    term_set_map_assoc(map, 1, size_key, term_make_maybe_boxed_int64((int64_t) counters->size, &ctx->heap));
    return map;
}
