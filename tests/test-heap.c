/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

#include <assert.h>
#include <stdlib.h>

#include "atomics.h"
#include "context.h"
#include "counters.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "memory.h"
#include "refc_binary.h"
#include "term.h"
#include "utils.h"

void test_memory_ensure_free(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
    enum MemoryGCResult res = memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);
    size_t memory_size = memory_heap_memory_size(&ctx->heap);
    assert(memory_size == 0);

    res = memory_ensure_free(ctx, TUPLE_SIZE(3));
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == TUPLE_SIZE(3));

    term tuple = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(tuple, 0, OK_ATOM);
    term_put_tuple_element(tuple, 1, TRUE_ATOM);
    term_put_tuple_element(tuple, 2, FALSE_ATOM);

    res = memory_ensure_free_with_roots(ctx, 0, 1, &tuple, MEMORY_FORCE_SHRINK);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == TUPLE_SIZE(3));

    res = memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == 0);
}

void test_gc_ref_count(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
    enum MemoryGCResult res = memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);
    size_t memory_size = memory_heap_memory_size(&ctx->heap);
    assert(memory_size == 0);

    struct ListHead *refc_binaries = synclist_nolock(&glb->refc_binaries);
    assert(list_is_empty(refc_binaries));

    res = memory_ensure_free(ctx, TUPLE_SIZE(2) + 2 * TERM_BOXED_REFC_BINARY_SIZE);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == TUPLE_SIZE(2) + 2 * TERM_BOXED_REFC_BINARY_SIZE);

    term refc_one = term_alloc_refc_binary(42, false, &ctx->heap, glb);
    struct RefcBinary *refc_one_ptr = term_refc_binary_ptr(refc_one);
    term refc_two = term_alloc_refc_binary(43, false, &ctx->heap, glb);
    struct RefcBinary *refc_two_ptr = term_refc_binary_ptr(refc_two);

    term tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple, 0, refc_one);
    term_put_tuple_element(tuple, 1, refc_two);

    term roots[2];
    roots[0] = tuple;
    roots[1] = refc_two;

    res = memory_ensure_free_with_roots(ctx, 0, 2, roots, MEMORY_FORCE_SHRINK);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == TUPLE_SIZE(2) + 2 * TERM_BOXED_REFC_BINARY_SIZE);

    assert(refc_one_ptr->ref_count == 1);
    assert(refc_two_ptr->ref_count == 1);

    res = memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == 0);

    refc_binaries = synclist_nolock(&glb->refc_binaries);
    assert(list_is_empty(refc_binaries));
}

void test_atomic_resources_are_rooted_during_gc(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;

    term atomics_new_args[] = { term_from_int11(1), term_from_int11(1) };
    term atomics_ref = nif_erts_internal_atomics_new_2(ctx, 2, atomics_new_args);
    assert(!term_is_invalid_term(atomics_ref));
    assert(memory_ensure_free_with_roots(ctx, 0, 1, &atomics_ref, MEMORY_FORCE_SHRINK) == MEMORY_GC_OK);
    assert(context_avail_free_memory(ctx) == 0);

    term add_get_args[] = { atomics_ref, term_from_int11(1), term_from_int11(7) };
    term value = nif_atomics_add_get_3(ctx, 3, add_get_args);
    assert(term_is_integer(value));
    assert(term_to_int(value) == 7);
    assert(!list_is_empty(synclist_nolock(&glb->refc_binaries)));

    atomics_ref = add_get_args[0];
    assert(memory_ensure_free_with_roots(ctx, 0, 1, &atomics_ref, MEMORY_FORCE_SHRINK) == MEMORY_GC_OK);
    assert(context_avail_free_memory(ctx) == 0);
    term sub_get_args[] = { atomics_ref, term_from_int11(1), term_from_int11(2) };
    value = nif_atomics_sub_get_3(ctx, 3, sub_get_args);
    assert(term_is_integer(value));
    assert(term_to_int(value) == 5);
    assert(!list_is_empty(synclist_nolock(&glb->refc_binaries)));

    atomics_ref = sub_get_args[0];
    assert(memory_ensure_free_with_roots(ctx, 0, 1, &atomics_ref, MEMORY_FORCE_SHRINK) == MEMORY_GC_OK);
    assert(context_avail_free_memory(ctx) == 0);
    term exchange_args[] = { atomics_ref, term_from_int11(1), term_from_int11(9) };
    value = nif_atomics_exchange_3(ctx, 3, exchange_args);
    assert(term_is_integer(value));
    assert(term_to_int(value) == 5);
    assert(!list_is_empty(synclist_nolock(&glb->refc_binaries)));

    atomics_ref = exchange_args[0];
    assert(memory_ensure_free_with_roots(ctx, 0, 1, &atomics_ref, MEMORY_FORCE_SHRINK) == MEMORY_GC_OK);
    assert(context_avail_free_memory(ctx) == 0);
    term atomics_info_args[] = { atomics_ref };
    term atomics_info = nif_atomics_info_1(ctx, 1, atomics_info_args);
    assert(term_is_map(atomics_info));
    assert(!list_is_empty(synclist_nolock(&glb->refc_binaries)));

    term unsigned_new_args[] = { term_from_int11(1), term_from_int11(0) };
    term unsigned_ref = nif_erts_internal_atomics_new_2(ctx, 2, unsigned_new_args);
    assert(!term_is_invalid_term(unsigned_ref));
    assert(memory_ensure_free_with_roots(ctx, 0, 1, &unsigned_ref, MEMORY_FORCE_SHRINK) == MEMORY_GC_OK);
    assert(context_avail_free_memory(ctx) == 0);
    term unsigned_add_get_args[] = { unsigned_ref, term_from_int11(1), term_from_int11(1) };
    value = nif_atomics_add_get_3(ctx, 3, unsigned_add_get_args);
    assert(term_is_integer(value));
    assert(term_to_int(value) == 1);
    assert(!list_is_empty(synclist_nolock(&glb->refc_binaries)));

    term counters_new_args[] = { term_from_int11(1) };
    term counters_ref = nif_erts_internal_counters_new_1(ctx, 1, counters_new_args);
    assert(!term_is_invalid_term(counters_ref));
    assert(memory_ensure_free_with_roots(ctx, 0, 1, &counters_ref, MEMORY_FORCE_SHRINK) == MEMORY_GC_OK);
    assert(context_avail_free_memory(ctx) == 0);

    term counters_get_args[] = { counters_ref, term_from_int11(1) };
    value = nif_erts_internal_counters_get_2(ctx, 2, counters_get_args);
    assert(term_is_integer(value));
    assert(term_to_int(value) == 0);
    assert(!list_is_empty(synclist_nolock(&glb->refc_binaries)));

    counters_ref = counters_get_args[0];
    assert(memory_ensure_free_with_roots(ctx, 0, 1, &counters_ref, MEMORY_FORCE_SHRINK) == MEMORY_GC_OK);
    assert(context_avail_free_memory(ctx) == 0);
    term counters_info_args[] = { counters_ref };
    term counters_info = nif_erts_internal_counters_info_1(ctx, 1, counters_info_args);
    assert(term_is_map(counters_info));
    assert(!list_is_empty(synclist_nolock(&glb->refc_binaries)));

    globalcontext_destroy(glb);
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_memory_ensure_free();
    test_gc_ref_count();
    test_atomic_resources_are_rooted_during_gc();

    return EXIT_SUCCESS;
}
