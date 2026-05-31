/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#include "persistent_term.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include "context.h"
#include "dictionary.h"
#include "globalcontext.h"
#include "memory.h"
#include "smp.h"
#include "sys.h"
#include "term.h"
#include "term_hash.h"
#include "utils.h"

struct PersistentTermEntry
{
    struct PersistentTermEntry *next;
    term key;
    term value;
    Heap *heap;
    size_t memory;
#ifndef AVM_NO_SMP
    uint64_t retire_epoch;
#endif
};

static persistent_term_result_t find_entry(
    PersistentTerm *persistent_term,
    uint32_t bucket_index,
    term key,
    struct PersistentTermEntry ***out_link,
    struct PersistentTermEntry **out_entry,
    GlobalContext *global);
static struct PersistentTermEntry *entry_new(term key, term value);
static void entry_destroy(struct PersistentTermEntry *entry, GlobalContext *global);
static void retire_entry(PersistentTerm *persistent_term, struct PersistentTermEntry *entry);
static bool term_is_equal(term a, term b, GlobalContext *global, persistent_term_result_t *result);
static bool context_has_reference_to_heap(const Context *ctx, const Heap *target_heap);
void persistent_term_reclaim(PersistentTerm *persistent_term, GlobalContext *global);

void persistent_term_init(PersistentTerm *persistent_term)
{
    persistent_term->count = 0;
    persistent_term->memory = 0;
    persistent_term->retired_entries = NULL;
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        persistent_term->buckets[i] = NULL;
    }

#ifndef AVM_NO_SMP
    persistent_term->reclaim_epoch = 0;
    persistent_term->lock = smp_rwlock_create();
#endif
}

void persistent_term_init_process_checkpoint(Context *ctx)
{
#ifndef AVM_NO_SMP
    PersistentTerm *pt = &ctx->global->persistent_term;
    SMP_RWLOCK_RDLOCK(pt->lock);
    ctx->persistent_term_checked_epoch = pt->reclaim_epoch;
    SMP_RWLOCK_UNLOCK(pt->lock);
#else
    UNUSED(ctx);
#endif
}

void persistent_term_destroy(PersistentTerm *persistent_term, GlobalContext *global)
{
    SMP_RWLOCK_WRLOCK(persistent_term->lock);
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        struct PersistentTermEntry *entry = persistent_term->buckets[i];
        while (entry != NULL) {
            struct PersistentTermEntry *next = entry->next;
            entry_destroy(entry, global);
            entry = next;
        }
        persistent_term->buckets[i] = NULL;
    }

    struct PersistentTermEntry *entry = persistent_term->retired_entries;
    while (entry != NULL) {
        struct PersistentTermEntry *next = entry->next;
        entry_destroy(entry, global);
        entry = next;
    }
    persistent_term->retired_entries = NULL;
    persistent_term->count = 0;
    persistent_term->memory = 0;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
#ifndef AVM_NO_SMP
    smp_rwlock_destroy(persistent_term->lock);
    persistent_term->lock = NULL;
#endif
}

persistent_term_result_t persistent_term_put(
    PersistentTerm *persistent_term,
    term key,
    term value,
    bool put_new,
    GlobalContext *global)
{
    uint32_t bucket_index = term_hash(key, global) % PERSISTENT_TERM_NUM_BUCKETS;

    struct PersistentTermEntry *new_entry = entry_new(key, value);
    if (IS_NULL_PTR(new_entry)) {
        return PersistentTermAllocationError;
    }

    SMP_RWLOCK_WRLOCK(persistent_term->lock);

    struct PersistentTermEntry **link;
    struct PersistentTermEntry *entry;
    persistent_term_result_t result = find_entry(persistent_term, bucket_index, key, &link, &entry, global);
    if (UNLIKELY(result != PersistentTermOk)) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        entry_destroy(new_entry, global);
        return result;
    }

    if (entry != NULL) {
        bool equal = term_is_equal(entry->value, value, global, &result);
        if (UNLIKELY(result != PersistentTermOk)) {
            SMP_RWLOCK_UNLOCK(persistent_term->lock);
            entry_destroy(new_entry, global);
            return result;
        }

        if (equal) {
            SMP_RWLOCK_UNLOCK(persistent_term->lock);
            entry_destroy(new_entry, global);
            return PersistentTermOk;
        }

        if (put_new) {
            SMP_RWLOCK_UNLOCK(persistent_term->lock);
            entry_destroy(new_entry, global);
            return PersistentTermExists;
        }
    }

    bool retired = false;
    if (entry == NULL) {
        new_entry->next = persistent_term->buckets[bucket_index];
        persistent_term->buckets[bucket_index] = new_entry;
        persistent_term->count++;
        persistent_term->memory += new_entry->memory;
    } else {
        new_entry->next = entry->next;
        *link = new_entry;
        persistent_term->memory += new_entry->memory;
        retire_entry(persistent_term, entry);
        retired = true;
    }

    SMP_RWLOCK_UNLOCK(persistent_term->lock);
    if (retired) {
#ifndef AVM_NO_SMP
        global->persistent_term_reclaim_pending = true;
        sys_signal(global);
#else
        persistent_term_reclaim(persistent_term, global);
#endif
    }
    return PersistentTermOk;
}

persistent_term_result_t persistent_term_get(
    PersistentTerm *persistent_term,
    term key,
    term *value,
    GlobalContext *global)
{
    assert(value != NULL);

    uint32_t bucket_index = term_hash(key, global) % PERSISTENT_TERM_NUM_BUCKETS;

    SMP_RWLOCK_RDLOCK(persistent_term->lock);

    struct PersistentTermEntry *entry;
    persistent_term_result_t result = find_entry(persistent_term, bucket_index, key, NULL, &entry, global);
    if (UNLIKELY(result != PersistentTermOk)) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return result;
    }

    if (entry == NULL) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return PersistentTermNotFound;
    }

    *value = entry->value;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
    return PersistentTermOk;
}

persistent_term_result_t persistent_term_erase(
    PersistentTerm *persistent_term,
    term key,
    bool *removed,
    GlobalContext *global)
{
    assert(removed != NULL);

    *removed = false;

    uint32_t bucket_index = term_hash(key, global) % PERSISTENT_TERM_NUM_BUCKETS;

    SMP_RWLOCK_WRLOCK(persistent_term->lock);

    struct PersistentTermEntry **link;
    struct PersistentTermEntry *entry;
    persistent_term_result_t result = find_entry(persistent_term, bucket_index, key, &link, &entry, global);
    if (UNLIKELY(result != PersistentTermOk)) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return result;
    }

    if (entry == NULL) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return PersistentTermOk;
    }

    *link = entry->next;
    persistent_term->count--;
    retire_entry(persistent_term, entry);

    *removed = true;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
#ifndef AVM_NO_SMP
    global->persistent_term_reclaim_pending = true;
    sys_signal(global);
#else
    persistent_term_reclaim(persistent_term, global);
#endif
    return PersistentTermOk;
}

persistent_term_result_t persistent_term_get_all_maybe_gc(
    PersistentTerm *persistent_term,
    term *ret,
    Context *ctx)
{
    assert(ret != NULL);

    SMP_RWLOCK_RDLOCK(persistent_term->lock);

    size_t needed = 0;
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        for (struct PersistentTermEntry *entry = persistent_term->buckets[i]; entry != NULL; entry = entry->next) {
            needed += CONS_SIZE + TUPLE_SIZE(2);
        }
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, needed, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return PersistentTermAllocationError;
    }

    term list = term_nil();
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        for (struct PersistentTermEntry *entry = persistent_term->buckets[i]; entry != NULL; entry = entry->next) {
            term tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(tuple, 0, entry->key);
            term_put_tuple_element(tuple, 1, entry->value);
            list = term_list_prepend(tuple, list, &ctx->heap);
        }
    }

    *ret = list;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
    return PersistentTermOk;
}

void persistent_term_info(PersistentTerm *persistent_term, size_t *count, size_t *memory)
{
    assert(count != NULL);
    assert(memory != NULL);

    SMP_RWLOCK_RDLOCK(persistent_term->lock);
    *count = persistent_term->count;
    *memory = persistent_term->memory;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
}

static persistent_term_result_t find_entry(
    PersistentTerm *persistent_term,
    uint32_t bucket_index,
    term key,
    struct PersistentTermEntry ***out_link,
    struct PersistentTermEntry **out_entry,
    GlobalContext *global)
{
    assert(out_entry != NULL);

    *out_entry = NULL;

    struct PersistentTermEntry **link = &persistent_term->buckets[bucket_index];
    while (*link != NULL) {
        persistent_term_result_t result = PersistentTermOk;
        bool equal = term_is_equal((*link)->key, key, global, &result);
        if (UNLIKELY(result != PersistentTermOk)) {
            return result;
        }

        if (equal) {
            if (out_link != NULL) {
                *out_link = link;
            }
            *out_entry = *link;
            return PersistentTermOk;
        }

        link = &(*link)->next;
    }

    if (out_link != NULL) {
        *out_link = link;
    }
    return PersistentTermOk;
}

static struct PersistentTermEntry *entry_new(term key, term value)
{
    struct PersistentTermEntry *entry = malloc(sizeof(struct PersistentTermEntry));
    if (IS_NULL_PTR(entry)) {
        return NULL;
    }

    Heap *heap = malloc(sizeof(Heap));
    if (IS_NULL_PTR(heap)) {
        free(entry);
        return NULL;
    }

    size_t size = memory_estimate_usage(key) + memory_estimate_usage(value);
    if (UNLIKELY(memory_init_heap(heap, size) != MEMORY_GC_OK)) {
        free(heap);
        free(entry);
        return NULL;
    }

    entry->key = memory_copy_term_tree(heap, key);
    entry->value = memory_copy_term_tree(heap, value);
    entry->heap = heap;
    entry->memory = sizeof(struct PersistentTermEntry) + sizeof(Heap) + sizeof(HeapFragment)
        + ((size_t) (heap->heap_ptr - heap->heap_start) * sizeof(term));
    entry->next = NULL;

    return entry;
}

static void entry_destroy(struct PersistentTermEntry *entry, GlobalContext *global)
{
    memory_destroy_heap(entry->heap, global);
    free(entry->heap);
    free(entry);
}

static void retire_entry(PersistentTerm *persistent_term, struct PersistentTermEntry *entry)
{
#ifndef AVM_NO_SMP
    entry->retire_epoch = ++persistent_term->reclaim_epoch;
#endif
    entry->next = persistent_term->retired_entries;
    persistent_term->retired_entries = entry;
}

static bool term_is_pointer_into_heap(term t, const Heap *target_heap)
{
    if (term_is_boxed(t)) {
        return memory_heap_contains_pointer(target_heap, term_to_const_term_ptr(t));
    }
    if (term_is_nonempty_list(t)) {
        return memory_heap_contains_pointer(target_heap, term_get_list_ptr(t));
    }
    return false;
}

static bool heap_range_has_reference_to_heap(const term *start, const term *end, const Heap *target_heap)
{
    for (const term *ptr = start; ptr < end; ptr++) {
        if (term_is_pointer_into_heap(*ptr, target_heap)) {
            return true;
        }
    }
    return false;
}

static bool context_has_reference_to_heap(const Context *ctx, const Heap *target_heap)
{
    if (heap_range_has_reference_to_heap(ctx->heap.heap_start, ctx->heap.heap_ptr, target_heap)) {
        return true;
    }
    const HeapFragment *fragment = ctx->heap.root->next;
    while (fragment) {
        if (heap_range_has_reference_to_heap(fragment->storage, fragment->heap_end, target_heap)) {
            return true;
        }
        fragment = fragment->next;
    }

    term *stack_base = context_stack_base(ctx);
    if (heap_range_has_reference_to_heap(ctx->e, stack_base, target_heap)) {
        return true;
    }

    for (size_t i = 0; i <= MAX_REG; i++) {
        if (term_is_pointer_into_heap(ctx->x[i], target_heap)) {
            return true;
        }
    }

    if (term_is_pointer_into_heap(ctx->cp, target_heap)) {
        return true;
    }

    if (term_is_pointer_into_heap(ctx->bs, target_heap)) {
        return true;
    }

    if (term_is_pointer_into_heap(ctx->exception_reason, target_heap)) {
        return true;
    }

    if (term_is_pointer_into_heap(ctx->exception_stacktrace, target_heap)) {
        return true;
    }

    struct ListHead *item;
    LIST_FOR_EACH(item, &ctx->extended_x_regs) {
        struct ExtendedRegister *ext_reg = CONTAINER_OF(item, struct ExtendedRegister, head);
        if (term_is_pointer_into_heap(ext_reg->value, target_heap)) {
            return true;
        }
    }

    LIST_FOR_EACH(item, &ctx->dictionary) {
        struct DictEntry *dict_entry = CONTAINER_OF(item, struct DictEntry, head);
        if (term_is_pointer_into_heap(dict_entry->key, target_heap)) {
            return true;
        }
        if (term_is_pointer_into_heap(dict_entry->value, target_heap)) {
            return true;
        }
    }

    if (term_is_pointer_into_heap(ctx->exit_reason, target_heap)) {
        return true;
    }

    if (term_is_pointer_into_heap(ctx->group_leader, target_heap)) {
        return true;
    }

    for (MailboxMessage *msg = ctx->mailbox.inner_first; msg != NULL; msg = msg->next) {
        if (msg->type == NormalMessage) {
            Message *m = CONTAINER_OF(msg, Message, base);
            if (heap_range_has_reference_to_heap(
                    m->storage + STORAGE_HEAP_START_INDEX, m->heap_end, target_heap)) {
                return true;
            }
        }
    }

    return false;
}

void persistent_term_process_checkpoint(Context *ctx)
{
#ifndef AVM_NO_SMP
    GlobalContext *global = ctx->global;
    if (!global->persistent_term_reclaim_pending) {
        return;
    }
    PersistentTerm *pt = &global->persistent_term;

    SMP_RWLOCK_WRLOCK(pt->lock);
    if (pt->retired_entries == NULL
        || ctx->persistent_term_checked_epoch >= pt->reclaim_epoch) {
        SMP_RWLOCK_UNLOCK(pt->lock);
        return;
    }

    uint64_t old_checked = ctx->persistent_term_checked_epoch;
    uint64_t new_checked = pt->reclaim_epoch;

    for (struct PersistentTermEntry *entry = pt->retired_entries;
         entry != NULL; entry = entry->next) {
        if (entry->retire_epoch <= old_checked) {
            continue;
        }
        if (context_has_reference_to_heap(ctx, entry->heap)) {
            if (entry->retire_epoch - 1 < new_checked) {
                new_checked = entry->retire_epoch - 1;
            }
        }
    }

    bool advanced = new_checked > old_checked;
    if (advanced) {
        ctx->persistent_term_checked_epoch = new_checked;
    }
    SMP_RWLOCK_UNLOCK(pt->lock);

    if (advanced) {
        persistent_term_reclaim(pt, global);
    }
#else
    UNUSED(ctx);
#endif
}

void persistent_term_reclaim(PersistentTerm *persistent_term, GlobalContext *global)
{
#ifndef AVM_NO_SMP
    if (!global->persistent_term_reclaim_pending) {
        return;
    }
    if (global->persistent_term_reclaim_teardown_guard != 0) {
        return;
    }

    SMP_RWLOCK_WRLOCK(persistent_term->lock);
    if (persistent_term->retired_entries == NULL) {
        global->persistent_term_reclaim_pending = false;
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return;
    }

    struct ListHead *processes = synclist_rdlock(&global->processes_table);
    struct PersistentTermEntry **link = &persistent_term->retired_entries;
    while (*link != NULL) {
        struct PersistentTermEntry *entry = *link;
        bool blocked = false;
        struct ListHead *item;
        LIST_FOR_EACH(item, processes) {
            Context *ctx = CONTAINER_OF(item, Context, processes_table_head);
            if (ctx->persistent_term_checked_epoch < entry->retire_epoch) {
                blocked = true;
                break;
            }
        }
        if (!blocked) {
            *link = entry->next;
            persistent_term->memory -= entry->memory;
            entry_destroy(entry, global);
        } else {
            link = &entry->next;
        }
    }
    synclist_unlock(&global->processes_table);

    global->persistent_term_reclaim_pending = persistent_term->retired_entries != NULL;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
#else
    SMP_RWLOCK_WRLOCK(persistent_term->lock);
    if (persistent_term->retired_entries == NULL) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return;
    }

    struct PersistentTermEntry **link = &persistent_term->retired_entries;
    struct ListHead *processes = synclist_rdlock(&global->processes_table);

    while (*link != NULL) {
        struct PersistentTermEntry *entry = *link;

        bool referenced = false;
        struct ListHead *item;
        LIST_FOR_EACH(item, processes) {
            Context *ctx = CONTAINER_OF(item, Context, processes_table_head);
            if (context_has_reference_to_heap(ctx, entry->heap)) {
                referenced = true;
                break;
            }
        }

        if (!referenced) {
            *link = entry->next;
            persistent_term->memory -= entry->memory;
            entry_destroy(entry, global);
        } else {
            link = &entry->next;
        }
    }

    synclist_unlock(&global->processes_table);
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
#endif
}

static bool term_is_equal(term a, term b, GlobalContext *global, persistent_term_result_t *result)
{
    TermCompareResult compare_result = term_compare(a, b, TermCompareExact, global);
    if (UNLIKELY(compare_result == TermCompareMemoryAllocFail)) {
        *result = PersistentTermAllocationError;
        return false;
    }

    *result = PersistentTermOk;
    return compare_result == TermEquals;
}
