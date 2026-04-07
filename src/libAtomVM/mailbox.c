/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

#include "mailbox.h"

#include <stddef.h>

#include "context.h"
#include "memory.h"
#include "scheduler.h"
#include "synclist.h"
#include "trace.h"

#ifdef HAVE_PLATFORM_ATOMIC_H
#include "platform_atomic.h"
#else
#if defined(HAVE_ATOMIC)
#include <stdatomic.h>
#define ATOMIC_COMPARE_EXCHANGE_WEAK_PTR atomic_compare_exchange_weak
#endif
#endif

#define ADDITIONAL_PROCESSING_MEMORY_SIZE 4
#define ALIAS_MESSAGE_METADATA_TERM_COUNT ((sizeof(uint64_t) + sizeof(term) - 1) / sizeof(term))

union AliasMessageMetadata
{
    uint64_t ref_ticks;
    term terms[ALIAS_MESSAGE_METADATA_TERM_COUNT];
};

struct AliasMessage
{
    MailboxMessage base;
    term message;
    term *heap_end;
    union AliasMessageMetadata metadata;
    term storage[];
};

struct MessageLike
{
    MailboxMessage base;
    term message;
    term *heap_end;
};

void mailbox_init(Mailbox *mbx)
{
    mbx->outer_first = NULL;
    mbx->inner_first = NULL;
    mbx->inner_last = NULL;
    mbx->receive_pointer = NULL;
    mbx->receive_pointer_prev = NULL;
    mbx->receive_has_match_clauses = false;
}

// Convert a mailbox message (struct Message or struct TermSignal) to a heap
// fragment (HeapFragment) so it can be owned by the recipient.
// We assert this layout mapping is correct.
_Static_assert(offsetof(struct Message, base) + offsetof(struct MailboxMessage, next) == offsetof(HeapFragment, next) ? 1 : 0,
    "Message.base.next doesn't match HeapFragment.next");
_Static_assert(offsetof(struct Message, base) + offsetof(struct MailboxMessage, type) == offsetof(HeapFragment, heap_end) ? 1 : 0,
    "Message.base.type doesn't match HeapFragment.heap_end");
_Static_assert(offsetof(struct Message, message) == offsetof(HeapFragment, storage) ? 1 : 0,
    "Message.message doesn't match HeapFragment.storage[0]");
_Static_assert(offsetof(struct Message, heap_end) == offsetof(HeapFragment, storage[1]) ? 1 : 0,
    "Message.heap_end doesn't match HeapFragment.storage[1]");
_Static_assert(sizeof(struct Message) == sizeof(HeapFragment) + 2 * sizeof(term) ? 1 : 0,
    "sizeof(Message) doesn't match sizeof(HeapFragment) + 2 terms");
_Static_assert(offsetof(struct TermSignal, base) + offsetof(struct MailboxMessage, next) == offsetof(HeapFragment, next) ? 1 : 0,
    "TermSignal.base.next doesn't match HeapFragment.next");
_Static_assert(offsetof(struct TermSignal, base) + offsetof(struct MailboxMessage, type) == offsetof(HeapFragment, heap_end) ? 1 : 0,
    "TermSignal.base.type doesn't match HeapFragment.heap_end");
_Static_assert(offsetof(struct TermSignal, signal_term) == offsetof(HeapFragment, storage) ? 1 : 0,
    "TermSignal.signal_term doesn't match HeapFragment.storage[0]");
_Static_assert(offsetof(struct TermSignal, heap_end) == offsetof(HeapFragment, storage[1]) ? 1 : 0,
    "TermSignal.heap_end doesn't match HeapFragment.storage[1]");
_Static_assert(sizeof(struct TermSignal) == sizeof(HeapFragment) + 2 * sizeof(term) ? 1 : 0,
    "sizeof(TermSignal) doesn't match sizeof(HeapFragment) + 2 terms");
_Static_assert(offsetof(struct AliasMessage, message) == offsetof(struct MessageLike, message) ? 1 : 0,
    "AliasMessage.message doesn't match MessageLike.message");
_Static_assert(offsetof(struct AliasMessage, heap_end) == offsetof(struct MessageLike, heap_end) ? 1 : 0,
    "AliasMessage.heap_end doesn't match MessageLike.heap_end");
_Static_assert(offsetof(struct AliasMessage, message) == offsetof(struct Message, message) ? 1 : 0,
    "AliasMessage.message doesn't match Message.message");
_Static_assert(offsetof(struct AliasMessage, heap_end) == offsetof(struct Message, heap_end) ? 1 : 0,
    "AliasMessage.heap_end doesn't match Message.heap_end");

static inline term mailbox_message_payload(MailboxMessage *m)
{
    struct MessageLike *message = (struct MessageLike *) m;
    return message->message;
}

HeapFragment *mailbox_message_to_heap_fragment(void *m, term *heap_end)
{
    HeapFragment *fragment = (HeapFragment *) m;
    fragment->next = NULL; // MailboxMessage.next
    fragment->heap_end = heap_end; // MailboxMessage.type/heap_fragment_end
    // We don't need to erase Message.message/TermSignal.signal_term as they are valid terms
    // Message.heap_end or TrapSignal.heap_end are not valid terms, put nil
    fragment->storage[1] = term_nil(); // Message/TrapSignal.heap_end

    return fragment;
}

static HeapFragment *mailbox_alias_message_to_heap_fragment(struct AliasMessage *alias_message)
{
    HeapFragment *fragment = mailbox_message_to_heap_fragment(alias_message, alias_message->heap_end);
    for (size_t i = 0; i < ALIAS_MESSAGE_METADATA_TERM_COUNT; ++i) {
        alias_message->metadata.terms[i] = term_nil();
    }

    return fragment;
}

static void mailbox_alias_message_dispose_unsent(struct AliasMessage *alias_message, GlobalContext *global, bool from_task)
{
    term mso_list = alias_message->storage[STORAGE_MSO_LIST_INDEX];
    memory_sweep_mso_list(mso_list, global, from_task);
    free(alias_message);
}

static void mailbox_reply_demonitor(Context *ctx, uint64_t ref_ticks, bool process_table_locked)
{
    bool is_monitoring = false;
    term monitor_pid = context_get_monitor_pid(ctx, ref_ticks, &is_monitoring);
    context_demonitor(ctx, ref_ticks);
    if (LIKELY(is_monitoring) && LIKELY(!term_is_invalid_term(monitor_pid))) {
        int local_process_id = term_to_local_process_id(monitor_pid);
        Context *target = process_table_locked
            ? globalcontext_get_process_nolock(ctx->global, local_process_id)
            : globalcontext_get_process_lock(ctx->global, local_process_id);
        if (target) {
            mailbox_send_ref_signal(target, DemonitorSignal, ref_ticks);
            if (!process_table_locked) {
                globalcontext_get_process_unlock(ctx->global, target);
            }
        }
    }
}

static size_t mailbox_message_size(MailboxMessage *msg)
{
    switch (msg->type) {
        case NormalMessage: {
            Message *normal_message = CONTAINER_OF(msg, Message, base);
            return sizeof(Message) + (size_t) (normal_message->heap_end - normal_message->storage);
        }
        case AliasMessage: {
            struct AliasMessage *alias_message = CONTAINER_OF(msg, struct AliasMessage, base);
            return sizeof(struct AliasMessage) + (size_t) (alias_message->heap_end - alias_message->storage);
        }
        default:
            return 0;
    }
}

// Dispose message. Normal / signal messages are not destroyed, instead they
// are appended to the current heap.
void mailbox_message_dispose(MailboxMessage *m, Heap *heap)
{
    switch (m->type) {
        case NormalMessage: {
            Message *normal_message = CONTAINER_OF(m, Message, base);
            term mso_list = normal_message->storage[STORAGE_MSO_LIST_INDEX];
            HeapFragment *fragment = mailbox_message_to_heap_fragment(normal_message, normal_message->heap_end);
            memory_heap_append_fragment(heap, fragment, mso_list);
            break;
        }
        case AliasMessage: {
            struct AliasMessage *alias_message = CONTAINER_OF(m, struct AliasMessage, base);
            term mso_list = alias_message->storage[STORAGE_MSO_LIST_INDEX];
            HeapFragment *fragment = mailbox_alias_message_to_heap_fragment(alias_message);
            memory_heap_append_fragment(heap, fragment, mso_list);
            break;
        }
        case KillSignal:
        case TrapAnswerSignal:
        case SetGroupLeaderSignal:
        case LinkExitSignal:
        case MonitorDownSignal:
        case UnlinkRemoteIDSignal:
        case UnlinkRemoteIDAckSignal: {
            struct TermSignal *term_signal = CONTAINER_OF(m, struct TermSignal, base);
            term mso_list = term_signal->storage[STORAGE_MSO_LIST_INDEX];
            HeapFragment *fragment = mailbox_message_to_heap_fragment(term_signal, term_signal->heap_end);
            memory_heap_append_fragment(heap, fragment, mso_list);
            break;
        }
        case ProcessInfoRequestSignal: {
            struct BuiltInAtomRequestSignal *request_signal
                = CONTAINER_OF(m, struct BuiltInAtomRequestSignal, base);
            free(request_signal);
            break;
        }
        case TrapExceptionSignal: {
            struct ImmediateSignal *immediate_signal = CONTAINER_OF(m, struct ImmediateSignal, base);
            free(immediate_signal);
            break;
        }
        case UnlinkIDSignal:
        case UnlinkIDAckSignal: {
            struct ImmediateRefSignal *immediate_ref_signal = CONTAINER_OF(m, struct ImmediateRefSignal, base);
            free(immediate_ref_signal);
            break;
        }
        case FlushMonitorSignal:
        case FlushInfoMonitorSignal:
        case DemonitorSignal: {
            struct RefSignal *ref_signal = CONTAINER_OF(m, struct RefSignal, base);
            free(ref_signal);
            break;
        }
        case MonitorSignal: {
            struct MonitorPointerSignal *monitor_signal = CONTAINER_OF(m, struct MonitorPointerSignal, base);
            free(monitor_signal);
            break;
        }
        case CodeServerResumeSignal:
        case GCSignal:
            free(m);
            break;
    }
}

// Dispose message. Normal / signal messages are not destroyed, instead they
// are appended to the current heap.
void mailbox_message_dispose_unsent(Message *m, GlobalContext *global, bool from_task)
{
    term mso_list = m->storage[STORAGE_MSO_LIST_INDEX];
    HeapFragment *fragment = mailbox_message_to_heap_fragment(m, m->heap_end);
    memory_sweep_mso_list(mso_list, global, from_task);
    memory_destroy_heap_fragment(fragment);
}

void mailbox_destroy(Mailbox *mbox, Heap *heap)
{
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        MailboxMessage *next = msg->next;
        mailbox_message_dispose(msg, heap);
        msg = next;
    }
    msg = mbox->inner_first;
    while (msg) {
        MailboxMessage *next = msg->next;
        mailbox_message_dispose(msg, heap);
        msg = next;
    }
}

size_t mailbox_len(Mailbox *mbox)
{
    size_t result = 0;
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        result++;
        msg = msg->next;
    }
    msg = mbox->inner_first;
    while (msg) {
        result++;
        msg = msg->next;
    }
    return result;
}

size_t mailbox_size(Mailbox *mbox)
{
    size_t result = 0;
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        // We don't count signals.
        result += mailbox_message_size(msg);
        msg = msg->next;
    }
    msg = mbox->inner_first;
    while (msg) {
        result += mailbox_message_size(msg);
        msg = msg->next;
    }
    return result;
}

// Messages are enqueued using atomics (or emulation) unless this is a no-smp
// build with no support for driver tasks
#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
inline void mailbox_enqueue_message(Context *c, MailboxMessage *m)
{
    // Append message at the beginning of outer_first.
    MailboxMessage *current_first = NULL;
    do {
        m->next = current_first;
    } while (!ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(&c->mailbox.outer_first, &current_first, m));
}

void mailbox_post_message(Context *c, MailboxMessage *m)
{
    mailbox_enqueue_message(c, m);
    scheduler_signal_message(c);
}
#else
void mailbox_post_message(Context *c, MailboxMessage *m)
{
    m->next = c->mailbox.outer_first;
    c->mailbox.outer_first = m;
    scheduler_signal_message(c);
}
#endif

MailboxMessage *mailbox_message_create_from_term(enum MessageType type, term t)
{
    unsigned long estimated_mem_usage = memory_estimate_usage(t) + 1; // mso_list

    size_t base_size = type == NormalMessage ? sizeof(Message) : sizeof(struct TermSignal);
    void *msg_buf = malloc(base_size + estimated_mem_usage * sizeof(term));
    if (IS_NULL_PTR(msg_buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }

    if (type == NormalMessage) {
        Message *msg = msg_buf;
        msg->base.type = NormalMessage;
        msg->message = memory_copy_term_tree_to_storage(msg->storage, &msg->heap_end, t);

        return &msg->base;
    } else {
        struct TermSignal *ts = msg_buf;
        ts->base.type = type;
        ts->signal_term = memory_copy_term_tree_to_storage(ts->storage, &ts->heap_end, t);

        return &ts->base;
    }
}

Message *mailbox_message_create_normal_message_from_term(term t)
{
    MailboxMessage *message = mailbox_message_create_from_term(NormalMessage, t);
    return CONTAINER_OF(message, Message, base);
}

static MailboxMessage *mailbox_message_create_alias_message_from_term(term t, uint64_t ref_ticks)
{
    unsigned long estimated_mem_usage = memory_estimate_usage(t) + 1; // mso_list

    struct AliasMessage *msg = malloc(sizeof(struct AliasMessage) + estimated_mem_usage * sizeof(term));
    if (IS_NULL_PTR(msg)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }

    msg->base.type = AliasMessage;
    msg->message = memory_copy_term_tree_to_storage(msg->storage, &msg->heap_end, t);
    msg->metadata.ref_ticks = ref_ticks;

    return &msg->base;
}

void mailbox_send(Context *c, term t)
{
    MailboxMessage *msg = mailbox_message_create_from_term(NormalMessage, t);
    mailbox_post_message(c, msg);
}

void mailbox_send_alias(Context *c, term t, uint64_t ref_ticks)
{
    MailboxMessage *msg = mailbox_message_create_alias_message_from_term(t, ref_ticks);
    if (LIKELY(msg != NULL)) {
        mailbox_post_message(c, msg);
    }
}

void mailbox_send_term_signal(Context *c, enum MessageType type, term t)
{
    MailboxMessage *signal = mailbox_message_create_from_term(type, t);
    mailbox_post_message(c, signal);
}

void mailbox_send_immediate_signal(Context *c, enum MessageType type, term immediate)
{
    struct ImmediateSignal *immediate_signal = malloc(sizeof(struct ImmediateSignal));
    if (IS_NULL_PTR(immediate_signal)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    immediate_signal->base.type = type;
    immediate_signal->immediate = immediate;

    mailbox_post_message(c, &immediate_signal->base);
}

void mailbox_send_built_in_atom_request_signal(
    Context *c, enum MessageType type, int32_t pid, term atom)
{
    struct BuiltInAtomRequestSignal *atom_request = malloc(sizeof(struct BuiltInAtomRequestSignal));
    if (IS_NULL_PTR(atom_request)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    atom_request->base.type = type;
    atom_request->sender_pid = pid;
    atom_request->atom = atom;

    mailbox_post_message(c, &atom_request->base);
}

void mailbox_send_ref_signal(Context *c, enum MessageType type, uint64_t ref_ticks)
{
    struct RefSignal *ref_signal = malloc(sizeof(struct RefSignal));
    if (IS_NULL_PTR(ref_signal)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    ref_signal->base.type = type;
    ref_signal->ref_ticks = ref_ticks;

    mailbox_post_message(c, &ref_signal->base);
}

void mailbox_send_immediate_ref_signal(Context *c, enum MessageType type, term immediate, uint64_t ref_ticks)
{
    struct ImmediateRefSignal *immediate_ref_signal = malloc(sizeof(struct ImmediateRefSignal));
    if (IS_NULL_PTR(immediate_ref_signal)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    immediate_ref_signal->base.type = type;
    immediate_ref_signal->immediate = immediate;
    immediate_ref_signal->ref_ticks = ref_ticks;

    mailbox_post_message(c, &immediate_ref_signal->base);
}

void mailbox_send_monitor_signal(Context *c, enum MessageType type, struct Monitor *monitor)
{
    struct MonitorPointerSignal *monitor_signal = malloc(sizeof(struct MonitorPointerSignal));
    if (IS_NULL_PTR(monitor_signal)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    monitor_signal->base.type = type;
    monitor_signal->monitor = monitor;

    mailbox_post_message(c, &monitor_signal->base);
}

void mailbox_send_empty_body_signal(Context *c, enum MessageType type)
{
    MailboxMessage *m = malloc(sizeof(MailboxMessage));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    m->type = type;

    mailbox_post_message(c, m);
}

void mailbox_reset(Mailbox *mbox)
{
    mbox->receive_pointer = mbox->inner_first;
    mbox->receive_pointer_prev = NULL;
}

static MailboxMessage *mailbox_process_outer_list_maybe_locked(Context *ctx, bool process_table_locked)
{
    Mailbox *mbox = &ctx->mailbox;
    // Empty outer list using CAS
    MailboxMessage *current = mbox->outer_first;
#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
    while (!ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(&mbox->outer_first, &current, NULL)) {
    };
#else
    mbox->outer_first = NULL;
#endif
    // Reverse the detached LIFO outer list into received order first, so any
    // alias side effects happen oldest-to-newest.
    MailboxMessage *received_first = NULL;
    while (current) {
        MailboxMessage *next = current->next;
        current->next = received_first;
        received_first = current;
        current = next;
    }

    MailboxMessage *first_normal = NULL;
    MailboxMessage *last_normal = NULL;
    MailboxMessage *first_signal = NULL;
    MailboxMessage *last_signal = NULL;
    current = received_first;
    while (current) {
        MailboxMessage *next = current->next;
        if (current->type == NormalMessage || current->type == AliasMessage) {
            if (current->type == AliasMessage) {
                struct AliasMessage *alias_message = CONTAINER_OF(current, struct AliasMessage, base);
                struct MonitorAlias *alias = context_find_alias(ctx, alias_message->metadata.ref_ticks);
                if (IS_NULL_PTR(alias)) {
                    mailbox_alias_message_dispose_unsent(alias_message, ctx->global, false);
                    current = next;
                    continue;
                }
                if (alias->alias_type == ContextMonitorAliasReplyDemonitor) {
                    mailbox_reply_demonitor(ctx, alias_message->metadata.ref_ticks, process_table_locked);
                }
            }
            current->next = NULL;
            if (first_normal == NULL) {
                first_normal = current;
            } else {
                last_normal->next = current;
            }
            last_normal = current;
        } else {
            current->next = NULL;
            if (first_signal == NULL) {
                first_signal = current;
            } else {
                last_signal->next = current;
            }
            last_signal = current;
        }
        current = next;
    }
    if (first_normal) {
        // If we had no receive_pointer, it should be this list head
        if (mbox->receive_pointer == NULL) {
            mbox->receive_pointer = first_normal;
            // If we had a prev, set the prev's next to the new current.
            if (mbox->receive_pointer_prev) {
                mbox->receive_pointer_prev->next = first_normal;
            } else if (mbox->inner_first == NULL) {
                // If we had no first, this is the first message.
                mbox->inner_first = first_normal;
            }
        }

        // Update last and previous last's next.
        // Append these new items at the end of inner list.
        if (mbox->inner_last) {
            // This may be mbox->receive_pointer_prev which we
            // are updating a second time here.
            mbox->inner_last->next = first_normal;
        }
        mbox->inner_last = last_normal;
    }

    return first_signal;
}

MailboxMessage *mailbox_process_outer_list(Context *ctx)
{
    return mailbox_process_outer_list_maybe_locked(ctx, false);
}

MailboxMessage *mailbox_process_outer_list_locked(Context *ctx)
{
    return mailbox_process_outer_list_maybe_locked(ctx, true);
}

void mailbox_next(Mailbox *mbox)
{
    // This is called from OP_LOOP_REC_END opcode, so we cannot make any
    // assumption about the state and should perform a nop if moving cursor
    // beyond last position.
    if (UNLIKELY(mbox->receive_pointer == NULL)) {
        fprintf(stderr, "OP_LOOP_REC_END beyond mailbox end\n");
        return;
    }

    mbox->receive_pointer_prev = mbox->receive_pointer;
    mbox->receive_pointer = mbox->receive_pointer->next;
}

bool mailbox_peek(Context *c, term *out)
{
    MailboxMessage *m = c->mailbox.receive_pointer;
    if (m == NULL) {
        return false;
    }

    term message = mailbox_message_payload(m);
    TRACE("Pid %i is peeking 0x%lx.\n", c->process_id, message);

    *out = message;

    return true;
}

MailboxMessage *mailbox_take_message(Mailbox *mbox)
{
    // This is called from OP_REMOVE_MESSAGE opcode, so we cannot make any
    // assumption about the state and should perform a nop if the mailbox
    // is empty.
    if (UNLIKELY(mbox->receive_pointer == NULL)) {
        fprintf(stderr, "OP_REMOVE_MESSAGE on empty mailbox\n");
        return NULL;
    }
    MailboxMessage *removed = mbox->receive_pointer;
    if (mbox->receive_pointer_prev) {
        // We did not remove first message.
        mbox->receive_pointer_prev->next = removed->next;
        // If we removed last messages, update inner last.
        if (mbox->inner_last == removed) {
            mbox->inner_last = mbox->receive_pointer_prev;
        }
    } else {
        // We did remove first message.
        mbox->inner_first = removed->next;
        if (mbox->inner_first == NULL) {
            // If this also the last, update inner_last.
            mbox->inner_last = NULL;
        }
    }

    // Reset receive pointers
    mailbox_reset(mbox);

    return removed;
}

Message *mailbox_first(Mailbox *mbox)
{
    mailbox_reset(mbox);
    MailboxMessage *msg = mbox->receive_pointer;
    Message *result = NULL;
    if (LIKELY(msg != NULL) && LIKELY(msg->type == NormalMessage || msg->type == AliasMessage)) {
        result = CONTAINER_OF(msg, Message, base);
    }
    return result;
}

void mailbox_crashdump(Context *ctx)
{
    // Move any pending outer-list items into the mailbox lists before dumping.
    ctx->mailbox.outer_first = mailbox_process_outer_list(ctx);
    MailboxMessage *msg = ctx->mailbox.inner_first;
    while (msg) {
        term_display(stderr, mailbox_message_payload(msg), ctx);
        fprintf(stderr, "\n");
        msg = msg->next;
    }
}
