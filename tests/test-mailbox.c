/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

#include "context.h"
#include "globalcontext.h"
#include "mailbox.h"
#include "sys.h"

#ifdef AVM_TASK_DRIVER_ENABLED
#include <pthread.h>
#include <time.h>
#endif

void test_mailbox_send(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    term t;

    assert(!mailbox_has_next(&ctx->mailbox));

    mailbox_send(ctx, term_from_int(1));

    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_has_next(&ctx->mailbox));
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(!mailbox_has_next(&ctx->mailbox));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_mailbox_next(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    term t;

    // Various combination of two sends:
    // 1. send send peek next peek remove peek remove
    mailbox_send(ctx, term_from_int(1));
    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    assert(mailbox_has_next(&ctx->mailbox));
    mailbox_next(&ctx->mailbox);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    assert(!mailbox_has_next(&ctx->mailbox));
    assert(mailbox_len(&ctx->mailbox) == 0);

    // 2. send peek send next peek remove peek remove
    mailbox_send(ctx, term_from_int(1));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_has_next(&ctx->mailbox));
    mailbox_next(&ctx->mailbox);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    assert(!mailbox_has_next(&ctx->mailbox));
    assert(mailbox_len(&ctx->mailbox) == 0);

    // 3. send peek next send peek remove peek remove
    mailbox_send(ctx, term_from_int(1));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    assert(mailbox_has_next(&ctx->mailbox));
    mailbox_next(&ctx->mailbox);

    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    assert(!mailbox_has_next(&ctx->mailbox));
    assert(mailbox_len(&ctx->mailbox) == 0);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

#ifdef AVM_TASK_DRIVER_ENABLED
static void *send_task_driver_message(void *arg)
{
    Context *ctx = arg;
    const struct timespec delay = { .tv_sec = 0, .tv_nsec = 50000000 };
    nanosleep(&delay, NULL);

    // Force deferred delivery in SMP builds as well.
    SMP_SPINLOCK_LOCK(&ctx->global->processes_spinlock);
    globalcontext_send_message_from_task(ctx->global, ctx->process_id, NormalMessage, term_from_int(42));
    SMP_SPINLOCK_UNLOCK(&ctx->global->processes_spinlock);

    return NULL;
}

void test_task_driver_wakeup(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    // Exercise both the initial polling set and its cached reuse.
    for (int i = 0; i < 2; i++) {
        pthread_t task;
        int result = pthread_create(&task, NULL, send_task_driver_message, ctx);
        assert(result == 0);

        uint64_t start = sys_monotonic_time_u64();
        sys_poll_events(glb, 5000);
        uint64_t elapsed = sys_monotonic_time_u64() - start;
        result = pthread_join(task, NULL);
        assert(result == 0);
        UNUSED(result);
        assert(elapsed < sys_monotonic_time_ms_to_u64(2500));

        globalcontext_process_task_driver_queues(glb);
        assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
        term message;
        assert(mailbox_peek(ctx, &message));
        assert(term_to_int(message) == 42);
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);

        // Drain any scheduler wakeup from delivering the queued message.
        sys_poll_events(glb, 1);
        start = sys_monotonic_time_u64();
        sys_poll_events(glb, 50);
        elapsed = sys_monotonic_time_u64() - start;
        assert(elapsed >= sys_monotonic_time_ms_to_u64(40));
    }

    context_destroy(ctx);
    globalcontext_destroy(glb);
}
#endif

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_mailbox_send();
    test_mailbox_next();
#ifdef AVM_TASK_DRIVER_ENABLED
    test_task_driver_wakeup();
#endif

    return EXIT_SUCCESS;
}
