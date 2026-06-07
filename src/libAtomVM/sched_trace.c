/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

#ifdef AVM_SCHED_TRACE

#include "sched_trace.h"

#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#ifndef AVM_SCHED_TRACE_RING_BITS
#define AVM_SCHED_TRACE_RING_BITS 17
#endif

#define RING_SIZE (1u << AVM_SCHED_TRACE_RING_BITS)
#define RING_MASK (RING_SIZE - 1u)

struct TraceEntry
{
    uint64_t ts_ns;
    int64_t b;
    int32_t a;
    uint8_t tid;
    uint8_t type;
};

static struct TraceEntry ring[RING_SIZE];
static atomic_uint_fast64_t ring_index = 0;
static atomic_int next_tid = 0;
static _Thread_local int my_tid = -1;
static pthread_once_t init_once = PTHREAD_ONCE_INIT;
static uint64_t start_ns;
static const char *dump_path = NULL;
static atomic_int dump_in_progress = 0;

static uint64_t now_ns(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ((uint64_t) ts.tv_sec) * 1000000000ull + (uint64_t) ts.tv_nsec;
}

static void sched_trace_signal_handler(int signo)
{
    (void) signo;
    sched_trace_dump();
}

static void sched_trace_init(void)
{
    start_ns = now_ns();
    dump_path = getenv("AVM_SCHED_TRACE_FILE");
    atexit(sched_trace_dump);

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = sched_trace_signal_handler;
    sigemptyset(&sa.sa_mask);
    /* Best-effort: do not fail startup if we cannot install the handler. */
    (void) sigaction(SIGUSR1, &sa, NULL);
}

void sched_trace(uint8_t type, int32_t a, int64_t b)
{
    pthread_once(&init_once, sched_trace_init);
    if (my_tid < 0) {
        my_tid = atomic_fetch_add(&next_tid, 1);
    }
    uint64_t index = atomic_fetch_add(&ring_index, 1);
    struct TraceEntry *entry = &ring[index & RING_MASK];
    entry->ts_ns = now_ns();
    entry->a = a;
    entry->b = b;
    entry->tid = (uint8_t) my_tid;
    entry->type = type;
}

static const char *type_names[] = {
    [TR_MAKE_READY] = "MAKE_READY",
    [TR_MAKE_READY_SIGNAL] = "MR_SIGNAL",
    [TR_SIGNAL] = "SIGNAL",
    [TR_SIGNAL_CONSUMED] = "SIGNAL_CONSUMED",
    [TR_POLL_ENTER] = "POLL_ENTER",
    [TR_POLL_EXIT] = "POLL_EXIT",
    [TR_REBUILD] = "REBUILD",
    [TR_SEL_REG] = "SEL_REG",
    [TR_SEL_UNREG] = "SEL_UNREG",
    [TR_SEL_NOTIFY] = "SEL_NOTIFY",
    [TR_SEL_MSG] = "SEL_MSG",
    [TR_SEL_STOP] = "SEL_STOP",
    [TR_CLAIM] = "CLAIM",
    [TR_RELINQUISH] = "RELINQUISH",
    [TR_CV_WAIT] = "CV_WAIT",
    [TR_CV_WOKE] = "CV_WOKE",
    [TR_POLL_GATE] = "POLL_GATE",
    [TR_SET_TIMEOUT] = "SET_TIMEOUT",
    [TR_PROC_WAIT] = "PROC_WAIT",
    [TR_DIRECT_HANDOFF] = "DIRECT_HANDOFF",
    [TR_LISTENER_REG] = "LISTENER_REG",
    [TR_LISTENER_UNREG] = "LISTENER_UNREG",
    [TR_LISTENER_NOTIFY] = "LISTENER_NOTIFY",
};

static void sched_trace_dump_to(FILE *out)
{
    uint64_t end = atomic_load(&ring_index);
    uint64_t begin = end > RING_SIZE ? end - RING_SIZE : 0;
    fprintf(out, "=== SCHED TRACE DUMP: %llu events, showing %llu..%llu\n",
        (unsigned long long) end, (unsigned long long) begin, (unsigned long long) end);
    for (uint64_t i = begin; i < end; i++) {
        struct TraceEntry *entry = &ring[i & RING_MASK];
        const char *name = (entry->type < (sizeof(type_names) / sizeof(type_names[0]))
                               && type_names[entry->type] != NULL)
            ? type_names[entry->type]
            : "?";
        uint64_t rel_ns = entry->ts_ns - start_ns;
        fprintf(out, "TR %llu %llu.%09llu t%u %s %d %lld\n",
            (unsigned long long) i,
            (unsigned long long) (rel_ns / 1000000000ull),
            (unsigned long long) (rel_ns % 1000000000ull),
            entry->tid,
            name,
            entry->a,
            (long long) entry->b);
    }
    fprintf(out, "=== SCHED TRACE DUMP END\n");
    fflush(out);
}

void sched_trace_dump(void)
{
    /* Best-effort guard: if multiple threads call this concurrently we only
     * let one through to avoid interleaving the dump. The atomic exchange
     * also makes it safe to call from signal handlers in the common case
     * where we are not already dumping.
     */
    int expected = 0;
    if (!atomic_compare_exchange_strong(&dump_in_progress, &expected, 1)) {
        return;
    }

    FILE *out = stderr;
    FILE *opened = NULL;
    if (dump_path != NULL && dump_path[0] != '\0') {
        opened = fopen(dump_path, "a");
        if (opened != NULL) {
            out = opened;
        }
    }
    sched_trace_dump_to(out);
    if (opened != NULL) {
        fclose(opened);
    }
    atomic_store(&dump_in_progress, 0);
}

#else

/* ISO C forbids an empty translation unit */
typedef int sched_trace_unused_t;

#endif
