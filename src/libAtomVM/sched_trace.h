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

/**
 * @file sched_trace.h
 * @brief Lock-free scheduler/poll event ring buffer for diagnosing lost
 * wake-ups. Only active when AVM_SCHED_TRACE is defined; all macros compile
 * to nothing otherwise. The ring is dumped to stderr at process exit, on
 * SIGUSR1, or on explicit sched_trace_dump() call.
 *
 * Adapted from commit 15b22259c on the
 * github-desktop-pguyot/w22/sched-trace-repro branch. The scheduler-trace
 * changes are kept; the workflow changes from that scratch commit are not.
 */

#ifndef _SCHED_TRACE_H_
#define _SCHED_TRACE_H_

#ifdef AVM_SCHED_TRACE

#include <stdint.h>

enum SchedTraceType
{
    TR_MAKE_READY = 1, // a=pid, b=flags
    TR_MAKE_READY_SIGNAL, // a=pid, b=0 trylock+waiting, 1 trylock+spawn, 2 trylock-fail, 3 task
    TR_SIGNAL, // sys_signal called
    TR_SIGNAL_CONSUMED, // poller drained the signal fd
    TR_POLL_ENTER, // a=timeout_ms, b=poll_count
    TR_POLL_EXIT, // a=nb_descriptors, b=timeout_ms
    TR_REBUILD, // a=listeners count, b=select events count
    TR_SEL_REG, // a=fd, b=is_write
    TR_SEL_UNREG, // a=fd, b=is_write
    TR_SEL_NOTIFY, // a=fd, b=read|write<<1|matched<<2
    TR_SEL_MSG, // a=fd, b=target pid
    TR_SEL_STOP, // a=fd, b=scheduled
    TR_CLAIM, // poller role claimed
    TR_RELINQUISH, // poller role relinquished, a=selected pid or -1
    TR_CV_WAIT, // entering condvar wait
    TR_CV_WOKE, // woke from condvar wait
    TR_POLL_GATE, // a=grabbed pid or -1, b=wait_timeout
    TR_SET_TIMEOUT, // a=pid, b=timeout_ms
    TR_PROC_WAIT, // a=pid (scheduler_wait)
    TR_DIRECT_HANDOFF, // a=pid grabbed, b=consecutive count for this scheduler
    TR_LISTENER_REG, // a=fd, b=0
    TR_LISTENER_UNREG, // a=fd, b=0
    TR_LISTENER_NOTIFY, // a=fd, b=0
};

void sched_trace(uint8_t type, int32_t a, int64_t b);
void sched_trace_dump(void);

#define SCHED_TRACE(type, a, b) sched_trace((type), (int32_t) (a), (int64_t) (b))

#else

#define SCHED_TRACE(type, a, b)

#endif

#endif
