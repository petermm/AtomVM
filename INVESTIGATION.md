# Scheduler CI Flakiness Investigation

## Objective

Determine whether scheduler behavior can delay event-loop progress long enough
to cause the intermittent Linux Valgrind CI failures. In particular, measure
whether `63ded8738` changed the frequency, latency, or failure shape of
pre-existing flakiness, without assuming that it introduced the entire problem.

The investigation should answer:

1. Is the designated poller scheduler prevented from running?
2. Does the poller run but remain blocked after a wake or registration?
3. Does `poll(2)` return an event that is lost before the waiting process runs?
4. Is the failure outside the scheduler/event loop?

## Observed Failure Family

| Test area | Observed failure | Event source that must make progress |
| --- | --- | --- |
| `test_net_kernel:test_rpc_loop_from_beam/1` | One RPC times out after 60 seconds, with successful RPCs before and after it | Distribution socket |
| `test_net_kernel:test_autoconnect_to_beam/1` | Distribution exchange completes, but the monitored subprocess does not deliver `DOWN` | Subprocess pipe EOF/process completion |
| `test_tcp_socket` | Intermittent socket timeout | `enif_select` socket readiness |
| `test_serial_dist_socat` | Intermittent serial/distribution timeout | PTY or serial FD readiness |
| Cleanup | Dangling `socket_fd` and `dist_connection` resources after a test exception | Usually secondary to skipped cleanup |

The shared layer is scheduler-driven FD/event progress. Distribution-specific
code cannot explain the subprocess, TCP, and serial failures together. This
does not establish that every historical failure had the same cause.

## Historical Baseline

Errors and flakiness existed before `63ded8738`. The investigation must
therefore compare rates and trace signatures, not classify one revision as
simply good and another as bad.

For each selected revision, use the same:

- CI image and Valgrind version
- Scheduler count and CPU allocation
- Test ordering and iteration count
- Per-test timeout
- Valgrind options
- Trace configuration

Record:

- Runs and failures per test
- Failure location and timeout duration
- Maximum and percentile event-notification latency
- Whether poller, dispatch, and target-process trace signatures match
- Cleanup warnings separately from initiating failures

Prefer enough runs to estimate a rate with useful confidence. If failures are
rare, report counts and total runtime rather than declaring a revision fixed
after a small passing sample.

## Scheduler Comparison Point

Commit `63ded8738` changed `scheduler_run0()` so a non-poller scheduler can take
a ready process directly instead of waking the poller scheduler and waiting.
It also made the scheduler holding the local `is_waiting` role the only
scheduler that calls `sys_poll_events()`.

Treat this commit as a comparison point and possible amplifier, not as a proven
origin. Useful revisions include:

1. A historical revision with known CI flakiness before the recent socket and
   scheduler work.
2. The parent of `63ded8738`.
3. `63ded8738` with only the direct-handoff behavior disabled.
4. Current `HEAD`.

The important paths are:

- `src/libAtomVM/scheduler.c`
  - `scheduler_run0()`
  - `scheduler_first_runnable_ready()`
  - `scheduler_make_ready()`
  - `scheduler_next()`
- `src/platforms/generic_unix/lib/sys.c`
  - `sys_poll_events()`
  - `sys_signal()`
  - `sys_register_listener()`
  - `sys_register_select_event()`
  - `select_event_notify()` call sites

On Linux, registering a listener or select event invalidates the cached poll
set and signals the event FD. Therefore, a simple missing wake on registration
is not currently the leading hypothesis.

## Primary Current Hypothesis

The direct-handoff path may allow a non-poller host thread to repeatedly claim
ready work while the designated poller host thread receives too little runtime.
Valgrind's default native-thread scheduling may amplify this.

If true, `63ded8738` may have increased the probability or duration of an
existing event-progress weakness. FD readiness would not be lost; it would
remain unprocessed until the poller host thread runs. That mechanism could
account for failures across sockets, PTYs, and subprocess pipes.

`--fair-sched=yes` is useful as a diagnostic comparison, but it is not an
acceptable production fix. AtomVM event progress should not depend on Valgrind
or host mutex scheduling fairness.

## Alternative Hypotheses

1. **Poller role stalls**

   `waiting_scheduler` remains true, but no live scheduler continues through
   `sys_poll_events()`.

2. **Poller mutex starvation**

   The poller returns from `poll(2)` but repeatedly loses
   `schedulers_mutex` to a direct-handoff scheduler.

3. **Wake consumed without timely poll-set rebuild**

   `sys_signal()` wakes the poller, but the invalidated listener/select set is
   not rebuilt before the poller blocks again.

4. **Event dispatch succeeds, scheduling fails**

   The event handler marks a process ready, but its flags/list membership keep
   it from being selected promptly.

5. **Platform event bug**

   The poller runs normally, but the generic Unix poll array, descriptor
   indexing, or event dispatch loses an event.

## Scheduler Invariants To Verify

The trace should prove all of these during both successful and failing runs:

1. At most one scheduler owns the poller role.
2. If schedulers are running, the poller role always has a live owner.
3. Pending listeners/select events receive bounded polling latency.
4. A registration or `sys_signal()` causes a blocked poller to return.
5. A ready FD is converted into a listener/select notification.
6. A notification makes the target process runnable.
7. A runnable, non-running process is eventually selected.
8. `waiting_scheduler`, `running_schedulers`, and the local `is_waiting` role
   remain consistent across condition-variable waits and scheduler shutdown.

## Trace Instrumentation

Use a fixed-size, lock-free ring buffer. Avoid `fprintf()` in hot paths because
stdio locking and timing can hide the race.

Record:

- Monotonic timestamp
- Native thread or scheduler identifier
- Event type and sequence number
- Local `is_waiting`
- Global `waiting_scheduler`
- `running_schedulers` and `online_schedulers`
- Selected process ID and its flags
- Ready/running/waiting queue counts, if cheap enough
- Poll timeout and poll return count
- FD and readiness mask for registration and dispatch events

Trace these events:

| Location | Events |
| --- | --- |
| `scheduler_run0()` | entry, poller role acquired, direct handoff, condvar wait/wake, poller role released |
| `scheduler_first_runnable_ready()` | process selected, no runnable process, skipped `Running` process |
| `scheduler_make_ready()` | process flags/list before and after, scheduler wake decision |
| `sys_poll_events()` | poll entry, timeout, return, descriptor count |
| `sys_signal()` | signal write/trigger and errors |
| Listener/select registration | FD added/removed and poll cache invalidated |
| Event dispatch | FD readiness observed, listener/select notified, target PID |

Dump the ring on test timeout, fatal error, or explicit trace request.

The scratch branch
`github-desktop-pguyot/w22/sched-trace-repro` contains an `AVM_SCHED_TRACE`
implementation in commit `15b22259c`. Reuse the trace files and call sites
selectively; do not cherry-pick the whole commit because it also replaces the
workflow set for the reproduction branch.

## Experiment Matrix

Run enough iterations to compare failure rates and latency distributions. A
passing run is evidence, not a boundary. Preserve traces from successful runs
as controls.

| Experiment | Purpose | Interpretation |
| --- | --- | --- |
| Historical flaky revision, parent of `63ded8738`, and current scheduler | Measure baseline and rate changes | A higher rate or distinct trace after the commit supports amplification; matching failures suggest an older shared cause |
| Native run versus Valgrind default | Measure Valgrind amplification | Failure only under Valgrind points toward timing/fairness, not necessarily a Valgrind bug |
| Valgrind default versus `--fair-sched=yes` | Test host-thread starvation | A large reduction with fair scheduling supports poller starvation |
| One online scheduler versus two or more | Remove inter-scheduler handoff | Success with one scheduler implicates SMP coordination |
| Disable only the new direct-handoff branch | Isolate the behavioral change | A meaningful rate or latency reduction implicates repeated handoff rather than unrelated code in the commit |
| Limit consecutive direct handoffs to a small budget | Test bounded poller progress | Success with a low budget supports starvation while preserving most optimization |
| Force a poller yield after direct handoff batches | Test host scheduling fairness | Success indicates the poller needs an explicit progress opportunity |
| Single-CPU affinity versus normal CI CPUs | Increase host-thread contention | More failures on one CPU supports native-thread starvation |
| RPC-only versus mixed socket/pipe/PTY workload | Separate distribution from shared event progress | Cross-transport failures with the same trace signature confirm a scheduler-level cause |

Treat changes such as a handoff budget as diagnostic patches until the trace
shows why they work.

## Reproduction Workloads

Prioritize these in order:

1. Loop `test_net_kernel:test_rpc_loop_from_beam/1`, which has shown a single
   60-second timeout followed by recovery.
2. Loop `test_net_kernel:test_autoconnect_to_beam/1`, preserving the subprocess
   output and the missing `DOWN` timeout.
3. Loop `test_tcp_socket` and `test_serial_dist_socat` separately.
4. Add a focused stress test that continuously:
   - keeps ordinary Erlang processes runnable,
   - registers or waits on a pipe/socket FD,
   - writes to that FD from another native thread or subprocess,
   - asserts a short maximum notification latency.

The focused test should report latency rather than only pass/fail. A growing
tail while direct handoffs continue is more informative than a final timeout.

## Reading A Failing Trace

| Trace result | Likely conclusion | Next investigation |
| --- | --- | --- |
| Direct handoffs continue, with no poll entry for seconds | Poller host thread starvation | Bound handoffs or explicitly yield/wake the poller |
| Poller returns from `poll`, then waits a long time for `schedulers_mutex` | Mutex starvation | Change role/handoff coordination or mutex acquisition pattern |
| `sys_signal()` occurs, but the blocked poll does not return | Wake mechanism problem | Inspect eventfd/pipe state and signal error handling |
| Poll returns the expected FD, but no notification follows | Poll array/index or event dispatch bug | Audit rebuild counts, FD ordering, and descriptor masks |
| Notification makes PID ready, but PID is never selected | Ready-list/flag bug | Audit `Ready`/`Running` transitions and list membership |
| Poller role is owned but its native thread has exited or stopped | Role lifecycle bug | Audit scheduler stop/start and role relinquishment |
| Poller and dispatch are timely, but peer data never arrives | Not primarily scheduler-related | Return to transport or subprocess code |

## Candidate Fix Shapes

Only choose one after a trace identifies the failure mode:

1. Add a bounded number of consecutive non-poller direct handoffs before the
   thread must park and give the poller an opportunity to progress.
2. Explicitly yield or signal at a handoff boundary when pollable resources are
   active.
3. Transfer the poller role explicitly, with an owner identity or generation,
   instead of representing ownership only with `waiting_scheduler`.
4. Correct wake consumption or poll-set rebuild ordering if the trace shows the
   poller runs but misses an invalidation.

Do not make every scheduler call `sys_poll_events()` without first protecting
or redesigning the generic Unix platform state. The cached `pollfd` array and
poll counts are currently shared and assume a single event-loop driver.

## Known Separate Issues

Keep these out of the scheduler conclusion unless a trace directly connects
them:

- A fresh `DistConnection` allocated by `nif_erlang_setnode_3` does not
  initialize `selecting_process_id`.
- `socket_dist_controller` dequeues distribution data before `socket:send/2`
  and ignores partial-send or error results.
- The Valgrind `sys_port(port_getn)` issue is specific to illumos/Solaris and
  does not explain Linux `poll(2)` stalls.
- Dangling resource warnings after an `etest` exception are probably cleanup
  fallout, not the initiating fault.

These should be fixed independently, but neither known distribution issue
explains the shared socket, serial, and subprocess failures.

## Acceptance Criteria

A scheduler fix is ready when:

1. The trace demonstrates the targeted failure mechanism and the fix removes
   that trace signature.
2. Repeated Linux Valgrind runs with default scheduling show a meaningful
   improvement over the measured historical and current baselines.
3. The RPC, autoconnect, TCP, serial, and subprocess cases all retain bounded
   event latency.
4. Native non-Valgrind throughput does not regress materially.
5. ThreadSanitizer reports no new scheduler or platform event-loop races.
6. Linux `poll`, macOS/FreeBSD `kqueue`, eventfd, and pipe wake paths preserve
   the single-poller invariants.

## Recommended Order

1. Establish comparable historical, pre-`63ded8738`, and current baselines.
2. Add low-perturbation scheduler and poll traces.
3. Capture both successful and failing traces under Valgrind default
   scheduling.
4. Determine whether old and new failures share a trace signature.
5. Classify each signature using the table above.
6. Apply the smallest diagnostic change that tests the leading classification.
7. Convert a successful diagnostic into a principled scheduler fix.
8. Run the full experiment and acceptance matrix.

## Work Log

### Trace infrastructure

* Added `src/libAtomVM/sched_trace.h` and `src/libAtomVM/sched_trace.c`,
  adapted from commit `15b22259c` on
  `github-desktop-pguyot/w22/sched-trace-repro`. The implementation:
  * is gated behind `AVM_SCHED_TRACE`; SCHED_TRACE expands to nothing
    otherwise;
  * uses a fixed-size lock-free ring buffer (`1 << 17` entries by default,
    tunable with `AVM_SCHED_TRACE_RING_BITS`);
  * dumps on process exit (`atexit`), on `SIGUSR1`, or on explicit
    `sched_trace_dump()` call;
  * writes to `stderr` by default, or to `$AVM_SCHED_TRACE_FILE` when set.
* Adds a per-thread `direct_handoff_streak` counter so the trace records the
  consecutive number of times a single scheduler thread has taken a process
  via direct handoff without going through poll/condvar. This is the key
  number for the "poller starvation" hypothesis.
* Trace call sites cover:
  * `scheduler_run0` entry, CLAIM, CV_WAIT/CV_WOKE, DIRECT_HANDOFF,
    POLL_GATE, RELINQUISH;
  * `scheduler_make_ready` (with the MR_SIGNAL detail of which branch);
  * `scheduler_wait` and `scheduler_set_timeout`;
  * generic Unix `sys_poll_events` (POLL_ENTER/EXIT/REBUILD/SIGNAL_CONSUMED),
    `sys_signal` (SIGNAL), `sys_register/unregister_listener`
    (LISTENER_REG/UNREG/NOTIFY), `sys_register/unregister_select_event`
    (SEL_REG/UNREG);
  * `select_event_notify` (SEL_NOTIFY) and `select_event_send_notification`
    (SEL_MSG); enif_select stop (SEL_STOP).

This is the same set of points instrumented on the scratch branch's commit
`15b22259c`. The workflow rewrite from that scratch commit is intentionally
not picked up.

To enable: `cmake -DCMAKE_C_FLAGS=-DAVM_SCHED_TRACE ..`

### macOS native baselines (HEAD vs parent of 63ded8738)

Environment: macOS Darwin 25.5.0 arm64, 10 CPU cores, ~10 schedulers,
kqueue platform, Erlang/OTP 28. Valgrind is unavailable on Darwin arm64;
results below establish a control baseline, not a reproduction of the
Linux CI failures.

Builds:

* HEAD = `pr/2329` (commit `1d6b7b255`), trace overlay applied.
* Parent of `63ded8738` = `b6a906a1f`, trace overlay applied (worktree
  under `/var/folders/.../atomvm-parent`).

Driver: `tools/scheduler-baseline.sh` runs `AtomVM test_estdlib.avm` N
times, captures stdout/stderr per iteration, and reports run counts and
runtimes. The test_estdlib suite exercises `test_net_kernel`
(including `test_rpc_loop_from_beam` and `test_autoconnect_to_beam`),
`test_tcp_socket`, `test_serial_dist_socat`, etc.

Results (3 iterations each, native macOS, no Valgrind):

| Revision | iter 1 | iter 2 | iter 3 | passed | runtime min..max (ms) |
| --- | --- | --- | --- | --- | --- |
| HEAD (trace) | ok 94210ms | ok 93944ms | ok 102524ms | 3/3 | 93944..102524 |
| Parent (trace) | ok 93497ms | ok 91596ms | ok 92119ms | 3/3 | 91596..93497 |

No failures under native macOS. Trace overhead does not materially change
runtime. Local macOS native runs cannot reproduce the Linux Valgrind
intermittent failures described above; this matches the expected
environment dependency.

### Trace signature summary

Last ~30-60s slice of `test_estdlib.avm` execution per revision (the ring
holds the trailing 2^17 events; total runtime per iteration ~93-102s, so
each dump represents the tail of the run after the heaviest networking
tests have started):

| Metric | HEAD (`pr/2329`) | Parent (`b6a906a1f`) |
| --- | --- | --- |
| Ring fill duration captured | 56.6 s | 29.6 s |
| Events in dump | 131072 | 131072 |
| MAKE_READY | 14076 | 12401 |
| MR_SIGNAL (sys_signal sent) | 11697 | 7282 |
| SIGNAL | 17927 | 17918 |
| SIGNAL_CONSUMED | 8047 | 11045 |
| POLL_ENTER (blocking or timeout!=0) | 5056 | 3312 |
| POLL_EXIT | 8769 | 11722 |
| CV_WAIT / CV_WOKE | 4137 / 4146 | 10032 / 10041 |
| CLAIM / RELINQUISH | 4142 / 4138 | 10037 / 10033 |
| DIRECT_HANDOFF | 7376 | n/a (path does not exist) |
| LISTENER_NOTIFY | 1264 | 1017 |
| SEL_NOTIFY | 788 | 565 |
| SEL_MSG (kernel readiness -> process message) | 788 | 565 |
| FD ready -> SEL_MSG p99 latency | 1 us | 1 us |
| FD ready -> SEL_MSG max latency | 11 us | 15 us |
| DIRECT_HANDOFF streak p50/p99/max | 3/22/35 | n/a |
| POLL_ENTER inter-arrival p99/max | 134.6 ms / 4500.0 ms | 100.0 ms / 2000.1 ms |
| CV_WAIT duration p50/p99/max | 0.33 ms / 2001 ms / 5901 ms | n/a (not separately measured here) |
| Slow polls > 100ms with descriptors=0 | many @ 1999-2000ms and 4499ms | many @ 1999-2000ms |

Both revisions are healthy on macOS native. Observations:

1. The parent does ~2.4x more CV_WAIT/CLAIM/RELINQUISH cycles per unit of
   captured time. Direct handoff removes those round trips, which matches
   the commit's stated optimization goal.
2. The maximum direct-handoff streak in this workload is 35 (p50=3, p90=10,
   p99=22). On a fair-scheduler host (macOS arm64 with 10 CPUs), this is
   short enough that the poller scheduler still gets ample runtime.
3. FD-ready latency is sub-microsecond p50 and <20 us max on kqueue. There
   is no sign of FDs being lost. The trace reliably pairs a kernel-side
   readiness with a `select_event_send_notification`.
4. Large POLL_ENTER inter-arrival gaps (e.g. 4500 ms, 500 ms, 250 ms)
   correlate with timer-driven idle periods (e.g. `timer:sleep(500)` in
   `test_rpc_loop_from_beam`'s `PingFun`). They are designed gaps, not
   stalls.
5. Single-poller invariant is intact: at any time there is one CLAIM/RELINQUISH
   pair per scheduler thread, never overlapping in the trace per pair.

### Hypothesis assessment (post-baseline)

Drawing on these traces and the scheduler code in `src/libAtomVM/scheduler.c`
and `src/platforms/generic_unix/lib/sys.c`:

| Hypothesis | macOS native evidence | Linux Valgrind expectation |
| --- | --- | --- |
| Poller role stalls (no live owner) | refuted: CLAIM/RELINQUISH balanced per thread | unlikely; no code change makes the role lose its owner |
| Poller mutex starvation | mutex contention negligible (low CV_WAIT) | possible amplifier with serialized threads |
| Wake consumed before poll-set rebuild | REBUILD count balanced with SEL_REG/UNREG, no signs of missing rebuilds in trace | needs Linux poll(2) instrumentation to confirm |
| Event dispatch ok, process never selected | each SEL_MSG immediately produces MAKE_READY for the target pid; no orphans observed | unlikely without code change |
| Platform event bug (kqueue/poll loses event) | every POLL_EXIT descriptor accounted for by SIGNAL_CONSUMED + LISTENER/SEL_NOTIFY | low prior |
| Non-poller hogs CPU; poller starves (INVESTIGATION primary) | refuted on a fair host; CONSISTENT WITH expected Valgrind-amplification | best supported under Valgrind default --fair-sched=no |

The leading hypothesis from INVESTIGATION.md is consistent with the code
change in `63ded8738`. The DIRECT_HANDOFF path is the only behavioural
change that lets a non-poller thread perform an unbounded number of
process slices without ever blocking on the schedulers condvar.

Specifically: prior to `63ded8738`, every non-poller scheduler thread had
to `smp_condvar_wait` whenever it returned to `scheduler_run0`, which
forced a pthread blocking call on each scheduling event. Under Valgrind's
default `--fair-sched=no`, a pthread blocking call is precisely the
context-switch hint Valgrind needs to schedule another thread. After
`63ded8738`, a non-poller thread can stay continuously in DIRECT_HANDOFF
mode as long as the ready queue stays non-empty.

If the workload keeps the ready queue non-empty (timer-driven processes,
distribution heartbeats, gen_server keep-alives, intra-VM message
forwarding), this lets one OS-thread monopolize the Valgrind core until
the queue actually empties. The poller's eventfd write from `sys_signal`
is delivered but the poller's pthread does not get scheduled until the
busy thread blocks, so `poll(2)` never returns into AtomVM and FD-only
events (incoming distribution data, subprocess EOF, socket readiness)
accumulate. The 60-second timeouts seen in `test_rpc_loop_from_beam` and
the missing `DOWN` in `test_autoconnect_to_beam` are consistent with the
poller's host thread being held off until either the busy thread
naturally runs out of ready processes or a Valgrind preemption tick
fires.

This does NOT prove that all historical pre-`63ded8738` failures share
the same mechanism; the parent revision still goes through CV_WAIT on
every scheduling cycle, so a different stall (e.g. a missed wake or a
listener registration not invalidating the poll set) could still hit
under load. Confirming or eliminating those for the parent requires
trace data captured during an actual Linux Valgrind failure, which is
out of reach in this local environment.

### Open questions for the next session

* Reproduce on Linux Valgrind with the trace overlay enabled, capturing
  both passing and failing traces. The CI workflow change required is
  just `cmake -DCMAKE_C_FLAGS=-DAVM_SCHED_TRACE ..`; nothing else.
* If a failure is captured, confirm the DIRECT_HANDOFF streak grows
  large (much larger than 35) and that POLL_ENTER inter-arrival exceeds
  the test's 60s timeout for the failing FD.
* Measure under `--fair-sched=yes` as a diagnostic comparison. Per
  INVESTIGATION.md this is diagnostic only, not a fix.
* If the starvation pattern is confirmed, the smallest diagnostic patch
  is to bound `direct_handoff_streak` (e.g. to 16 or 64) and after the
  budget force the non-poller through a brief blocking call to let the
  poller's thread be scheduled (e.g. a 0-timeout `smp_condvar_wait` on
  the schedulers condvar, or an explicit `sys_signal` plus a short
  `smp_mutex_lock`/unlock dance). The principled fix would be to make
  the poller role transfer explicit (option 3 from the candidate fix
  shapes section).

### Diagnostic patch: AVM_SCHED_HANDOFF_BUDGET

A small, opt-in diagnostic was added to `scheduler_run0()` behind
`-DAVM_SCHED_HANDOFF_BUDGET=<N>`. When defined, a per-thread counter
caps the number of consecutive direct handoffs a single non-poller
scheduler thread can perform before it must go through `sys_signal()`
+ `smp_condvar_wait()`. The condvar wait is the host-side blocking call
Valgrind's default scheduler uses as a context-switch hint.

The macro is OFF by default. Setting it to a small value (e.g. 16)
preserves the optimisation introduced by `63ded8738` for the common
case (p50 streak = 3) while bounding the worst case. With AVM_SCHED_TRACE
also enabled, the per-thread max DIRECT_HANDOFF streak is observed to
cap exactly at the budget.

### Reproduction of a slowdown on macOS native (unexpected)

While iterating `test_estdlib.avm` repeatedly without Valgrind on macOS
arm64 (10-core M-series, kqueue platform, OTP 28), occasional iterations
take ~2-2.4x as long as the baseline. The first occurrence was a
suspected hang (`sample` on the AtomVM process showed all 10 schedulers
parked in `smp_condvar_wait` while one held the poller role and was
blocked in `kevent(2)`). Re-running with a per-iteration soft timeout
showed those iterations are not deadlocks: they complete in 165-225 s
rather than hanging forever.

Driver: `tools/scheduler-baseline-monitor.sh`, which sends SIGUSR1 to
dump the SCHED_TRACE ring if a run goes 30 s without output and is past
a soft deadline.

Aggregate results (each row is one full `test_estdlib.avm` run that
includes `test_net_kernel:test_rpc_loop_from_beam`,
`test_autoconnect_to_beam`, `test_tcp_socket`, `test_serial_dist_socat`,
etc.):

| Build | Runs | Slow runs (>150s) | Slow-run latencies (ms) | Slow rate |
| --- | --- | --- | --- | --- |
| HEAD default | 25 | 4 | 165822, 188854, 213991, 224948 | 16% |
| HEAD + AVM_SCHED_TRACE | 10 | 3 | 198555, 213324, 224311 | 30% |
| HEAD + AVM_SCHED_HANDOFF_BUDGET=16 (no trace) | 25 | 0 | - | 0% |

Fisher's exact test on `slow vs not` between HEAD-default and
budget=16: 4/25 vs 0/25, p ≈ 0.013. With the trace-enabled subset
included (7/35 vs 0/25), p ≈ 0.04. This is borderline, not
conclusive.

Trace evidence (`head-trace-x10/run_6.err`, an iteration that took 224
s and was captured under SCHED_TRACE):

* Event rate drops to 1-2k events/s in the slow stretches (vs ~10k
  events/s in a fast iteration).
* The low-rate stretches are dominated by legitimate timer waits:
  `SET_TIMEOUT pid <N>` followed by a matching `POLL_ENTER <N>` /
  `POLL_EXIT 0 <N>` pair. Each long wait is something the Erlang
  code explicitly requested (e.g. `timer:sleep(500)` /
  `timer:sleep(4500)`).
* Max DIRECT_HANDOFF streak: 62 (vs 35 in normal iterations).
* DIRECT_HANDOFF distribution: p50=4, p90=14, p99=38, max=62.
* POLL_ENTER inter-arrival p99: 249 ms (vs 134 ms in normal
  iterations); max 4.5 s (timer-driven).
* FD-ready -> SEL_MSG latency: p99=1 us, max=7 us. Kqueue did not
  lose any events.
* SIGNAL_CONSUMED count and LISTENER_NOTIFY/SEL_NOTIFY counts balance
  the POLL_EXIT descriptor totals exactly.

These numbers do not show a scheduler stall. They show the scheduler
correctly servicing a workload that asked for more timer-driven waits
in some iterations than in others. The slowdowns are most likely
driven by distribution-layer behaviour (`invalid_challenge` shutdowns,
`PingFun` retrying `net_adm:ping` for 200 ms each, and possibly a 60 s
`rpc:call` timeout in `test_rpc_loop_from_beam`), not by the scheduler.

The DIRECT_HANDOFF streak does grow during slow iterations
(62 vs 35), but correlation is not causation: nothing in the trace
shows a streak preventing an FD-readiness signal from being processed
or a make-ready from waking the poller. The budget patch's apparent
elimination of slow iterations could be a real but indirect benefit
(less scheduler-mediated message latency under load → fewer upstream
retries) or it could be insufficient sample size. The data do not
separate the two.

A minor pre-existing inefficiency observed in the trace: when the next
timer is less than 1 ms away, `update_timer_list` returns 0, which
makes the poller spin through several `POLL_GATE -1 0` iterations
(~16 iterations in 256 µs) until the timer actually fires. This is a
sub-millisecond busy-wait, not a multi-second stall.

### Updated hypothesis assessment

| Hypothesis | macOS native evidence | Linux Valgrind expectation |
| --- | --- | --- |
| Non-poller hogs CPU; poller starves (INVESTIGATION primary) | not supported: the slow stretches are dominated by legitimate timer waits, FD-ready latency stays at 1-7 us, and DIRECT_HANDOFF max stays well below any pathological size. AVM_SCHED_HANDOFF_BUDGET=16 happens to remove slow iterations in 25/25 vs 4/25, but the mechanism is unclear and the p-value is borderline. | still the leading candidate on Linux Valgrind specifically, because Valgrind's serial scheduler is the regime in which an uncapped DIRECT_HANDOFF can actually monopolise CPU. macOS fair scheduling cannot reproduce that regime. |
| Poller role stalls (no live owner) | refuted: each scheduler thread has balanced CLAIM/RELINQUISH counts | unlikely |
| Poller mutex starvation | mutex contention low on macOS | possible amplifier under Valgrind |
| Wake consumed before poll-set rebuild | REBUILD count matches SEL_REG/SEL_UNREG count; no missed rebuilds | needs Linux trace |
| Event dispatch ok, process never selected | every SEL_MSG immediately produces MAKE_READY for the target pid; no orphans observed | unlikely without separate code change |
| Platform event bug (kqueue/poll loses event) | every POLL_EXIT descriptor accounted for by SIGNAL_CONSUMED + LISTENER/SEL_NOTIFY | low prior |
| Distribution-layer flakiness (out of scheduler scope) | most consistent with the observed event-rate pattern and the `invalid_challenge` line in slow runs' stdout | independent issue, but could amplify or be amplified by scheduler latency |

I did not identify a specific lock or wake bug in the scheduler from
either inspection or the macOS traces. The starvation/amplification
hypothesis remains the most plausible Linux-Valgrind-specific
mechanism, but on a fair-scheduler host it is not what is producing
the observed slowdowns.

### Recommended next steps

1. Apply the existing trace overlay to a Linux CI job that already
   reproduces the timeouts (`-DCMAKE_C_FLAGS=-DAVM_SCHED_TRACE`).
   Confirm or refute that the DIRECT_HANDOFF streak grows large during
   the timeout window and that POLL_ENTER inter-arrival exceeds the
   test's per-RPC timeout. Save the failing run's `stderr` so the
   trace dumped via `atexit` or SIGUSR1 is captured.
2. Run the same Linux CI job with
   `-DCMAKE_C_FLAGS="-DAVM_SCHED_TRACE -DAVM_SCHED_HANDOFF_BUDGET=16"`
   to test whether the budget closes the failure window in the
   environment that exposes it.
3. Independently, run the Linux CI job under
   `valgrind --fair-sched=yes` as a diagnostic-only check. A pass rate
   that matches `--fair-sched=yes` would corroborate the starvation
   hypothesis; it is not an acceptable production fix.
4. Re-investigate the distribution-layer slowdowns
   (`invalid_challenge` shutdowns, PingFun pang retries,
   `test_rpc_loop_from_beam` 60 s timeouts) as a separate question
   from any scheduler change. INVESTIGATION.md already lists known
   distribution issues that should be fixed independently.
5. Only if 1+2 confirm the starvation mechanism under Valgrind, the
   principled production change is to make the poller role transfer
   explicit (option 3 from "Candidate Fix Shapes"). The handoff budget
   is a smaller transitional fix that does not change any invariant
   beyond "no single thread direct-handoffs more than N times in a row
   before parking on the condvar"; without a Linux-side confirmation,
   it is best treated as a diagnostic knob, not a production fix.

### Files touched (working tree)

* `src/libAtomVM/sched_trace.h` (new) — trace API gated by
  `AVM_SCHED_TRACE`.
* `src/libAtomVM/sched_trace.c` (new) — lock-free ring, atexit /
  SIGUSR1 / explicit dump, env-controlled output file.
* `src/libAtomVM/CMakeLists.txt` — add the new translation unit/header.
* `src/libAtomVM/scheduler.c` — trace call sites plus the optional
  `AVM_SCHED_HANDOFF_BUDGET` diagnostic in `scheduler_run0`.
* `src/libAtomVM/resources.c` — trace call sites in `enif_select`
  stop path, `select_event_send_notification`, and
  `select_event_notify`.
* `src/platforms/generic_unix/lib/sys.c` — trace call sites in the
  kqueue and poll `sys_poll_events` paths, `sys_signal`,
  listener/select event register/unregister.
* `tools/scheduler-baseline.sh` (new) — simple repeated run driver.
* `tools/scheduler-baseline-monitor.sh` (new) — driver with soft hang
  detection that sends SIGUSR1 before SIGTERM.

All scheduler-side changes are no-ops when neither
`AVM_SCHED_TRACE` nor `AVM_SCHED_HANDOFF_BUDGET` is defined.
The default build still compiles successfully and passes the
existing tests (`build-default` directory in this workspace).

