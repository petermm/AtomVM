# Emscripten NoSMP Specification

## Purpose

`emscripten_nosmp` is a browser-first AtomVM platform variant for WebAssembly builds that do not use pthreads.

The goal is to provide an Emscripten target that:

- builds without `-pthread`
- does not use `-sPROXY_TO_PTHREAD`
- does not require `SharedArrayBuffer`
- does not require cross-origin isolation headers (`COOP` / `COEP`) just to start AtomVM in the browser
- preserves the existing Erlang-facing `emscripten` and `websocket` APIs

This document explains the resulting architecture, the implementation choices, and the code changes made to support the new platform.

## Summary Of The Result

The repository now contains two browser-oriented Emscripten platforms:

- `emscripten`
  - threaded build
  - uses pthreads
  - uses `-sPROXY_TO_PTHREAD`
  - requires cross-origin isolation for normal browser use
- `emscripten_nosmp`
  - non-pthread build
  - web-only
  - does not use `-sPROXY_TO_PTHREAD`
  - does not require cross-origin isolation headers for same-origin hosting

The public platform identity is also split:

- `atomvm:platform()` returns `emscripten` on the threaded platform
- `atomvm:platform()` returns `emscripten_nosmp` on the nosmp platform

The Erlang modules and message protocol are intentionally unchanged:

- the Erlang modules are still `emscripten` and `websocket`
- JS `call` / `cast` still target the `emscripten` mailbox protocol
- callback messages are still tagged with `emscripten`

## Important Architectural Note

The shipped implementation differs in one important way from the original design sketch.

The original plan considered a worker-based nosmp build using `--proxy-to-worker`. The implementation that landed does **not** use `--proxy-to-worker`. Instead, `emscripten_nosmp` is implemented as a single-threaded browser runtime that runs on the web main thread and advances the AtomVM scheduler through a non-blocking async pump.

That means:

- no pthread infrastructure is needed
- no thread proxy APIs are needed for main-thread-only browser APIs
- the runtime integrates with the browser event loop directly
- the platform is currently web-only, not Node-capable

This simplification is the main reason the final implementation is smaller and more reliable than a half-threaded browser bridge.

## Platform Layout

### New Platform Root

The new platform lives under:

- `src/platforms/emscripten_nosmp`

It mirrors the existing Emscripten platform layout but reuses most of the shared implementation from:

- `src/platforms/emscripten/src/lib`

### Shared Source Reuse

The nosmp platform does not fork the full Emscripten NIF and support layer. Instead, it compiles the existing shared Emscripten sources with a new compile definition:

- `AVM_EMSCRIPTEN_NOSMP`

This is wired in:

- `src/platforms/emscripten_nosmp/src/lib/CMakeLists.txt`

The nosmp platform library reuses:

- `platform_defaultatoms.c`
- `platform_nifs.c`
- `sys.c`
- `websocket_nifs.c`

and changes behavior through `#ifdef AVM_EMSCRIPTEN_NOSMP`.

## Build And Packaging Changes

### New CMake Entry Points

New nosmp build entry points were added:

- `src/platforms/emscripten_nosmp/CMakeLists.txt`
- `src/platforms/emscripten_nosmp/src/CMakeLists.txt`
- `src/platforms/emscripten_nosmp/src/lib/CMakeLists.txt`
- `src/platforms/emscripten_nosmp/tests/src/CMakeLists.txt`

### New Erlang Library Packaging

New library packaging was added for the nosmp platform:

- `libs/avm_emscripten_nosmp/src/CMakeLists.txt`

This produces:

- `avm_emscripten_nosmp.avm`
- `atomvmlib-emscripten_nosmp.avm`

and corresponding install layout under:

- `lib/atomvm/lib/avm_emscripten_nosmp-<version>/...`

### Global Packaging And PLT Wiring

The new package was added to:

- `libs/CMakeLists.txt`

This includes:

- pack assembly for `atomvmlib-emscripten_nosmp`
- Dialyzer beam list generation
- Dialyzer PLT generation for `atomvmlib-emscripten_nosmp`
- install rules for the new archive

`CMakeModules/BuildErlang.cmake` did not need a new macro. The existing archive packaging macros were reused.

## Build Flags

### Threaded Emscripten

The existing threaded platform still uses:

- `-pthread`
- `-sPROXY_TO_PTHREAD`
- `-sENVIRONMENT=web,worker`

from:

- `src/platforms/emscripten/src/CMakeLists.txt`

### NoSMP Emscripten

The nosmp platform uses:

- `-sFETCH`
- `-sFORCE_FILESYSTEM`
- `-sENVIRONMENT=web`
- `-lwebsocket.js`
- `--pre-js src/platforms/emscripten_nosmp/src/atomvm.pre.js`

from:

- `src/platforms/emscripten_nosmp/src/CMakeLists.txt`

and intentionally does **not** use:

- `-pthread`
- `-sPROXY_TO_PTHREAD`

The nosmp build currently rejects non-web environments:

- `emscripten_nosmp currently only supports AVM_EMSCRIPTEN_ENV=web`

## Public Interface Changes

### New Platform Atom

The nosmp platform atom was added in:

- `src/platforms/emscripten/src/lib/platform_defaultatoms.def`

New atom:

- `emscripten_nosmp`

### atomvm:platform/0

`atomvm:platform/0` now returns:

- `emscripten_nosmp`

on the nosmp platform. The type was updated in:

- `libs/eavmlib/src/atomvm.erl`

The platform NIF implementation was updated in:

- `src/platforms/emscripten/src/lib/platform_nifs.c`

Internally:

- threaded build maps `atomvm:platform()` to `EMSCRIPTEN_ATOM`
- nosmp build maps it to `EMSCRIPTEN_NOSMP_PLATFORM_ATOM`

### Unchanged Erlang API Surface

The following remain intentionally unchanged:

- the module name `emscripten`
- the module name `websocket`
- `{emscripten, ...}` mailbox messages
- browser-side `Module.call(...)`
- browser-side `Module.cast(...)`

This keeps user code largely source-compatible while still exposing the new platform identity when needed.

## Runtime Architecture

### Thread Model

`emscripten_nosmp` is single-threaded.

AtomVM, browser callbacks, JS interop, promise resolution, HTML5 event registration, and websocket callbacks all run in the browser web environment without pthread handoff.

This is the core architectural difference from the threaded `emscripten` build, where AtomVM runs on a proxied pthread runtime and main-thread-only browser APIs must be dispatched back to the browser main thread.

### Startup Model

Startup now happens in three stages:

1. `atomvm.pre.js` runs during Emscripten `preRun`
2. all module arguments are fetched and preloaded into the Emscripten filesystem
3. `main()` initializes AtomVM and schedules the first scheduler pump

### Why Preload Exists

Without worker/pthread machinery, the browser build still needs a way to make startup `.beam` and `.avm` files visible through normal file APIs. The nosmp pre-js shim does this by:

- reading `Module.arguments`
- `fetch()`ing each path
- creating parent directories in `FS`
- writing the file contents into the virtual filesystem
- releasing a run dependency once preload is complete

This logic lives in:

- `src/platforms/emscripten_nosmp/src/atomvm.pre.js`

The result is that runtime code can still open startup files using the same path-based mechanisms as the other platforms.

### Scheduler Pump

The threaded build can block in `sys_poll_events()` on a condition variable. The nosmp build cannot do that without freezing the browser event loop, so it uses an async scheduler pump.

The pump lives in:

- `src/platforms/emscripten_nosmp/src/main.c`

Key state:

- `pump_scheduled`
- `scheduled_pump_delay_ms`
- `pump_token`
- `entry_started`

Key function:

- `emscripten_nosmp_schedule_pump(int timeout_ms)`

Behavior:

- schedule `atomvm_pump` with `emscripten_async_call`
- coalesce redundant wakeups
- avoid replacing an already earlier wakeup with a later one
- invalidate stale callbacks with `pump_token`

`atomvm_pump()` then:

- starts the entry function the first time
- later runs `scheduler_entry_point(global)`
- exits cleanly if `scheduler_stop_all` is set
- asks the scheduler for the next idle delay via `scheduler_get_idle_wait_timeout_ms(global)`
- reschedules itself if the scheduler wants another wakeup

### Practical Meaning

In the threaded platform, AtomVM can sleep in a blocking wait for browser-originated messages.

In the nosmp platform, AtomVM never blocks the browser thread. Instead:

- browser-originated work enqueues messages and requests an immediate pump
- idle time is represented as a delayed `emscripten_async_call`

This is the key runtime adaptation that makes nosmp viable in a browser without pthreads.

### Budgeted / Time-Sliced Pump

The current nosmp pump yields when the runtime becomes idle, but it does not yet enforce a hard upper bound on how long AtomVM may continue running while work remains runnable.

That means the current behavior is:

- fair between Erlang processes
- cooperative with the browser only once AtomVM goes idle
- vulnerable to UI jank if Erlang code remains continuously runnable on the browser main thread

A natural next step for this architecture is a budgeted or time-sliced pump.

The idea is:

- keep the single-threaded nosmp design
- continue using the existing async pump model
- stop a pump iteration after a bounded amount of VM work
- reschedule immediately with `emscripten_async_call(..., 0)` so the browser can process input, paint, and other events between VM slices

AtomVM already tracks reductions for emulated processes, so one implementation path is:

- run until a total reduction budget is consumed
- save scheduler state
- return a `yield_soon` style result to the nosmp pump
- schedule the next slice on the next browser tick

This is similar to how many browser-hosted VMs preserve responsiveness while still running substantial amounts of bytecode.

In practice, the best form of this design would likely be hybrid:

- a reduction budget for normal BEAM execution
- optionally a small wall-clock budget as a second guardrail

The wall-clock guard is important because not all expensive operations are well represented by reductions alone. Long NIFs or browser-facing calls may still monopolize the main thread even if the BEAM reduction count looks modest.

So the intended direction is not to replace the nosmp async pump, but to refine it:

- current nosmp behavior: yield on idle
- possible future nosmp behavior: yield on idle or after a bounded busy slice

This would preserve the simplicity of the current single-threaded architecture while improving page responsiveness under sustained CPU-bound Erlang workloads.

### Message Queue Integration

The shared platform queue in `sys.c` is reused in both variants.

In the threaded build:

- messages are enqueued under `pthread_mutex`
- `pthread_cond_signal` wakes the runtime thread

In the nosmp build:

- messages are appended directly to the shared platform list
- `emscripten_nosmp_schedule_pump(0)` wakes the scheduler

This behavior is implemented in:

- `src/platforms/emscripten/src/lib/sys.c`
- `src/platforms/emscripten/src/lib/emscripten_sys.h`

The same queue carries:

- JS `cast`
- JS `call`
- HTML5 callback events
- HTML5 callback unregistration requests
- internal signal messages

## Browser API Bridge Changes

### Main-Thread-Only Operations

The threaded platform uses Emscripten thread-dispatch helpers for operations that must run on the browser main thread:

- `emscripten_dispatch_to_thread`
- `emscripten_set_*_callback_on_thread`

The nosmp platform does not need those APIs because it already runs in the web environment.

The shared NIF layer was changed so that under `AVM_EMSCRIPTEN_NOSMP` it uses:

- direct `emscripten_set_*_callback(...)`
- direct `emscripten_run_script(...)`
- direct promise resolve/reject helpers
- `emscripten_async_call(...)` when async main-thread semantics are still needed

These changes are primarily in:

- `src/platforms/emscripten/src/lib/platform_nifs.c`
- `src/platforms/emscripten/src/lib/sys.c`

### emscripten:run_script/1,2

Behavior is preserved, but implementation changes by platform.

Threaded build:

- synchronous `main_thread` mode traps the caller
- work is dispatched to the browser main thread
- the caller is released when the work completes

NoSMP build:

- `main_thread` synchronous mode executes inline because AtomVM is already on the browser thread
- `main_thread + async` uses `emscripten_async_call`
- non-`main_thread` mode is also direct

This keeps the API stable while removing unnecessary thread proxying in nosmp.

### Promise Resolution

Threaded build:

- promise resolve/reject is dispatched to the main runtime thread

NoSMP build:

- promise resolve/reject can happen directly

This affects:

- JS `Module.call(...)`
- `emscripten:promise_resolve/*`
- `emscripten:promise_reject/*`

The nosmp promise/resource destructors also resolve pending promises directly to avoid stranded JS promises.

### HTML5 Event Registration

HTML5 callback registration and unregistration now differ by platform.

Threaded build:

- register and unregister on the browser main thread with `_on_thread` APIs

NoSMP build:

- register and unregister directly with normal Emscripten HTML5 callback APIs

Unregistration requests still go through the platform message queue so resource lifetime stays safe and callback data is not freed before the handler is actually removed.

## WebSocket Architecture

### Shared Design

The websocket NIF implementation remains shared between both platforms in:

- `src/platforms/emscripten/src/lib/websocket_nifs.c`

The browser websocket object is still created with:

- `createOnMainThread = true`

That keeps behavior aligned with browser websocket APIs regardless of platform flavor.

### NoSMP Scheduler Wakeups

A nosmp-specific helper was added so websocket callbacks wake the scheduler pump after delivering messages into AtomVM:

- open
- message
- error
- close

Without this, websocket events could arrive while the scheduler was idle and remain unprocessed until some unrelated wakeup happened.

### NoSMP Close Semantics

The nosmp websocket path also needed explicit close-state handling to match observable behavior expected by the existing Erlang tests.

Additional nosmp state was added to the websocket resource:

- `close_event_delivered`
- `close_poll_active`
- `close_code`
- `close_reason`

Key behaviors:

- local websocket close records pending close details
- a short async poll can synthesize a close event if the browser has not delivered one yet
- `ready_state` reports `closed` once the close event is considered delivered
- duplicate close delivery is avoided if a real close callback arrives after the synthetic path

This keeps `websocket` behavior usable in the non-pthread event model and was needed to make the nosmp websocket browser tests reliable.

### Other Runtime Fixes Incorporated During Bring-Up

While getting the nosmp browser path green, a few correctness fixes were folded into the shared implementation:

- the websocket error tuple was corrected to match its actual allocated arity
- websocket callbacks explicitly wake the nosmp scheduler
- final DOM updates in the websocket browser test were switched to synchronous `main_thread` execution so Cypress observes the result reliably

These are all part of stabilizing the nosmp browser runtime, especially around end-of-test timing.

## File Serving And Browser Test Harness

The toy browser webserver in:

- `examples/emscripten/wasm_webserver.erl`

was updated so it can serve both variants correctly.

Notable changes:

- new `/nosmp/...` route for nosmp wasm assets
- nosmp test pages now reference `/nosmp/AtomVM.js`
- cross-origin isolation headers are only added for the threaded browser build
- nosmp requests skip those headers
- path checks were rewritten to avoid unavailable `string:str/2` and `lists:prefix/2` calls in CI environments

This lets the same lightweight AtomVM webserver host:

- the original threaded Emscripten tests
- the new nosmp browser tests

## Browser Headers And Hosting Model

The nosmp platform is specifically designed so that it does **not** need:

- `Cross-Origin-Opener-Policy: same-origin`
- `Cross-Origin-Embedder-Policy: require-corp`
- `SharedArrayBuffer`

for ordinary same-origin browser hosting.

This is the major deployment advantage over the threaded browser target.

However, standard browser fetch rules still apply:

- same-origin hosting works normally
- cross-origin asset hosting may still require ordinary CORS

So the accurate statement is:

- `emscripten_nosmp` removes the cross-origin isolation requirement
- it does not remove normal browser origin policy rules

## Tests And CI

### New Browser Test Tree

Nosmp browser tests live under:

- `src/platforms/emscripten_nosmp/tests`

This includes:

- its own test HTML entrypoints
- its own Cypress config
- its own test build target

The test BEAM modules are compiled into:

- `src/platforms/emscripten_nosmp/build/tests/src`

### CI Coverage

The wasm CI workflow now includes nosmp coverage in:

- `.github/workflows/wasm-build.yaml`

Added coverage includes:

- build triggers when nosmp sources change
- `atomvmlib-emscripten_nosmp` packaging in the compile phase
- compilation and upload of nosmp test modules
- `wasm_build_web_nosmp`
- `wasm_test_web_nosmp`

The nosmp Cypress lane now exercises:

- `atomvm:platform()`
- JS `call` / `cast`
- HTML5 event registration and delivery
- websocket behavior

## Compatibility Notes

### What Stays The Same

- Erlang code can still use the `emscripten` module
- Erlang code can still use the `websocket` module
- JS still calls `Module.call(...)` and `Module.cast(...)`
- the main browser integration model remains "serve AtomVM.js, AtomVM.wasm, and beam/avm files"

### What Changes

- platform name can now be `emscripten_nosmp`
- Node is not currently supported for nosmp
- nosmp is single-threaded
- nosmp uses async scheduler pumping rather than blocking waits

### Capability Differences

Tests that already special-cased Emscripten behavior were widened to include nosmp as another Wasm-style environment. For example:

- `tests/libs/eavmlib/test_file.erl`

now excludes both `emscripten` and `emscripten_nosmp` from `select` and `execve` expectations.

## Files Added Or Significantly Changed

### New files and directories

- `src/platforms/emscripten_nosmp/**`
- `libs/avm_emscripten_nosmp/src/CMakeLists.txt`

### Shared runtime files updated for dual-mode behavior

- `src/platforms/emscripten/src/lib/emscripten_sys.h`
- `src/platforms/emscripten/src/lib/sys.c`
- `src/platforms/emscripten/src/lib/platform_nifs.c`
- `src/platforms/emscripten/src/lib/websocket_nifs.c`
- `src/platforms/emscripten/src/lib/platform_defaultatoms.def`

### Packaging and type updates

- `libs/CMakeLists.txt`
- `libs/eavmlib/src/atomvm.erl`

### Test and harness updates

- `examples/emscripten/wasm_webserver.erl`
- `src/platforms/emscripten_nosmp/tests/src/*.html`
- `.github/workflows/wasm-build.yaml`
- `tests/libs/eavmlib/test_file.erl`

## Current Scope And Limitations

- `emscripten_nosmp` is currently `AVM_EMSCRIPTEN_ENV=web` only
- the implementation is single-threaded by design
- the platform avoids pthread browser requirements rather than emulating the pthread architecture without threads
- normal browser origin rules still apply even though cross-origin isolation is no longer required

## Bottom Line

`emscripten_nosmp` is not just "the old Emscripten platform with a couple of flags removed".

It is a separate browser runtime strategy:

- same Erlang-facing APIs
- different platform identity
- different scheduler integration model
- no pthreads
- no `SharedArrayBuffer` dependency
- no `COOP` / `COEP` requirement for same-origin browser hosting

The main implementation technique is to reuse the existing Emscripten platform support code, selectively switch behavior with `AVM_EMSCRIPTEN_NOSMP`, and replace blocking/threaded coordination with direct browser execution plus an async scheduler pump.
