# Implementation Plan — SMP-safe persistent_term reclamation

## Goal

Make retired `persistent_term` entries eventually reclaimable in SMP builds, without
introducing a stop-the-world scheduler pause and without changing the semantics of
`persistent_term:get/1` (still returns by reference, no copy).

The current branch (`codex/add-persistentterm-support`) adds a reclamation pass that
scans every process's heap/stack/registers from any scheduler thread. That is unsound
under SMP (other schedulers can mutate the scanned state, and references can briefly
live only in C locals between a BIF call and its register store). It also misses some
roots (list pointers, exception fields) and a few other bookkeeping issues.

This plan replaces that with a **per-process safe-point checkpoint** scheme using
monotonic epochs.

---

## Design overview

### Core idea

- Each retired entry gets a monotonic `retire_epoch`.
- Each `Context` carries a `persistent_term_checked_epoch`.
- A process is scanned **only from its own scheduler thread, at a safe point** where
  the scheduler owns it (selected to run / about to yield / about to wait). It is
  never scanned by another scheduler thread.
- The global reclaim pass walks the process table reading only per-process epochs
  (never heaps/stacks/regs of other processes) and frees an entry when
  `entry->retire_epoch <= ctx->persistent_term_checked_epoch` for every live process.

### Why this is SMP-safe

- Checkpoint runs when the scheduler owns the process → no concurrent mutation of its
  heap, stack, registers, mailbox-inner, or dictionary.
- A BIF that briefly holds a persistent value in a C local (e.g. `persistent_term:get/1`
  returning `entry->value` into a local before the emulator/JIT stores it into `x[0]`)
  is safe: the retire happens with an epoch strictly greater than the process's last
  `checked_epoch`, so reclaim is blocked until the process next reaches a checkpoint
  — by which point the return value has been written into `x[0]` (a scanned root).
- New processes inherit the current `reclaim_epoch` on creation. They cannot reach
  older retired entries: `persistent_term:get` on a current key won't yield an old
  pointer, and any inherited references via spawn/send go through
  `memory_copy_term_tree` (deep copy).
- Dying processes are covered by a small teardown guard during `context_destroy`,
  because the process is removed from `processes_table` before all term-using
  cleanup completes.

### Conservative prefix epoch

If a process still references an entry retired at epoch 5, its `checked_epoch` is
pinned at 4. Later unrelated retirements (epoch 6, 7…) are temporarily blocked from
reclamation for that process. This keeps the implementation small (no per-entry/per-
process bitsets) at the cost of occasionally deferring unrelated reclamations until
the held reference is dropped.

---

## Data-structure changes

### `src/libAtomVM/persistent_term.c` — `struct PersistentTermEntry`

```c
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
```

### `src/libAtomVM/persistent_term.h` — `PersistentTerm`

Add `reclaim_epoch` next to existing fields:

```c
typedef struct PersistentTerm
{
    size_t count;
    size_t memory;
    struct PersistentTermEntry *buckets[PERSISTENT_TERM_NUM_BUCKETS];
    struct PersistentTermEntry *retired_entries;
#ifndef AVM_NO_SMP
    uint64_t reclaim_epoch;
    SpinLock lock;
#endif
} PersistentTerm;
```

Add new public declarations:

```c
void persistent_term_process_checkpoint(struct Context *ctx);
void persistent_term_init_process_checkpoint(struct Context *ctx);
void persistent_term_reclaim(PersistentTerm *persistent_term, struct GlobalContext *global);
```

### `src/libAtomVM/context.h` — `Context`

Add (outside the JIT fixed-offset region):

```c
#ifndef AVM_NO_SMP
    uint64_t persistent_term_checked_epoch;
#endif
```

### `src/libAtomVM/globalcontext.h` — `GlobalContext`

Keep the existing `persistent_term_reclaim_pending` flag (initialize it!) and add a
teardown guard:

```c
bool ATOMIC persistent_term_reclaim_pending;
#ifndef AVM_NO_SMP
    unsigned int ATOMIC persistent_term_reclaim_teardown_guard;
#endif
```

---

## Initialization

### `persistent_term_init` (persistent_term.c)

```c
#ifndef AVM_NO_SMP
    persistent_term->reclaim_epoch = 0;
#endif
```

### `globalcontext_new` (globalcontext.c)

```c
glb->persistent_term_reclaim_pending = false;
#ifndef AVM_NO_SMP
glb->persistent_term_reclaim_teardown_guard = 0;
#endif
```

### Process creation

In `globalcontext_init_process` (or just before `synclist_append(&glb->processes_table, ...)`)
call `persistent_term_init_process_checkpoint(ctx)`, which snapshots the current
`reclaim_epoch` under the persistent-term lock.

---

## Retiring an entry

Update `retire_entry()` to stamp an epoch:

```c
static void retire_entry(PersistentTerm *persistent_term, struct PersistentTermEntry *entry)
{
#ifndef AVM_NO_SMP
    entry->retire_epoch = ++persistent_term->reclaim_epoch;
#endif
    entry->next = persistent_term->retired_entries;
    persistent_term->retired_entries = entry;
}
```

`persistent_term_put` (replace path) and `persistent_term_erase` continue to:

```c
#ifndef AVM_NO_SMP
    global->persistent_term_reclaim_pending = true;
    sys_signal(global);
#else
    persistent_term_reclaim(persistent_term, global);
#endif
```

Important: do **not** clear `persistent_term_reclaim_pending` just because a reclaim
pass ran. Clear it only when `retired_entries == NULL` (otherwise checkpoints stop).

---

## Per-process checkpoint

New function, called only when the current scheduler owns `ctx`:

```c
void persistent_term_process_checkpoint(Context *ctx)
{
#ifndef AVM_NO_SMP
    GlobalContext *global = ctx->global;
    if (!global->persistent_term_reclaim_pending) {
        return;
    }
    PersistentTerm *pt = &global->persistent_term;

    SMP_WRLOCK(pt);
    if (pt->retired_entries == NULL
        || ctx->persistent_term_checked_epoch >= pt->reclaim_epoch) {
        SMP_UNLOCK(pt);
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
            // Conservative prefix: cannot advance past this entry.
            if (entry->retire_epoch - 1 < new_checked) {
                new_checked = entry->retire_epoch - 1;
            }
        }
    }

    bool advanced = new_checked > old_checked;
    if (advanced) {
        ctx->persistent_term_checked_epoch = new_checked;
    }
    SMP_UNLOCK(pt);

    if (advanced) {
        persistent_term_reclaim(pt, global);
    }
#else
    UNUSED(ctx);
#endif
}
```

---

## Reclaim pass (SMP)

The SMP reclaim path must not scan other processes' heaps. It reads only their
`persistent_term_checked_epoch`:

```c
void persistent_term_reclaim(PersistentTerm *persistent_term, GlobalContext *global)
{
#ifndef AVM_NO_SMP
    if (!global->persistent_term_reclaim_pending) {
        return;
    }
    if (global->persistent_term_reclaim_teardown_guard != 0) {
        return;
    }

    SMP_WRLOCK(persistent_term);
    if (persistent_term->retired_entries == NULL) {
        global->persistent_term_reclaim_pending = false;
        SMP_UNLOCK(persistent_term);
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

    global->persistent_term_reclaim_pending =
        persistent_term->retired_entries != NULL;
    SMP_UNLOCK(persistent_term);
#else
    /* Non-SMP keeps the direct in-place scan/free. */
    ...
#endif
}
```

---

## Scheduler hooks

In `src/libAtomVM/scheduler.c`:

1. **Before resuming a selected process** — at the end of `scheduler_run0`, after
   locks are released and before returning `result`:

   ```c
   if (result != NULL) {
       persistent_term_process_checkpoint(result);
   }
   ```

2. **When the current process yields** — in `scheduler_next(global, c)`, before
   acquiring `processes_spinlock`:

   ```c
   persistent_term_process_checkpoint(c);
   ```

3. **When the current process waits** — at the top of `scheduler_wait(ctx)`, before
   the flag/list changes:

   ```c
   persistent_term_process_checkpoint(ctx);
   ```

4. **Native handler path** — in `scheduler_run` after the native handler has been
   invoked and signals processed, before clearing `Running` or destroying the
   context, while the scheduler still owns it.

The "every scheduler tick reclaim under `schedulers_mutex`" trigger in the current
diff can be removed — checkpoints already call `persistent_term_reclaim` when they
advance an epoch.

---

## Root scanner

Tighten `context_has_reference_to_heap` (or replace its helper) to be correct and
conservative:

- Replace `term_is_boxed_pointer_into_heap` with a generic
  `term_is_pointer_into_heap` that also checks `term_is_nonempty_list` →
  `term_get_list_ptr` (current code misses list pointers entirely).
- Scan only **used** heap regions: `heap_start .. heap_ptr` (not `heap_end`). Older
  fragments scanned as `storage .. heap_end` (their used boundary).
- Stack scanned as `[ctx->e, context_stack_base(ctx))`.
- Add missing roots: `ctx->bs`, `ctx->exception_reason`, `ctx->exception_stacktrace`.
- Keep existing roots: `x[]`, extended x regs, `cp`, dictionary, `exit_reason`,
  `group_leader`, mailbox `inner_first` `NormalMessage` storage.
- Do **not** scan the mailbox outer list (it can be mutated by other schedulers
  without taking the mailbox's send lock here).

---

## Process teardown

In `context_destroy`:

```c
#ifndef AVM_NO_SMP
ctx->global->persistent_term_reclaim_teardown_guard++;
#endif

/* existing destruction, including removal from processes_table
 * and all monitor/distribution/exit_reason cleanup that may
 * read persistent_term-derived terms */

#ifndef AVM_NO_SMP
ctx->global->persistent_term_reclaim_teardown_guard--;
if (ctx->global->persistent_term_reclaim_pending) {
    persistent_term_reclaim(&ctx->global->persistent_term, ctx->global);
}
#endif
```

Place the decrement after the last code that may touch terms belonging to the
dying process, before `free(ctx)`.

---

## Other fixes from the review (independent of SMP design)

These are required regardless of the SMP approach:

- Initialize `glb->persistent_term_reclaim_pending = false` in `globalcontext_new`.
- Take the persistent-term lock **before** the `if (retired_entries == NULL) return;`
  fast path in `persistent_term_reclaim`. (Current code races.)
- Stale comment in `test_info_and_get_all/0` says retired entries are kept until
  shutdown — update it.
- `cleanup/0` in the test should also erase `{?MODULE, reclaim}` and any new test
  keys.

---

## Testing

Replace the timing-dependent `receive after 10` test with bounded polling and add
tests that actually exercise the safety property.

Add to `tests/erlang_tests/test_persistent_term.erl`:

```erlang
wait_until(Fun) -> wait_until(Fun, 200).
wait_until(_Fun, 0) -> timeout;
wait_until(Fun, N) ->
    case Fun() of
        true -> ok;
        false -> receive after 10 -> ok end, wait_until(Fun, N - 1)
    end.
```

Tests:

1. **Reclaim of a real heap-allocated value** (replaces current `some_value` atom test):
   ```erlang
   ok = persistent_term:put(Key, lists:seq(1, 100)),
   #{memory := M1} = persistent_term:info(),
   true = persistent_term:erase(Key),
   ok = wait_until(fun() ->
       #{memory := M2} = persistent_term:info(),
       M2 < M1
   end).
   ```

2. **Live reference prevents reclaim**:
   ```erlang
   Value = lists:seq(1, 100),
   ok = persistent_term:put(Key, Value),
   #{memory := M1} = persistent_term:info(),
   Retained = persistent_term:get(Key),
   true = persistent_term:erase(Key),
   %% Give schedulers time to attempt reclaim.
   _ = wait_until(fun() -> false end),  %% just delay
   #{memory := M2} = persistent_term:info(),
   true = M2 >= M1,
   Value = Retained.
   ```

3. **Eventual reclaim after holder dies**:
   ```erlang
   Parent = self(),
   Pid = spawn(fun() ->
       _ = persistent_term:get(Key),
       Parent ! ready,
       receive release -> ok end
   end),
   receive ready -> ok end,
   true = persistent_term:erase(Key),
   Pid ! release,
   ok = wait_until(fun() ->
       #{memory := M} = persistent_term:info(),
       M < M_before
   end).
   ```

Run on both SMP and non-SMP builds.

---

## Estimated work

| Area                                | LOC      |
|-------------------------------------|----------|
| struct fields, init, glob init      | ~25      |
| retire epoch stamping               | ~10      |
| `persistent_term_process_checkpoint`| ~60–90   |
| Reclaim rewrite (SMP epoch-only)    | ~50–70   |
| Scheduler hooks (4 sites)           | ~15–30   |
| Context teardown guard              | ~15–25   |
| Root scanner fixes                  | ~20–40   |
| Tests                               | ~60      |
| **Total**                           | ~250–350 |

Risk: **moderate**. The epoch protocol itself is small; the implementation hazard
is making `context_has_reference_to_heap` cover every place a persistent reference
can live in a `Context` while never scanning state that is concurrently mutated by
another scheduler thread.

---

## Implementation order

1. Independent fixes (init the pending flag, lock-before-precheck, scanner fixes:
   list pointers + `bs`/`exception_reason`/`exception_stacktrace` + `heap_ptr` bound).
2. Add epoch fields and initialize them (`PersistentTerm.reclaim_epoch`,
   `PersistentTermEntry.retire_epoch`, `Context.persistent_term_checked_epoch`,
   `GlobalContext.persistent_term_reclaim_teardown_guard`).
3. Stamp `retire_epoch` in `retire_entry`.
4. Implement `persistent_term_process_checkpoint`.
5. Rewrite SMP `persistent_term_reclaim` to read only epochs.
6. Add scheduler hooks; remove the existing
   `schedulers_mutex`-held reclaim call in `scheduler_run0`.
7. Add `context_destroy` teardown guard.
8. Update test suite.
9. Verify with `tests/erlang_tests/test_persistent_term`, including under SMP
   (default) and `AVM_NO_SMP` builds, and run valgrind on the unix build.

---

## Out of scope

- Stop-the-world / scheduler-park protocol.
- Per-entry/per-process verification bitsets (defer if conservative-prefix epoch
  proves too slow in heavy retire workloads).
- Hooking checkpoints into GC (avoided here because
  `persistent_term_get_all_maybe_gc` already holds the persistent-term lock during
  GC and reversing that lock order is awkward).
- Native-handler private term storage (`platform_data` etc.). If a native handler
  stashes raw terms aliasing a persistent heap outside `Context` roots, it must
  expose its own root-scan or copy the term — document this as a contract.
