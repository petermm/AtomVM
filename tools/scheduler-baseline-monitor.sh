#!/bin/bash
# scheduler-baseline-monitor.sh — like scheduler-baseline.sh but with a
# per-iteration soft timeout. If a run does not finish in `MAX_S` seconds,
# we send SIGUSR1 (in case AVM_SCHED_TRACE was compiled in, this dumps the
# trace), wait 2 s, then send SIGTERM, then SIGKILL.
#
# Usage:
#   scheduler-baseline-monitor.sh <atomvm> <test_avm> <iterations> <log_dir> [max_s] [hang_s]
# Defaults: max_s=180, hang_s=160 (send SIGUSR1 after hang_s seconds w/o output)

set -u

BIN="${1:?atomvm binary}"
AVM="${2:?test avm}"
N="${3:?iterations}"
LOGDIR="${4:?log dir}"
MAX_S="${5:-300}"
HANG_S="${6:-260}"

mkdir -p "$LOGDIR"

passed=0
failed=0
timedout=0
hung=0

for i in $(seq 1 "$N"); do
    out="$LOGDIR/run_$i.out"
    err="$LOGDIR/run_$i.err"
    : > "$out"
    : > "$err"

    t0=$(python3 -c "import time;print(int(time.monotonic()*1000))")
    "$BIN" "$AVM" > "$out" 2> "$err" &
    PID=$!
    deadline=$((SECONDS + MAX_S))
    hang_deadline=$((SECONDS + HANG_S))
    last_size=0
    last_motion=$SECONDS
    while kill -0 "$PID" 2>/dev/null; do
        sleep 5
        cur_size=$(wc -c < "$out" 2>/dev/null || echo 0)
        if [ "$cur_size" != "$last_size" ]; then
            last_motion=$SECONDS
            last_size=$cur_size
        fi
        if [ "$SECONDS" -ge "$deadline" ]; then
            echo "iter $i HARD-TIMEOUT after ${MAX_S}s"
            kill -USR1 "$PID" 2>/dev/null
            sleep 2
            kill -TERM "$PID" 2>/dev/null
            sleep 2
            kill -KILL "$PID" 2>/dev/null
            hung=$((hung + 1))
            break
        fi
        if [ $((SECONDS - last_motion)) -gt 30 ] && [ "$SECONDS" -ge "$hang_deadline" ]; then
            echo "iter $i SOFT-HANG: no output in 30s and past hang_s=${HANG_S}s, dumping trace"
            kill -USR1 "$PID" 2>/dev/null
            sleep 3
            kill -TERM "$PID" 2>/dev/null
            sleep 2
            kill -KILL "$PID" 2>/dev/null
            mv "$out" "$LOGDIR/hung_$i.out"
            mv "$err" "$LOGDIR/hung_$i.err"
            hung=$((hung + 1))
            break
        fi
    done
    wait "$PID" 2>/dev/null
    rc=$?
    t1=$(python3 -c "import time;print(int(time.monotonic()*1000))")
    dt=$((t1 - t0))
    if [ "$rc" -eq 0 ]; then
        passed=$((passed + 1))
        echo "iter $i ok ${dt}ms"
    elif [ -f "$LOGDIR/hung_$i.out" ]; then
        echo "iter $i HUNG ${dt}ms"
    else
        failed=$((failed + 1))
        mv "$out" "$LOGDIR/fail_${i}_rc${rc}.out"
        mv "$err" "$LOGDIR/fail_${i}_rc${rc}.err"
        echo "iter $i FAIL rc=$rc ${dt}ms"
    fi
done

echo "----"
echo "passed=$passed failed=$failed hung=$hung"
