#!/bin/bash
# scheduler-baseline.sh — repeated runs of test_estdlib, compare latency stats
# Usage: scheduler-baseline.sh <atomvm_binary> <test_avm> <iterations> <log_dir> [timeout_s]

set -u

BIN="${1:?atomvm binary}"
AVM="${2:?test avm}"
N="${3:?iterations}"
LOGDIR="${4:?log dir}"
TIMEOUT="${5:-180}"

mkdir -p "$LOGDIR"

passed=0
failed=0
timedout=0
max_runtime=0
min_runtime=99999

for i in $(seq 1 "$N"); do
    out="$LOGDIR/run_$i.out"
    err="$LOGDIR/run_$i.err"
    t0=$(python3 -c "import time;print(int(time.monotonic()*1000))")
    "$BIN" "$AVM" > "$out" 2> "$err"
    rc=$?
    t1=$(python3 -c "import time;print(int(time.monotonic()*1000))")
    dt=$((t1 - t0))
    if [ "$dt" -gt "$max_runtime" ]; then max_runtime=$dt; fi
    if [ "$dt" -lt "$min_runtime" ]; then min_runtime=$dt; fi
    if [ "$rc" -eq 124 ]; then
        timedout=$((timedout + 1))
        mv "$out" "$LOGDIR/timeout_$i.out"
        mv "$err" "$LOGDIR/timeout_$i.err"
        echo "iter $i TIMEOUT ${dt}ms"
    elif [ "$rc" -ne 0 ]; then
        failed=$((failed + 1))
        mv "$out" "$LOGDIR/fail_${i}_rc${rc}.out"
        mv "$err" "$LOGDIR/fail_${i}_rc${rc}.err"
        echo "iter $i FAIL rc=$rc ${dt}ms"
    else
        # Only keep test_net_kernel report and the trailing test summary.
        passed=$((passed + 1))
        echo "iter $i ok ${dt}ms"
    fi
done

echo "----"
echo "passed=$passed failed=$failed timedout=$timedout"
echo "runtime ms: min=$min_runtime max=$max_runtime"
