#!/bin/bash
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
# Build the AtomVM source tree mounted at /work in the Docker image
# created from tools/Dockerfile.sched-trace, then loop test_estdlib.avm
# under valgrind. Designed to be run *inside* the container.
#
# Caller controls build flags via the C_FLAGS env var, iteration count
# via N, and per-iteration soft timeout via MAX_S. The log directory
# is BUILD_LOG_DIR (defaults to /work/build-docker-logs).
#
# Outputs:
#   $BUILD_LOG_DIR/build.log         - build output
#   $BUILD_LOG_DIR/iter_<i>.out      - test stdout per iteration
#   $BUILD_LOG_DIR/iter_<i>.err      - test stderr (incl. SCHED_TRACE dump)
#   $BUILD_LOG_DIR/summary.txt       - one-line per iteration result
#   $BUILD_LOG_DIR/env.txt           - tool versions / env snapshot
#
# Usage (from host):
#   docker build --platform linux/amd64 -t atomvm-sched-trace \
#       -f tools/Dockerfile.sched-trace .
#   docker run --rm --platform linux/amd64 \
#       -v "$PWD:/work" \
#       -e C_FLAGS="-DAVM_SCHED_TRACE" \
#       -e N=3 \
#       -e MAX_S=900 \
#       atomvm-sched-trace \
#       bash tools/run-sched-trace-docker.sh
#
set -u

C_FLAGS="${C_FLAGS:-}"
BUILD_DIR="${BUILD_DIR:-/work/build-docker}"
BUILD_LOG_DIR="${BUILD_LOG_DIR:-/work/build-docker-logs}"
N="${N:-3}"
MAX_S="${MAX_S:-900}"

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR" "$BUILD_LOG_DIR"

{
    echo "uname: $(uname -a)"
    echo "gcc: $(gcc --version | head -1)"
    echo "cmake: $(cmake --version | head -1)"
    echo "valgrind: $(valgrind --version)"
    erl -version 2>&1 || true
    echo "C_FLAGS=$C_FLAGS"
    echo "N=$N"
    echo "MAX_S=$MAX_S"
    echo "Build dir: $BUILD_DIR"
    echo "Log dir:   $BUILD_LOG_DIR"
} > "$BUILD_LOG_DIR/env.txt"

cd "$BUILD_DIR"
cmake -G Ninja -DCMAKE_C_FLAGS="$C_FLAGS" /work \
    > "$BUILD_LOG_DIR/build.log" 2>&1
if ! ninja AtomVM test_estdlib >> "$BUILD_LOG_DIR/build.log" 2>&1; then
    echo "build failed; see $BUILD_LOG_DIR/build.log" >&2
    tail -30 "$BUILD_LOG_DIR/build.log" >&2
    exit 1
fi

ls -la src/AtomVM tests/libs/estdlib/test_estdlib.avm \
    >> "$BUILD_LOG_DIR/env.txt"

: > "$BUILD_LOG_DIR/summary.txt"

passed=0
failed=0
timedout=0

for i in $(seq 1 "$N"); do
    out="$BUILD_LOG_DIR/iter_$i.out"
    err="$BUILD_LOG_DIR/iter_$i.err"
    t0=$(date +%s)
    # --error-exitcode=1 so memory errors fail the iteration; same as CI.
    # --child-silent-after-fork=yes so subprocess valgrind output does
    # not contaminate the trace dump.
    timeout "$MAX_S" \
        valgrind \
            --error-exitcode=1 \
            --child-silent-after-fork=yes \
            ./src/AtomVM ./tests/libs/estdlib/test_estdlib.avm \
        > "$out" 2> "$err"
    rc=$?
    t1=$(date +%s)
    dt=$((t1 - t0))
    case "$rc" in
        0)
            passed=$((passed + 1))
            echo "iter $i ok ${dt}s rc=$rc" | tee -a "$BUILD_LOG_DIR/summary.txt"
            ;;
        124)
            timedout=$((timedout + 1))
            mv "$out" "$BUILD_LOG_DIR/timeout_$i.out"
            mv "$err" "$BUILD_LOG_DIR/timeout_$i.err"
            echo "iter $i TIMEOUT ${dt}s rc=$rc" \
                | tee -a "$BUILD_LOG_DIR/summary.txt"
            ;;
        *)
            failed=$((failed + 1))
            mv "$out" "$BUILD_LOG_DIR/fail_${i}_rc${rc}.out"
            mv "$err" "$BUILD_LOG_DIR/fail_${i}_rc${rc}.err"
            echo "iter $i FAIL rc=$rc ${dt}s" \
                | tee -a "$BUILD_LOG_DIR/summary.txt"
            ;;
    esac
    # Kill epmd that the test left behind so the next iteration's
    # test_epmd can bind 4369 itself. Same as the scratch-branch
    # workflow.
    epmd -kill > /dev/null 2>&1 || true
    pkill -x epmd > /dev/null 2>&1 || true
done

echo "----" | tee -a "$BUILD_LOG_DIR/summary.txt"
echo "passed=$passed failed=$failed timedout=$timedout" \
    | tee -a "$BUILD_LOG_DIR/summary.txt"

if [ "$failed" -gt 0 ] || [ "$timedout" -gt 0 ]; then
    exit 1
fi
exit 0
