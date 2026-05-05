%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. Madsen <petermm@gmail.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_counters).

-export([start/0, test/0]).

start() ->
    ok = test(),
    0.

test() ->
    Types = [atomics, write_concurrency],
    [ok = test_basic(Type) || Type <- Types],
    [ok = test_wraparound(Type) || Type <- Types],
    [ok = test_shared(Type) || Type <- Types],
    [ok = test_info(Type) || Type <- Types],
    ok = test_write_concurrency_total(),
    ok = test_write_concurrency_put(),
    ok = test_badarg(),
    ok.

new_counter(atomics, Size) ->
    counters:new(Size, [atomics]);
new_counter(write_concurrency, Size) ->
    counters:new(Size, [write_concurrency]).

test_basic(Type) ->
    Ref = new_counter(Type, 2),
    0 = counters:get(Ref, 1),
    ok = counters:add(Ref, 1, 42),
    42 = counters:get(Ref, 1),
    ok = counters:sub(Ref, 1, 12),
    30 = counters:get(Ref, 1),
    ok = counters:put(Ref, 2, -7),
    -7 = counters:get(Ref, 2),
    ExplicitRef = counters:new(1, [atomics]),
    ok = counters:add(ExplicitRef, 1, 1),
    1 = counters:get(ExplicitRef, 1),
    ok.

test_wraparound(Type) ->
    Ref = new_counter(Type, 1),
    Max = (1 bsl 63) - 1,
    Min = -(1 bsl 63),
    ok = counters:put(Ref, 1, Max),
    ok = counters:add(Ref, 1, 1),
    Min = counters:get(Ref, 1),
    ok = counters:sub(Ref, 1, 1),
    Max = counters:get(Ref, 1),
    MaxU64 = (1 bsl 64) - 1,
    ok = counters:put(Ref, 1, 0),
    ok = counters:sub(Ref, 1, -MaxU64),
    -1 = counters:get(Ref, 1),
    ok.

test_shared(Type) ->
    Ref = new_counter(Type, 1),
    Parent = self(),
    Pid = spawn(fun() ->
        ok = counters:add(Ref, 1, 13),
        Parent ! {self(), done},
        receive stop -> ok end
    end),
    receive
        {Pid, done} ->
            case counters:get(Ref, 1) of
                13 -> ok;
                Value -> error({shared_counter_failed, Type, Value})
            end
    end,
    Pid ! stop,
    ok.

test_info(atomics) ->
    Ref = new_counter(atomics, 3),
    Info = counters:info(Ref),
    #{size := 3, min := Min, max := Max, memory := Memory} = Info,
    -(1 bsl 63) = Min,
    (1 bsl 63) - 1 = Max,
    true = Memory > 0,
    ok;
test_info(write_concurrency) ->
    Ref = new_counter(write_concurrency, 3),
    Info = counters:info(Ref),
    #{size := 3, memory := Memory} = Info,
    true = Memory > 0,
    false = has_map_key(min, Info),
    false = has_map_key(max, Info),
    ok.

has_map_key(Key, Map) ->
    case Map of
        #{Key := _} -> true;
        _ -> false
    end.

test_write_concurrency_total() ->
    Ref = new_counter(write_concurrency, 1),
    Workers = 6,
    Rounds = 100,
    Incr = 3,
    Pids = start_workers(Ref, 1, Workers, Rounds, Incr),
    ok = wait_workers(Pids),
    Expected = Workers * Rounds * Incr,
    Expected = counters:get(Ref, 1),
    ok = stop_workers(Pids),
    ok.

test_write_concurrency_put() ->
    Ref = new_counter(write_concurrency, 1),
    Pids = start_workers(Ref, 1, 4, 50, 2),
    ok = wait_workers(Pids),
    ok = counters:put(Ref, 1, -17),
    -17 = counters:get(Ref, 1),
    ok = counters:add(Ref, 1, 5),
    -12 = counters:get(Ref, 1),
    ok = stop_workers(Pids),
    ok.

start_workers(Ref, Ix, Workers, Rounds, Incr) ->
    Parent = self(),
    [
        spawn(fun() ->
            ok = worker_loop(Rounds, Ref, Ix, Incr),
            Parent ! {self(), done},
            receive stop -> ok end
        end)
        || _ <- lists:seq(1, Workers)
    ].

wait_workers(Pids) ->
    [receive {Pid, done} -> ok end || Pid <- Pids],
    ok.

stop_workers(Pids) ->
    [Pid ! stop || Pid <- Pids],
    ok.

worker_loop(0, _Ref, _Ix, _Incr) ->
    ok;
worker_loop(N, Ref, Ix, Incr) ->
    ok = counters:add(Ref, Ix, Incr),
    worker_loop(N - 1, Ref, Ix, Incr).

test_badarg() ->
    assert_badarg(fun() -> counters:new(0, []) end),
    assert_badarg(fun() -> counters:new(1, [not_an_option]) end),
    assert_badarg(fun() -> counters:new(1, [write_concurrency | bad]) end),
    assert_system_limit(fun() -> counters:new(1 bsl 64, [write_concurrency]) end),
    [ok = test_badarg_for_type(Type) || Type <- [atomics, write_concurrency]],
    ok.

test_badarg_for_type(Type) ->
    Ref = new_counter(Type, 1),
    assert_badarg(fun() -> counters:get(Ref, 0) end),
    assert_badarg(fun() -> counters:get(not_a_ref, 1) end),
    assert_badarg(fun() -> counters:get({atomics, make_ref()}, 1) end),
    assert_badarg(fun() -> counters:get({write_concurrency, make_ref()}, 1) end),
    assert_badarg(fun() -> counters:put(Ref, 1, 1 bsl 63) end),
    assert_badarg(fun() -> counters:add(Ref, 1, 1 bsl 64) end),
    assert_badarg(fun() -> counters:sub(Ref, 1, (1 bsl 63) + 1) end),
    assert_badarith(fun() -> counters:sub(Ref, 1, not_an_integer) end),
    ok.

assert_badarg(Fun) ->
    try
        Fun(),
        error(expected_badarg)
    catch
        error:badarg ->
            ok
    end.

assert_badarith(Fun) ->
    try
        Fun(),
        error(expected_badarith)
    catch
        error:badarith ->
            ok
    end.

assert_system_limit(Fun) ->
    try
        Fun(),
        error(expected_system_limit)
    catch
        error:system_limit ->
            ok
    end.
