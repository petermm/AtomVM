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
    ok = test_basic(),
    ok = test_wraparound(),
    ok = test_shared(),
    ok = test_info(),
    ok = test_badarg(),
    ok.

test_basic() ->
    Ref = counters:new(2, []),
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

test_wraparound() ->
    Ref = counters:new(1, []),
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

test_shared() ->
    Ref = counters:new(1, []),
    Parent = self(),
    spawn_opt(fun() ->
        ok = counters:add(Ref, 1, 13),
        Parent ! done
    end, []),
    receive
        done -> 13 = counters:get(Ref, 1)
    end,
    ok.

test_info() ->
    Ref = counters:new(3, []),
    #{size := 3, min := Min, max := Max, memory := Memory} = counters:info(Ref),
    -(1 bsl 63) = Min,
    (1 bsl 63) - 1 = Max,
    true = Memory > 0,
    ok.

test_badarg() ->
    Ref = counters:new(1, []),
    assert_badarg(fun() -> counters:new(0, []) end),
    assert_badarg(fun() -> counters:new(1, [not_an_option]) end),
    assert_badarg(fun() -> counters:get(Ref, 0) end),
    assert_badarg(fun() -> counters:get(not_a_ref, 1) end),
    assert_badarg(fun() -> counters:put(Ref, 1, 1 bsl 63) end),
    assert_badarg(fun() -> counters:add(Ref, 1, 1 bsl 64) end),
    assert_badarg(fun() -> counters:sub(Ref, 1, (1 bsl 63) + 1) end),
    assert_badarith(fun() -> counters:sub(Ref, 1, not_an_integer) end),
    ok = test_write_concurrency_option(),
    ok.

test_write_concurrency_option() ->
    try counters:new(1, [write_concurrency]) of
        Ref ->
            ok = counters:add(Ref, 1, 1),
            1 = counters:get(Ref, 1),
            ok
    catch
        error:badarg ->
            ok
    end.

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
