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

-module(test_atomics).

-export([start/0]).

start() ->
    ok = test_signed(),
    ok = test_unsigned(),
    ok = test_shared(),
    ok = test_info(),
    ok = test_badarg(),
    0.

test_signed() ->
    Ref = atomics:new(2, []),
    ok = atomics:put(Ref, 1, 10),
    10 = atomics:get(Ref, 1),
    ok = atomics:add(Ref, 1, 32),
    42 = atomics:get(Ref, 1),
    50 = atomics:add_get(Ref, 1, 8),
    ok = atomics:sub(Ref, 1, 20),
    30 = atomics:get(Ref, 1),
    25 = atomics:sub_get(Ref, 1, 5),
    25 = atomics:exchange(Ref, 1, 7),
    7 = atomics:compare_exchange(Ref, 1, 8, 99),
    ok = atomics:compare_exchange(Ref, 1, 7, 99),
    99 = atomics:get(Ref, 1),
    Max = (1 bsl 63) - 1,
    Min = -(1 bsl 63),
    ok = atomics:put(Ref, 2, Max),
    ok = atomics:add(Ref, 2, 1),
    Min = atomics:get(Ref, 2),
    ok.

test_unsigned() ->
    Ref = atomics:new(1, [{signed, false}]),
    Max = (1 bsl 64) - 1,
    ok = atomics:put(Ref, 1, Max),
    Max = atomics:get(Ref, 1),
    ok = atomics:add(Ref, 1, 1),
    0 = atomics:get(Ref, 1),
    Max = atomics:add_get(Ref, 1, -1),
    ok = atomics:compare_exchange(Ref, 1, Max, 123),
    123 = atomics:get(Ref, 1),
    ok.

test_shared() ->
    Ref = atomics:new(1, []),
    Parent = self(),
    spawn_opt(fun() ->
        ok = atomics:add(Ref, 1, 42),
        Parent ! done
    end, []),
    receive
        done -> 42 = atomics:get(Ref, 1)
    end,
    ok.

test_info() ->
    SignedRef = atomics:new(2, []),
    #{size := 2, min := SignedMin, max := SignedMax, memory := SignedMemory} = atomics:info(SignedRef),
    4 = map_size(atomics:info(SignedRef)),
    -(1 bsl 63) = SignedMin,
    (1 bsl 63) - 1 = SignedMax,
    true = SignedMemory > 0,
    UnsignedRef = atomics:new(1, [{signed, false}]),
    #{size := 1, min := 0, max := UnsignedMax, memory := UnsignedMemory} = atomics:info(UnsignedRef),
    (1 bsl 64) - 1 = UnsignedMax,
    true = UnsignedMemory > 0,
    ok.

test_badarg() ->
    assert_badarg(fun() -> atomics:new(0, []) end),
    assert_badarg(fun() -> atomics:new(1, [{signed, 'maybe'}]) end),
    assert_badarg(fun() -> atomics:new(1, [signed]) end),
    Ref = atomics:new(1, []),
    assert_badarg(fun() -> atomics:get(Ref, 0) end),
    assert_badarg(fun() -> atomics:get(Ref, 2) end),
    assert_badarg(fun() -> atomics:put(Ref, 1, 1 bsl 63) end),
    UnsignedRef = atomics:new(1, [{signed, false}]),
    assert_badarg(fun() -> atomics:put(UnsignedRef, 1, -1) end),
    assert_badarg(fun() -> atomics:exchange(Ref, 1, 1 bsl 63) end),
    assert_badarg(fun() -> atomics:exchange(UnsignedRef, 1, -1) end),
    assert_badarg(fun() -> atomics:compare_exchange(Ref, 1, 0, 1 bsl 63) end),
    assert_badarg(fun() -> atomics:compare_exchange(UnsignedRef, 1, -1, 0) end),
    assert_badarg(fun() -> atomics:sub(Ref, 1, (1 bsl 63) + 1) end),
    assert_badarg(fun() -> atomics:sub_get(Ref, 1, (1 bsl 63) + 1) end),
    assert_badarith(fun() -> atomics:sub(Ref, 1, not_an_integer) end),
    MaxU64 = (1 bsl 64) - 1,
    ok = atomics:sub(UnsignedRef, 1, -MaxU64),
    MaxU64 = atomics:get(UnsignedRef, 1),
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
