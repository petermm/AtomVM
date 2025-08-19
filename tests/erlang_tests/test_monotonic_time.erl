%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(test_monotonic_time).

-export([start/0]).

start() ->
    % Test monotonic_time/1 with millisecond
    T1 = erlang:monotonic_time(millisecond),
    receive
    after 1 -> ok
    end,
    T2 = erlang:monotonic_time(millisecond),
    Diff1 = T2 - T1,
    
    % Test monotonic_time/0 (native nanoseconds)
    T3 = erlang:monotonic_time(),
    receive
    after 1 -> ok
    end,
    T4 = erlang:monotonic_time(),
    Diff2 = T4 - T3,
    
    % Both tests must pass: millisecond diff >= 0, nanosecond diff >= 1000
    case {is_integer(Diff1) andalso Diff1 >= 0, is_integer(Diff2) andalso Diff2 >= 1000} of
        {true, true} -> 1;
        _ -> 0
    end.
