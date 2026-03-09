%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(test_wifi_restart).

-export([start/0]).

-define(ITERATIONS, 10).

start() ->
    case atomvm:platform() of
        esp32 ->
            restart_loop(1);
        Platform ->
            {error, {unsupported_platform, Platform}}
    end.

restart_loop(I) when I > ?ITERATIONS ->
    ok;
restart_loop(I) ->
    io:format("wifi restart iteration ~p/~p~n", [I, ?ITERATIONS]),
    case network:start([{sta, [managed]}]) of
        {ok, _Pid} ->
            try
                ok = network:stop(),
                restart_loop(I + 1)
            catch
                Class:Reason:Stacktrace ->
                    _ = catch network:stop(),
                    erlang:raise(Class, Reason, Stacktrace)
            end;
        Error ->
            Error
    end.
