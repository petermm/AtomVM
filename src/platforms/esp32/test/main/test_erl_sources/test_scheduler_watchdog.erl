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

-module(test_scheduler_watchdog).
-export([start/0]).

start() ->
    ok = test_status_returns_proplist(),
    ok = test_mode_switch(),
    ok = test_mode_badarg(),
    ok = test_stall_updates_ages(),
    ok.

%% Verify scheduler_watchdog_status/0 returns a well-formed proplist
test_status_returns_proplist() ->
    Status = atomvm:scheduler_watchdog_status(),
    true = is_list(Status),
    {mode, Mode} = lists:keyfind(mode, 1, Status),
    true = (Mode =:= restart orelse Mode =:= log_only),
    {timeout_ms, TimeoutMs} = lists:keyfind(timeout_ms, 1, Status),
    true = is_integer(TimeoutMs),
    {poll_interval_ms, PollMs} = lists:keyfind(poll_interval_ms, 1, Status),
    true = is_integer(PollMs),
    {active_mask, Mask} = lists:keyfind(active_mask, 1, Status),
    true = is_integer(Mask),
    {ages, Ages} = lists:keyfind(ages, 1, Status),
    true = is_list(Ages),
    ok.

%% Verify mode can be toggled and status reflects the change
test_mode_switch() ->
    ok = atomvm:scheduler_watchdog_mode(log_only),
    S1 = atomvm:scheduler_watchdog_status(),
    {mode, log_only} = lists:keyfind(mode, 1, S1),

    ok = atomvm:scheduler_watchdog_mode(restart),
    S2 = atomvm:scheduler_watchdog_status(),
    {mode, restart} = lists:keyfind(mode, 1, S2),

    %% Leave in log_only for safety during test
    ok = atomvm:scheduler_watchdog_mode(log_only),
    ok.

%% Verify badarg on invalid mode
test_mode_badarg() ->
    ok =
        try
            atomvm:scheduler_watchdog_mode(invalid),
            fail
        catch
            error:badarg -> ok
        end,
    ok =
        try
            atomvm:scheduler_watchdog_mode(42),
            fail
        catch
            error:badarg -> ok
        end,
    ok.

%% Stall a scheduler briefly and verify the ages list is populated
test_stall_updates_ages() ->
    %% Ensure we're in log_only mode so no reboot
    ok = atomvm:scheduler_watchdog_mode(log_only),

    %% Stall this scheduler for 100ms to generate heartbeat activity
    ok = atomvm:debug_stall(100),

    Status = atomvm:scheduler_watchdog_status(),
    {active_mask, Mask} = lists:keyfind(active_mask, 1, Status),
    true = Mask > 0,
    {ages, Ages} = lists:keyfind(ages, 1, Status),
    true = length(Ages) > 0,
    lists:foreach(
        fun({SlotId, AgeMs}) ->
            true = is_integer(SlotId),
            true = SlotId >= 0,
            true = is_integer(AgeMs),
            true = AgeMs >= 0
        end,
        Ages
    ),
    %% Restore default mode
    ok = atomvm:scheduler_watchdog_mode(restart),
    ok.
