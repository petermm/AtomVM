%%
%% This file is part of AtomVM.
%%
%% Copyright 2026 Peter M <petermm@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%

-module(main).
-export([start/0]).

start() ->
    erlang:display(atomvm_openc6_dev_mode_start),
    
    erlang:display(launching_wifi_ap),
    case openc6:wifi_start_ap("AtomVM-C6-Dev", "password") of
        ok ->
            erlang:display(wifi_ap_started_successfully);
        error ->
            erlang:display(wifi_ap_start_failed)
    end,
    
    cycle_status().

cycle_status() ->
    % Flash LED Red
    openc6:set_led_color(255, 0, 0),
    sleep(50),
    
    % Flash LED Green
    openc6:set_led_color(0, 255, 0),
    sleep(50),
    
    % Flash LED Blue
    openc6:set_led_color(0, 0, 255),
    sleep(50),
    
    % Turn off LED and check status
    openc6:set_led_color(0, 0, 0),
    IsConnected = openc6:wifi_is_connected(),
    erlang:display({wifi_connected, IsConnected}),
    sleep(50),
    
    cycle_status().

sleep(Ms) ->
    receive
    after Ms ->
        ok
    end.
