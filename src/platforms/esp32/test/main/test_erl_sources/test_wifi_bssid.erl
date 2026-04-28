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

%% Test connecting to an access point by BSSID.
%%
%% Implements https://github.com/atomvm/AtomVM/issues/2144 - Network driver
%% should allow connecting to a bssid.

-module(test_wifi_bssid).

-export([start/0]).

%% CI sim test network
-define(FIND_NETWORK, <<"Wokwi-GUEST">>).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            case erlang:system_info(esp32_chip_info) of
                #{model := esp32_s2} ->
                    io:format("test_wifi_bssid skipped on this platform.~n"),
                    ok;
                #{model := esp32_h2} ->
                    io:format("No wifi, test_wifi_bssid skipped on this platform.~n"),
                    ok;
                _ ->
                    ok = test_invalid_bssid_rejected(),
                    BSSID = discover_test_bssid(),
                    ok = test_bssid_requires_ssid(BSSID),
                    ok = test_connect_by_ssid_and_bssid(BSSID),
                    network:stop(),
                    ok
            end;
        Error ->
            Error
    end.

verify_platform(esp32) -> ok;
verify_platform(Platform) -> {error, {unsupported_platform, Platform}}.

%% Start the driver and look up the BSSID for the well-known test SSID.
discover_test_bssid() ->
    {ok, _} = network:start([{sta, [managed]}]),
    try
        case network:wifi_scan([{results, 10}, {dwell, 200}]) of
            {ok, {_Num, Networks}} ->
                case
                    lists:filter(
                        fun(#{ssid := SSID}) -> SSID =:= ?FIND_NETWORK end, Networks
                    )
                of
                    [#{bssid := BSSID} | _] when is_binary(BSSID), byte_size(BSSID) =:= 6 ->
                        io:format("Discovered ~p with BSSID ~p~n", [?FIND_NETWORK, BSSID]),
                        BSSID;
                    [] ->
                        erlang:error({test_network_not_found, ?FIND_NETWORK})
                end;
            {error, Reason} ->
                erlang:error({scan_failed, Reason})
        end
    after
        ok = network:stop()
    end.

%% Verify that an invalid BSSID is rejected by the driver before any
%% connection attempt is made.
test_invalid_bssid_rejected() ->
    %% Start in managed mode so the driver does not attempt to connect on its own
    {ok, _Pid} = network:start([{sta, [managed]}]),
    try
        %% BSSID must be a 6-byte binary; supplying a 5-byte one is invalid
        {error, _} = network:sta_connect([
            {ssid, ?FIND_NETWORK}, {bssid, <<1, 2, 3, 4, 5>>}, {psk, ""}
        ]),
        %% Atom is not a binary
        {error, _} = network:sta_connect([{ssid, "irrelevant"}, {bssid, not_a_binary}, {psk, ""}]),
        io:format("test_invalid_bssid_rejected OK.~n"),
        ok
    after
        ok = network:stop()
    end.

%% ESP-IDF requires SSID even when BSSID is supplied.
test_bssid_requires_ssid(BSSID) ->
    {ok, _Pid} = network:start([{sta, [managed]}]),
    try
        {error, no_ssid} = network:sta_connect([{bssid, BSSID}, {psk, ""}]),
        io:format("test_bssid_requires_ssid OK.~n"),
        ok
    after
        ok = network:stop()
    end.

%% Connect with both SSID and BSSID supplied. This should succeed and pin the
%% connection to the specific access point.
test_connect_by_ssid_and_bssid(BSSID) ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, ?FIND_NETWORK},
            {bssid, BSSID},
            {psk, ""},
            {connected, fun() -> Self ! sta_connected end},
            {got_ip, fun(IpInfo) -> Self ! {got_ip, IpInfo} end},
            {disconnected, fun() -> Self ! sta_disconnected end}
        ]}
    ],
    {ok, _Pid} = network:start(Config),
    case wait_for_ip(20000) of
        ok ->
            connected = network:sta_status(),
            io:format("test_connect_by_ssid_and_bssid OK.~n"),
            ok;
        E ->
            ok = network:stop(),
            erlang:error({connect_by_ssid_and_bssid_failed, E})
    end.

wait_for_ip(Timeout) ->
    receive
        {got_ip, IpInfo} ->
            io:format("Got IP: ~p~n", [IpInfo]),
            ok
    after Timeout ->
        {error, timeout}
    end.
