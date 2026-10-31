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

-module(test_wifi_managed).

-export([start/0]).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            try
                ok = test_managed_start(),
                ok = test_bssid_requires_ssid(),
                ok = test_invalid_bssid_rejected(),
                ok = test_managed_connect(),
                ok = test_sta_status_lifecycle(),
                ok = test_disconnect_and_reconnect(),
                ok = test_connect_new_ap(),
                BSSID = test_connect_by_bssid(),
                ok = test_start_with_bssid(BSSID),
                ok = test_bogus_bssid_does_not_connect(),
                ok
            after
                ok = network:stop()
            end;
        Error ->
            Error
    end.

%% Test starting the network driver in managed mode (no immediate connection)
test_managed_start() ->
    Self = self(),
    Config =
        [
            {sta, [
                managed,
                {connected, fun() -> Self ! sta_connected end},
                {got_ip, fun(IpInfo) -> Self ! {got_ip, IpInfo} end},
                {disconnected, fun() -> Self ! sta_disconnected end}
            ]},
            {sntp, [{host, "time.aws.com"}, {synchronized, fun sntp_synchronized/1}]}
        ],
    case network:start(Config) of
        {ok, _Pid} ->
            io:format("Managed network started.~n"),
            %% In managed mode, sta_status should be disconnected (not connecting)
            disconnected = network:sta_status(),
            io:format("test_managed_start OK.~n"),
            ok;
        Error ->
            Error
    end.

%% ESP-IDF connects to an SSID, so a bssid alone is not enough.
test_bssid_requires_ssid() ->
    {error, no_ssid} = network:sta_connect([{bssid, <<0, 1, 2, 3, 4, 5>>}]),
    io:format("test_bssid_requires_ssid OK.~n"),
    ok.

%% A bssid must be a 6-byte binary; network:sta_connect/1 refuses anything else.
test_invalid_bssid_rejected() ->
    {error, badarg} = network:sta_connect([
        {ssid, "Wokwi-GUEST"}, {bssid, <<1, 2, 3, 4, 5>>}, {psk, ""}
    ]),
    {error, badarg} = network:sta_connect([
        {ssid, "Wokwi-GUEST"}, {bssid, not_a_binary}, {psk, ""}
    ]),
    io:format("test_invalid_bssid_rejected OK.~n"),
    ok.

%% Test connecting to an AP using sta_connect/1
test_managed_connect() ->
    %% Status should still be disconnected before connect
    disconnected = network:sta_status(),
    ok =
        network:sta_connect([
            {ssid, "Wokwi-GUEST"},
            {psk, ""},
            {sntp, [{host, "pool.ntp.org"}, {synchronized, fun sntp_synchronized/1}]}
        ]),
    %% After initiating connect, status should be connecting
    Status = network:sta_status(),
    case Status of
        connecting ->
            ok;
        associated ->
            ok;
        connected ->
            ok;
        Other ->
            error({unexpected_sta_status, Other})
    end,
    case wait_for_ip(30000) of
        ok -> ok;
        E -> error({waiting_for_ip, E})
    end,
    connected = network:sta_status(),
    io:format("test_managed_connect OK.~n"),
    ok.

%% Test the full sta_status lifecycle
test_sta_status_lifecycle() ->
    %% We should be connected from previous test
    connected = network:sta_status(),
    io:format("test_sta_status_lifecycle OK.~n"),
    ok.

%% Test disconnecting and reconnecting to the same AP
test_disconnect_and_reconnect() ->
    ok = network:sta_disconnect(),
    ok = wait_for_disconnect(5000),
    disconnected = network:sta_status(),
    %% Reconnect using sta_connect/0 (reconnect to last AP)
    ok = network:sta_connect(),
    ok = wait_for_ip(30000),
    connected = network:sta_status(),
    io:format("test_disconnect_and_reconnect OK.~n"),
    ok.

%% Test connecting to a new AP using sta_connect/1 with sta_config
test_connect_new_ap() ->
    ok = network:sta_disconnect(),
    ok = wait_for_disconnect(5000),
    disconnected = network:sta_status(),
    %% Connect again with explicit config
    ok = network:sta_connect([{ssid, "Wokwi-GUEST"}, {psk, ""}]),
    ok = wait_for_ip(30000),
    connected = network:sta_status(),
    io:format("test_connect_new_ap OK.~n"),
    ok.

%% Test connecting to one access point among those sharing an ssid.
test_connect_by_bssid() ->
    ok = network:sta_disconnect(),
    ok = wait_for_disconnect(5000),
    case network:wifi_scan([{results, 10}]) of
        {ok, {_Num, Networks}} ->
            case
                [BSSID || #{ssid := SSID, bssid := BSSID} <- Networks, SSID =:= <<"Wokwi-GUEST">>]
            of
                [BSSID | _] ->
                    io:format("Connecting to Wokwi-GUEST with BSSID ~p~n", [BSSID]),
                    ok = network:sta_connect([
                        {ssid, "Wokwi-GUEST"}, {bssid, BSSID}, {psk, ""}
                    ]),
                    ok = wait_for_ip(30000),
                    connected = network:sta_status(),
                    io:format("test_connect_by_bssid OK.~n"),
                    BSSID;
                [] ->
                    error({no_bssid_for_network, <<"Wokwi-GUEST">>})
            end;
        Error ->
            error({scan_failed, Error})
    end.

%% The STA configuration passed to network:start/1 must honor a bssid too.
test_start_with_bssid(BSSID) ->
    ok = network:stop(),
    flush_network_messages(),
    Self = self(),
    {ok, _Pid} = network:start([
        {sta, [
            {ssid, "Wokwi-GUEST"},
            {bssid, BSSID},
            {psk, ""},
            {got_ip, fun(IpInfo) -> Self ! {got_ip, IpInfo} end}
        ]}
    ]),
    ok = wait_for_ip(30000),
    connected = network:sta_status(),
    io:format("test_start_with_bssid OK.~n"),
    ok.

%% A valid but nonexistent bssid must not fall back to another AP with the
%% same ssid; without bssid_set this would connect and get an address.
test_bogus_bssid_does_not_connect() ->
    ok = network:stop(),
    flush_network_messages(),
    Self = self(),
    {ok, _Pid} = network:start([
        {sta, [managed, {got_ip, fun(IpInfo) -> Self ! {got_ip, IpInfo} end}]}
    ]),
    Result = network:sta_connect([
        {ssid, "Wokwi-GUEST"}, {bssid, <<0, 0, 0, 0, 0, 1>>}, {psk, ""}
    ]),
    case Result of
        ok -> {error, timeout} = wait_for_ip(5000);
        {error, _Reason} -> ok
    end,
    ok = network:stop(),
    io:format("test_bogus_bssid_does_not_connect OK.~n"),
    ok.

flush_network_messages() ->
    receive
        sta_connected -> flush_network_messages();
        sta_disconnected -> flush_network_messages();
        {got_ip, _IpInfo} -> flush_network_messages()
    after 0 -> ok
    end.

sntp_synchronized({TVSec, TVUsec}) ->
    io:format("Synchronized time with SNTP server. TVSec=~p TVUsec=~p~n", [TVSec, TVUsec]).

verify_platform(esp32) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.

wait_for_ip(Timeout) ->
    receive
        {got_ip, IpInfo} ->
            io:format("Got IP: ~p~n", [IpInfo]),
            ok
    after Timeout ->
        {error, timeout}
    end.

wait_for_disconnect(Timeout) ->
    receive
        sta_disconnected ->
            io:format("STA disconnected.~n"),
            ok
    after Timeout ->
        io:format("wait_for_disconnect timeout.~n"),
        {error, timeout}
    end.
