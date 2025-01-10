%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(epmd_disterl).

-export([start/0]).

start() ->
    Creds = [
        {ssid, "myssid"},
        {psk, "mypsk"}
    ],
    case network:wait_for_sta(Creds, 30000) of
        {ok, {Address, _Netmask, _Gateway}} ->
            distribution_start(Address);
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.

distribution_start(Address) ->
    {ok, _EPMDPid} = epmd:start_link([]),
    {ok, _KernelPid} = kernel:start(normal, []),
    {X, Y, Z, T} = Address,
    Node = list_to_atom(lists:flatten(io_lib:format("atomvm@~B.~B.~B.~B", [X, Y, Z, T]))),
    {ok, _NetKernelPid} = net_kernel:start(Node, #{name_domain => longnames}),
    io:format("Distribution was started\n"),
    io:format("Node is ~p\n", [node()]),
    net_kernel:set_cookie(<<"AtomVM">>),
    io:format("Cookie is ~s\n", [net_kernel:get_cookie()]),
    register(disterl, self()),
    io:format(
        "This AtomVM node is waiting for 'quit' message, and this process is registered as 'disterl'\n"
    ),
    io:format("On an OTP node with long names distribution, run:\n"),
    io:format("erlang:set_cookie('~s', 'AtomVM').\n", [Node]),
    io:format("{disterl, '~s'} ! quit.\n", [Node]),
    receive
        quit -> ok
    end.
