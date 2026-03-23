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

-module(test_float_short).

-export([start/0]).

start() ->
    ok = test_short_uses_shortest_format(),
    ok.

test_short_uses_shortest_format() ->
    % These values distinguish shortest formatting from the
    % {decimals, 10}, compact fallback used when Ryu is disabled.
    <<"1.2e20">> = erlang:float_to_binary(1.2e20, [short]),
    <<"1.2e-20">> = erlang:float_to_binary(1.2e-20, [short]),
    "1.2e20" = erlang:float_to_list(1.2e20, [short]),
    "1.2e-20" = erlang:float_to_list(1.2e-20, [short]),
    ok.
