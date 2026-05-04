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

%%-----------------------------------------------------------------------------
%% @doc Dialyzer-only stubs for AtomVM native `erts_internal' functions.
%%      This module is compiled only for PLT creation and is not packaged into
%%      AtomVM libraries.
%% @end
%%-----------------------------------------------------------------------------
-module(erts_internal).

-export([
    cmp_term/2,
    atomics_new/2
]).

-spec cmp_term(A :: term(), B :: term()) -> -1 | 0 | 1.
cmp_term(_A, _B) ->
    erlang:nif_error(undefined).

%% Note: the second argument is an encoded options bitmask (see
%% atomics:new/2 in libs/estdlib), not the user-facing options list.
-spec atomics_new(Arity :: pos_integer(), OptsBitmask :: non_neg_integer()) -> reference().
atomics_new(_Arity, _OptsBitmask) ->
    erlang:nif_error(undefined).
