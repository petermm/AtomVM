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
    atomics_new/2,
    counters_new/1,
    counters_get/2,
    counters_add/3,
    counters_put/3,
    counters_info/1
]).

-spec cmp_term(A :: term(), B :: term()) -> -1 | 0 | 1.
cmp_term(_A, _B) ->
    erlang:nif_error(undefined).

-spec atomics_new(Arity :: pos_integer(), Opts :: [term()]) -> reference().
atomics_new(_Arity, _Opts) ->
    erlang:nif_error(undefined).

-spec counters_new(Size :: pos_integer()) -> reference().
counters_new(_Size) ->
    erlang:nif_error(undefined).

-spec counters_get(Ref :: reference(), Ix :: pos_integer()) -> integer().
counters_get(_Ref, _Ix) ->
    erlang:nif_error(undefined).

-spec counters_add(Ref :: reference(), Ix :: pos_integer(), Incr :: integer()) -> ok.
counters_add(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undefined).

-spec counters_put(Ref :: reference(), Ix :: pos_integer(), Value :: integer()) -> ok.
counters_put(_Ref, _Ix, _Value) ->
    erlang:nif_error(undefined).

-spec counters_info(Ref :: reference()) ->
    #{
        size := non_neg_integer(),
        memory := non_neg_integer()
    }.
counters_info(_Ref) ->
    erlang:nif_error(undefined).
