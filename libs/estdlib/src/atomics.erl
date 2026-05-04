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
%% @doc A limited implementation of the Erlang/OTP `atomics' module.
%% @end
%%-----------------------------------------------------------------------------
-module(atomics).

-export([
    new/2,
    put/3,
    get/2,
    add/3,
    add_get/3,
    sub/3,
    sub_get/3,
    exchange/3,
    compare_exchange/4,
    info/1
]).

-export_type([atomics_ref/0]).

-opaque atomics_ref() :: reference().

-type option() :: {signed, boolean()}.

-spec new(Arity :: pos_integer(), Opts :: [option()]) -> atomics_ref().
new(_Arity, _Opts) ->
    erlang:nif_error(undefined).

-spec put(Ref :: atomics_ref(), Ix :: pos_integer(), Value :: integer()) -> ok.
put(_Ref, _Ix, _Value) ->
    erlang:nif_error(undefined).

-spec get(Ref :: atomics_ref(), Ix :: pos_integer()) -> integer().
get(_Ref, _Ix) ->
    erlang:nif_error(undefined).

-spec add(Ref :: atomics_ref(), Ix :: pos_integer(), Incr :: integer()) -> ok.
add(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undefined).

-spec add_get(Ref :: atomics_ref(), Ix :: pos_integer(), Incr :: integer()) -> integer().
add_get(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undefined).

-spec sub(Ref :: atomics_ref(), Ix :: pos_integer(), Decr :: integer()) -> ok.
sub(_Ref, _Ix, _Decr) ->
    erlang:nif_error(undefined).

-spec sub_get(Ref :: atomics_ref(), Ix :: pos_integer(), Decr :: integer()) -> integer().
sub_get(_Ref, _Ix, _Decr) ->
    erlang:nif_error(undefined).

-spec exchange(Ref :: atomics_ref(), Ix :: pos_integer(), Desired :: integer()) -> integer().
exchange(_Ref, _Ix, _Desired) ->
    erlang:nif_error(undefined).

-spec compare_exchange(
    Ref :: atomics_ref(),
    Ix :: pos_integer(),
    Expected :: integer(),
    Desired :: integer()
) -> ok | integer().
compare_exchange(_Ref, _Ix, _Expected, _Desired) ->
    erlang:nif_error(undefined).

-spec info(Ref :: atomics_ref()) ->
    #{
        size := non_neg_integer(),
        max := integer(),
        min := integer(),
        memory := non_neg_integer()
    }.
info(_Ref) ->
    erlang:nif_error(undefined).
