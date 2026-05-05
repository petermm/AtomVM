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
%% @doc A limited implementation of the Erlang/OTP `counters' module.
%%      Atomics-backed counters are supported. The `write_concurrency' option is
%%      not implemented.
%% @end
%%-----------------------------------------------------------------------------
-module(counters).

-export([
    new/2,
    get/2,
    add/3,
    sub/3,
    put/3,
    info/1
]).

-export_type([counters_ref/0]).

-opaque counters_ref() :: {atomics, atomics:atomics_ref()}.

-type option() :: atomics.

-spec new(Size :: pos_integer(), Opts :: [option()]) -> counters_ref().
new(Size, []) ->
    {atomics, atomics:new(Size, [{signed, true}])};
new(Size, [atomics]) ->
    {atomics, atomics:new(Size, [{signed, true}])};
new(_Size, _Opts) ->
    erlang:error(badarg).

-spec get(Ref :: counters_ref(), Ix :: pos_integer()) -> integer().
get({atomics, Ref}, Ix) ->
    atomics:get(Ref, Ix);
get(_Ref, _Ix) ->
    erlang:error(badarg).

-spec add(Ref :: counters_ref(), Ix :: pos_integer(), Incr :: integer()) -> ok.
add({atomics, Ref}, Ix, Incr) ->
    atomics:add(Ref, Ix, Incr);
add(_Ref, _Ix, _Incr) ->
    erlang:error(badarg).

-spec sub(Ref :: counters_ref(), Ix :: pos_integer(), Decr :: integer()) -> ok.
sub(Ref, Ix, Decr) ->
    Incr = -Decr,
    case Ref of
        {atomics, AtomicRef} ->
            atomics:add(AtomicRef, Ix, Incr);
        _ ->
            erlang:error(badarg)
    end.

-spec put(Ref :: counters_ref(), Ix :: pos_integer(), Value :: integer()) -> ok.
put({atomics, Ref}, Ix, Value) ->
    atomics:put(Ref, Ix, Value);
put(_Ref, _Ix, _Value) ->
    erlang:error(badarg).

-spec info(Ref :: counters_ref()) ->
    #{
        size := non_neg_integer(),
        memory := non_neg_integer()
    }.
info({atomics, Ref}) ->
    atomics:info(Ref);
info(_Ref) ->
    erlang:error(badarg).
