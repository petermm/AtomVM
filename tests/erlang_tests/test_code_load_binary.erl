%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(test_code_load_binary).

-export([start/0]).

-include("code_load/export_test_module_data.hrl").

export_test_module_data() ->
    ?EXPORT_TEST_MODULE_DATA.

start() ->
    Bin = export_test_module_data(),
    {error, _} = code:ensure_loaded(export_test_module),
    {module, export_test_module} = code:load_binary(
        export_test_module, "export_test_module.beam", Bin
    ),
    {module, export_test_module} = code:ensure_loaded(export_test_module),
    ok = test_literal_tables(),
    export_test_module:exported_func(4).

-ifdef(AVM_DISABLE_JIT).
% These are AtomVM's uncompressed literal chunk formats, not OTP's LitT format.
% Use tiny interpreter modules so offsets and empty tables are under test control.
test_literal_tables() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            ok;
        _ ->
            Marker = term_to_binary({literal_marker, 42}),
            % Count + two size headers + the first ETF binary's six-byte header.
            BoundaryPadding = 65535 - 18 - byte_size(Marker),
            ok = check_literals(metadata_litu_small, <<"LitU">>, 7, Marker),
            ok = check_literals(metadata_litu_boundary, <<"LitU">>, BoundaryPadding, Marker),
            ok = check_literals(metadata_litu_above, <<"LitU">>, BoundaryPadding + 1, Marker),
            % The second entry itself must start beyond the 16-bit range.
            ok = check_literals(metadata_litu_large, <<"LitU">>, 65536, Marker),
            ok = check_literals(metadata_litt_large, <<"LitT">>, 65536, Marker),
            ok = check_empty_literals(metadata_literals_empty, beam_chunk(<<"LitU">>, <<0:32>>)),
            ok = check_empty_literals(metadata_literals_absent, <<>>)
    end.

check_literals(Module, Tag, Padding, Marker) ->
    Value = binary:copy(<<16#A5>>, Padding),
    First = term_to_binary(Value),
    Data = <<2:32, (byte_size(First)):32, First/binary, (byte_size(Marker)):32, Marker/binary>>,
    Payload =
        case Tag of
            <<"LitT">> -> <<0:32, Data/binary>>;
            <<"LitU">> -> Data
        end,
    Bin = literal_module(Module, beam_chunk(Tag, Payload), true),
    {module, Module} = code:load_binary(Module, "metadata_literals.beam", Bin),
    Value = Module:first(),
    {literal_marker, 42} = Module:second(),
    ok.

check_empty_literals(Module, Chunk) ->
    Bin = literal_module(Module, Chunk, false),
    {module, Module} = code:load_binary(Module, "metadata_literals.beam", Bin),
    42 = Module:first(),
    42 = Module:second(),
    ok.

literal_module(Module, Literals, HasLiterals) ->
    Name = atom_to_binary(Module, utf8),
    Atoms = <<3:32, (byte_size(Name)):8, Name/binary, 5, "first", 6, "second">>,
    {First, Second} =
        case HasLiterals of
            true -> {<<16#47, 0>>, <<16#47, 16#10>>};
            false -> {<<16#09, 42>>, <<16#09, 42>>}
        end,
    % label 1; func_info module:first/0; label 2; move source,x0; return;
    % label 3; func_info module:second/0; label 4; move source,x0; return; end.
    Code =
        <<16:32, 0:32, 64:32, 5:32, 2:32, 1, 16#10, 2, 16#12, 16#22, 0, 1, 16#20, 64, First/binary,
            3, 19, 1, 16#30, 2, 16#12, 16#32, 0, 1, 16#40, 64, Second/binary, 3, 19, 3>>,
    Chunks = <<
        (beam_chunk(<<"AtU8">>, Atoms))/binary,
        (beam_chunk(<<"Code">>, Code))/binary,
        (beam_chunk(<<"ImpT">>, <<0:32>>))/binary,
        (beam_chunk(<<"ExpT">>, <<2:32, 2:32, 0:32, 2:32, 3:32, 0:32, 4:32>>))/binary,
        (beam_chunk(<<"LocT">>, <<0:32>>))/binary,
        Literals/binary
    >>,
    <<"FOR1", (byte_size(Chunks) + 4):32, "BEAM", Chunks/binary>>.

beam_chunk(Tag, Payload) ->
    Size = byte_size(Payload),
    PadBits = ((4 - Size rem 4) rem 4) * 8,
    <<Tag/binary, Size:32, Payload/binary, 0:PadBits>>.
-else.
test_literal_tables() -> ok.
-endif.
