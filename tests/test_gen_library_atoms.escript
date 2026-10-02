#!/usr/bin/env escript
%%
%% This file is part of AtomVM.
%%
%% Copyright 2026 AtomVM contributors
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

-mode(compile).

-include_lib("kernel/include/file.hrl").

main(Args) ->
    {ok, Cwd} = file:get_cwd(),
    Generator = filename:join(Cwd, "tools/gen_library_atoms.escript"),
    Tmp = filename:join([
        os:getenv("TMPDIR", "/tmp"),
        "atomvm-library-atoms-" ++ os:getpid() ++ "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ]),
    ok = file:make_dir(Tmp),
    try
        run_tests(Generator, Tmp),
        case Args of
            [] -> ok;
            ["--build", BuildGenerator] -> run_build_tests(Cwd, Tmp, BuildGenerator)
        end,
        io:format("gen_library_atoms tests passed~n")
    after
        remove_tree(Tmp)
    end.

run_tests(Generator, Tmp) ->
    Generic = path(Tmp, "generic.def"),
    Platform = path(Tmp, "platform.def"),
    ok = file:write_file(
        Generic,
        "// parser exercise\nX(A, \"\\x3\", \"dup\")\n"
        "X(B, \"\\x3\", \"q\\\"x\") /* comment */\n"
    ),
    ok = file:write_file(Platform, "X(C, \"\\4\", \"back\\\\slash\")\n"),
    Special = [
        <<"z">>,
        <<"aa">>,
        <<"dup">>,
        <<"q\"x">>,
        <<"back\\slash">>,
        unicode:characters_to_binary("mód"),
        <<"nul", 0, "name">>
    ],
    Beams = [{N, make_beam(N)} || N <- Special],
    Archive = path(Tmp, "input.avm"),
    %% Names intentionally lie; both an additional and a builtin are duplicated.
    write_avm(
        Archive,
        [{<<"not-the-module.beam">>, B} || {_, B} <- lists:reverse(Beams)] ++
            [
                {<<"also-not-z.beam">>, proplists:get_value(<<"z">>, Beams)},
                {<<"also-not-dup.beam">>, proplists:get_value(<<"dup">>, Beams)}
            ]
    ),
    Output = path(Tmp, "generated.h"),
    {0, FirstLog} = command(Generator, [Archive, Generic, Platform, Output]),
    assert(contains(FirstLog, <<"4 names">>), count_report),
    {ok, Header} = file:read_file(Output),
    Expected = lists:sort(Special -- [<<"dup">>, <<"q\"x">>, <<"back\\slash">>]),
    assert(macro(Header, "LIBRARY_ATOMS_COUNT") =:= 4, count),
    assert(macro(Header, "LIBRARY_ATOMS_BUILTIN_COUNT") =:= 3, builtin_count),
    assert(
        array(Header, "library_atoms_data") =:= binary_to_list(iolist_to_binary(Expected)), data
    ),
    assert(array(Header, "library_atoms_offsets") =:= expected_offsets(Expected), offsets),
    %% z, aa, mód, nul\0name have lengths 1, 2, 4, 8 respectively.
    assert(array(Header, "library_atoms_by_len") =:= [3, 0, 1, 2], by_length),
    assert(
        array(Header, "library_atoms_len_offsets") =:= [0, 0, 1, 2, 2, 3, 3, 3, 3, 4],
        length_boundaries
    ),
    {ok, Info1} = file:read_file_info(Output),
    timer:sleep(1100),
    {0, SecondLog} = command(Generator, [Archive, Generic, Platform, Output]),
    {ok, Info2} = file:read_file_info(Output),
    assert(contains(SecondLog, <<"unchanged">>), unchanged_report),
    assert(Info1#file_info.mtime =:= Info2#file_info.mtime, unchanged_mtime),

    Reordered = path(Tmp, "reordered.avm"),
    write_avm(Reordered, [{<<"x">>, B} || {_, B} <- Beams]),
    {0, _} = command(Generator, [Reordered, Generic, Platform, Output]),
    {ok, Header} = file:read_file(Output),

    test_empty(Generator, Tmp),
    test_wide_indices(Generator, Tmp),
    test_failures(Generator, Tmp, Generic, Platform).

test_empty(Generator, Tmp) ->
    Def = path(Tmp, "all.def"),
    EmptyDef = path(Tmp, "empty.def"),
    ok = file:write_file(Def, "X(A, \"\\1\", \"only\")\n"),
    ok = file:write_file(EmptyDef, <<>>),
    Avm = path(Tmp, "all-default.avm"),
    write_avm(Avm, [{<<"wrong">>, make_beam(<<"only">>)}]),
    Out = path(Tmp, "empty.h"),
    {0, _} = command(Generator, [Avm, Def, EmptyDef, Out]),
    {ok, H} = file:read_file(Out),
    assert(macro(H, "LIBRARY_ATOMS_COUNT") =:= 0, empty_count),
    assert(array(H, "library_atoms_data") =:= [0], empty_data_storage),
    assert(array(H, "library_atoms_by_len") =:= [0], empty_index_storage),
    assert(array(H, "library_atoms_offsets") =:= [0], empty_offsets),
    assert(array(H, "library_atoms_len_offsets") =:= [0, 0], empty_len_offsets),
    C = path(Tmp, "empty.c"),
    ok = file:write_file(C, [
        "#include \"", Out, "\"\nint main(void) { return library_atoms_data[0]; }\n"
    ]),
    {0, _} = command(os:getenv("CC", "cc"), [
        "-std=c11", "-pedantic-errors", "-c", C, "-o", path(Tmp, "empty.o")
    ]).

test_wide_indices(Generator, Tmp) ->
    Names = [list_to_binary(io_lib:format("module_~3..0B", [I])) || I <- lists:seq(0, 269)],
    Avm = path(Tmp, "wide.avm"),
    write_avm(Avm, [{<<"misleading">>, make_beam(N)} || N <- lists:reverse(Names)]),
    Def = path(Tmp, "none.def"),
    ok = file:write_file(Def, <<>>),
    Out = path(Tmp, "wide.h"),
    {0, _} = command(Generator, [Avm, Def, Def, Out]),
    {ok, H} = file:read_file(Out),
    assert(lists:max(array(H, "library_atoms_by_len")) =:= 269, wide_metadata).

test_failures(Generator, Tmp, Generic, Platform) ->
    BadAvm = path(Tmp, "bad.avm"),
    ok = file:write_file(BadAvm, <<"bad">>),
    {Status1, Log1} = command(Generator, [BadAvm, Generic, Platform, path(Tmp, "bad.h")]),
    assert(Status1 =/= 0 andalso contains(Log1, <<"invalid AVM">>), malformed_container),
    BadBeam = path(Tmp, "bad-beam.avm"),
    write_avm(BadBeam, [{<<"fake.beam">>, <<0:96>>}]),
    {Status2, Log2} = command(Generator, [BadBeam, Generic, Platform, path(Tmp, "bad2.h")]),
    assert(
        Status2 =/= 0 andalso contains(Log2, <<"BEAM member fake.beam at byte 24">>) andalso
            contains(Log2, <<"not_a_beam_file">>) andalso
            contains(Log2, <<"generator OTP ">>) andalso
            contains(Log2, <<"compatible newer release">>),
        {malformed_beam, Status2, Log2}
    ),
    %% A valid IFF wrapper without an atom chunk must not intern "undefined".
    write_avm(BadBeam, [{<<"no-atoms.beam">>, <<"FOR1", 4:32/big, "BEAM">>}]),
    {StatusNoAtoms, LogNoAtoms} = command(Generator, [
        BadBeam, Generic, Platform, path(Tmp, "no-atoms.h")
    ]),
    assert(
        StatusNoAtoms =/= 0 andalso contains(LogNoAtoms, <<"missing_chunk">>),
        {missing_atoms, LogNoAtoms}
    ),
    BadDef = path(Tmp, "bad.def"),
    ok = file:write_file(BadDef, "X(BROKEN)\n"),
    {Status3, Log3} = command(Generator, [BadBeam, BadDef, Platform, path(Tmp, "bad3.h")]),
    assert(Status3 =/= 0 andalso contains(Log3, <<"malformed default">>), malformed_def).

run_build_tests(Root, Tmp, BuildGenerator) ->
    Archive = path(Tmp, "boot.avm"),
    OlderArchive = path(Tmp, "older-boot.avm"),
    write_avm(Archive, [{<<"module.beam">>, make_beam(<<"library_a">>)}]),
    write_avm(OlderArchive, [{<<"module.beam">>, make_beam(<<"library_b">>)}]),
    Build = path(Tmp, "build"),
    Opts = ["-DAVM_STATIC_LIBRARY_ATOMS=ON", "-DAVM_STATIC_LIBRARY_ATOMS_ARCHIVE=" ++ Archive],
    {0, _} = configure_build(Root, Build, BuildGenerator, Opts),
    Header = filename:join([Build, "src/platforms/generic_unix/libAtomVM/library_atoms.h"]),
    Stamp = filename:rootname(Header) ++ ".stamp",
    assert(contains(build_registry(Build), <<"updated">>), initial_generation),
    {ok, HeaderInfo} = file:read_file_info(Header),
    {ok, StampInfo} = file:read_file_info(Stamp),
    {ok, OlderInfo} = file:read_file_info(OlderArchive),
    assert(OlderInfo#file_info.mtime < StampInfo#file_info.mtime, older_archive_fixture),
    %% Code/archive contents change, but the module-name registry does not.
    timer:sleep(1100),
    write_avm(Archive, [{<<"renamed.beam">>, make_beam(<<"library_a">>)}]),
    assert(contains(build_registry(Build), <<"unchanged">>), unchanged_generation),
    {ok, HeaderInfo2} = file:read_file_info(Header),
    {ok, StampInfo2} = file:read_file_info(Stamp),
    assert(HeaderInfo#file_info.mtime =:= HeaderInfo2#file_info.mtime, stable_header_timestamp),
    assert(StampInfo2#file_info.mtime > StampInfo#file_info.mtime, updated_success_stamp),
    assert(not contains(build_registry(Build), <<"library atoms:">>), first_noop_build),
    assert(not contains(build_registry(Build), <<"library atoms:">>), second_noop_build),
    %% Changing to an older archive must invalidate the stamp too, even under Make.
    {0, _} = configure_build(
        Root,
        Build,
        BuildGenerator,
        ["-DAVM_STATIC_LIBRARY_ATOMS_ARCHIVE=" ++ OlderArchive]
    ),
    assert(contains(build_registry(Build), <<"updated">>), changed_archive_selection),
    {ok, SelectedHeader} = file:read_file(Header),
    assert(array(SelectedHeader, "library_atoms_data") =:= "library_b", selected_archive_contents),
    {0, _} = configure_build(Root, Build, BuildGenerator, ["-DAVM_STATIC_LIBRARY_ATOMS=OFF"]),
    {0, _} = configure_build(Root, Build, BuildGenerator, ["-DAVM_STATIC_LIBRARY_ATOMS=ON"]),
    assert(not contains(build_registry(Build), <<"library atoms:">>), reenabled_unchanged_registry),
    SelectionAlias = path(Tmp, "selection-alias.avm"),
    ok = file:make_symlink(Archive, SelectionAlias),
    {0, _} = configure_build(
        Root,
        Build,
        BuildGenerator,
        ["-DAVM_STATIC_LIBRARY_ATOMS_ARCHIVE=" ++ SelectionAlias]
    ),
    assert(contains(build_registry(Build), <<"updated">>), initial_alias_selection),
    ok = file:delete(SelectionAlias),
    ok = file:make_symlink(OlderArchive, SelectionAlias),
    {0, _} = configure_build(Root, Build, BuildGenerator, []),
    assert(contains(build_registry(Build), <<"updated">>), retargeted_alias_selection),
    {ok, SelectedHeader} = file:read_file(Header),
    Inside = path(Build, "inside.avm"),
    {ok, _} = file:copy(Archive, Inside),
    OutsideAlias = path(Tmp, "outside-alias.avm"),
    ok = file:make_symlink(Inside, OutsideAlias),
    {StatusAlias, LogAlias} = configure_build(
        Root,
        Build,
        BuildGenerator,
        ["-DAVM_STATIC_LIBRARY_ATOMS_ARCHIVE=" ++ OutsideAlias]
    ),
    assert(
        StatusAlias =/= 0 andalso contains(LogAlias, <<"dependency cycle">>),
        archive_symlink_rejected
    ),
    BuildAlias = path(Tmp, "build-alias"),
    ok = file:make_symlink(Build, BuildAlias),
    {StatusBuild, LogBuild} = configure_build(
        Root,
        BuildAlias,
        BuildGenerator,
        ["-DAVM_STATIC_LIBRARY_ATOMS_ARCHIVE=" ++ Inside]
    ),
    assert(
        StatusBuild =/= 0 andalso contains(LogBuild, <<"dependency cycle">>), build_symlink_rejected
    ),
    %% A similarly prefixed sibling is outside the build, and must be accepted.
    Sibling = path(Tmp, "build-sibling"),
    ok = file:make_dir(Sibling),
    SiblingArchive = path(Sibling, "outside.avm"),
    {ok, _} = file:copy(Archive, SiblingArchive),
    {0, _} = configure_build(
        Root,
        Build,
        BuildGenerator,
        ["-DAVM_STATIC_LIBRARY_ATOMS_ARCHIVE=" ++ SiblingArchive]
    ),
    assert(contains(build_registry(Build), <<"updated">>), sibling_archive_accepted),
    ok = file:delete(SiblingArchive),
    {StatusMissing, LogMissing} = configure_build(Root, Build, BuildGenerator, []),
    assert(
        StatusMissing =/= 0 andalso contains(LogMissing, <<"prebuilt boot archive">>),
        missing_archive_rejected
    ),
    %% Remove the directory alias before recursive temporary-directory cleanup.
    ok = file:delete(BuildAlias),
    io:format("library atom build tests passed (~s)~n", [BuildGenerator]).

configure_build(Root, Build, BuildGenerator, Opts) ->
    {Status, Log} = command("cmake", [
        "-S",
        Root,
        "-B",
        Build,
        "-G",
        BuildGenerator,
        "-DAVM_BUILD_RUNTIME_ONLY=ON",
        "-DAVM_DISABLE_JIT=ON"
        | Opts
    ]),
    case Status of
        0 -> ok;
        _ -> io:format(standard_error, "CMake configuration exited ~B:~n~ts~n", [Status, Log])
    end,
    {Status, Log}.

build_registry(Build) ->
    {Status, Log} = command("cmake", ["--build", Build, "--target", "libAtomVM_library_atoms"]),
    assert(Status =:= 0, {build_registry, Log}),
    Log.

make_beam(Name) ->
    CompileName = binary:replace(Name, <<0>>, <<"X">>, [global]),
    Module = binary_to_atom(CompileName, utf8),
    Forms = [
        {attribute, 1, module, Module},
        {attribute, 2, export, [{f, 0}]},
        {function, 3, f, 0, [{clause, 3, [], [], [{atom, 3, ok}]}]}
    ],
    {ok, Module, Beam} = compile:forms(Forms, [binary]),
    binary:replace(Beam, CompileName, Name, [global]).

write_avm(Path, Members) ->
    Header = <<"#!/usr/bin/env AtomVM\n", 0, 0>>,
    Encoded = [member(Name, Beam) || {Name, Beam} <- Members],
    %% Even a valid BEAM-looking asset must be ignored without BEAM_CODE_FLAG.
    <<Size:32/big, _:32, Body/binary>> = member(<<"asset.beam">>, make_beam(<<"not_a_module">>)),
    Asset = <<Size:32/big, 0:32, Body/binary>>,
    ok = file:write_file(Path, [Header, Encoded, Asset, <<0:32/big, 0:32, 0:32, "end", 0>>]).

member(Name, Beam) ->
    Name0 = <<Name/binary, 0>>,
    Padding = (4 - byte_size(Name0) rem 4) rem 4,
    Size = 12 + byte_size(Name0) + Padding + byte_size(Beam),
    %% BEAM files are four-byte aligned, hence the whole section is aligned.
    <<Size:32/big, 2:32/big, 0:32, Name0/binary, 0:Padding/unit:8, Beam/binary>>.

expected_offsets(Names) -> expected_offsets(Names, 0, [0]).
expected_offsets([], _, Acc) ->
    lists:reverse(Acc);
expected_offsets([N | Ns], Sum, Acc) ->
    expected_offsets(Ns, Sum + byte_size(N), [Sum + byte_size(N) | Acc]).

macro(Header, Name) ->
    Pattern = iolist_to_binary(["#define ", Name, " ([0-9]+)"]),
    {match, [Value]} = re:run(Header, Pattern, [{capture, [1], binary}]),
    binary_to_integer(Value).

array(Header, Name) ->
    Pattern = iolist_to_binary([Name, "\\[[0-9]+\\] = \\{ ([0-9, ]+) \\}"]),
    {match, [Values]} = re:run(Header, Pattern, [{capture, [1], binary}]),
    [binary_to_integer(V) || V <- binary:split(Values, <<", ">>, [global])].

command(Program, Args) ->
    Executable =
        case filename:pathtype(Program) of
            absolute -> Program;
            _ -> os:find_executable(Program)
        end,
    Port = open_port({spawn_executable, Executable}, [
        exit_status,
        stderr_to_stdout,
        binary,
        {args, Args}
    ]),
    collect(Port, <<>>).
collect(Port, Acc) ->
    receive
        {Port, {data, Data}} -> collect(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, Status}} -> {Status, Acc}
    end.

contains(Haystack, Needle) -> binary:match(Haystack, Needle) =/= nomatch.
assert(true, _) -> ok;
assert(false, What) -> erlang:error({assertion_failed, What}).
path(Dir, Name) -> filename:join(Dir, Name).

remove_tree(Path) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = directory}} ->
            {ok, Names} = file:list_dir(Path),
            lists:foreach(fun(Name) -> remove_tree(filename:join(Path, Name)) end, Names),
            file:del_dir(Path);
        _ ->
            file:delete(Path)
    end.
