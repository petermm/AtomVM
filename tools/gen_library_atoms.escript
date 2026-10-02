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

-define(MAX_ATOM_LEN, 4095).

main([Archive, Generic, Platform, Output]) ->
    generate_archive(Archive, Generic, Platform, Output, module_names);
main(["--all-atoms", Archive, Generic, Platform, Output]) ->
    generate_archive(Archive, Generic, Platform, Output, all_atoms);
main(_) ->
    io:format(
        standard_error,
        "usage: gen_library_atoms.escript [--all-atoms] <archive.avm> <generic.def> <platform.def> <output.h>~n",
        []
    ),
    halt(2).

generate_archive(Archive, Generic, Platform, Output, Mode) ->
    try
        Builtins = read_def(Generic) ++ read_def(Platform),
        Names = read_avm(Archive, Mode),
        Additional = lists:usort([Name || Name <- Names, not lists:member(Name, Builtins)]),
        check_lengths(Additional),
        Header = generate(length(Builtins), Additional),
        Changed = write_if_changed(Output, Header),
        io:format(
            "library atoms: ~B names, ~B text bytes (~s)~n",
            [length(Additional), lists:sum([byte_size(N) || N <- Additional]), Changed]
        )
    catch
        throw:{error, Message} ->
            io:format(standard_error, "gen_library_atoms: ~s~n", [Message]),
            halt(1);
        Class:Reason:Stack ->
            io:format(standard_error, "gen_library_atoms: ~p:~p~n~p~n", [Class, Reason, Stack]),
            halt(1)
    end.

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            Data;
        {error, Reason} ->
            throw({error, io_lib:format("cannot read ~s: ~s", [Path, file:format_error(Reason)])})
    end.

read_def(Path) ->
    Data = read_file(Path),
    try parse_def(binary_to_list(Data), []) of
        Names -> Names
    catch
        throw:{def_error, Why} ->
            throw({error, io_lib:format("malformed default atom file ~s: ~s", [Path, Why])})
    end.

parse_def([], Acc) ->
    lists:reverse(Acc);
parse_def([$/, $/ | Rest], Acc) ->
    parse_def(skip_line(Rest), Acc);
parse_def([$/, $* | Rest], Acc) ->
    parse_def(skip_comment(Rest), Acc);
parse_def([$X | Rest], Acc) ->
    case skip_space(Rest) of
        [$( | Tail] ->
            {Args, More} = take_call(Tail, [], [], false, false),
            case split_args(Args) of
                [_First, _Second, Third] -> parse_def(More, [decode_c_string(Third) | Acc]);
                _ -> throw({def_error, "X declaration does not have exactly three arguments"})
            end;
        _ ->
            parse_def(Rest, Acc)
    end;
parse_def([_ | Rest], Acc) ->
    parse_def(Rest, Acc).

skip_line([]) -> [];
skip_line([$\n | Rest]) -> Rest;
skip_line([_ | Rest]) -> skip_line(Rest).
skip_comment([$*, $/ | Rest]) -> Rest;
skip_comment([_ | Rest]) -> skip_comment(Rest);
skip_comment([]) -> throw({def_error, "unterminated comment"}).
skip_space([C | Rest]) when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n -> skip_space(Rest);
skip_space(Rest) -> Rest.

take_call([], _, _, _, _) ->
    throw({def_error, "unterminated X declaration"});
take_call([C | Rest], Cur, Args, InString, Escape) ->
    case {C, InString, Escape} of
        {_, true, true} -> take_call(Rest, [C | Cur], Args, true, false);
        {$\\, true, false} -> take_call(Rest, [C | Cur], Args, true, true);
        {$\", true, false} -> take_call(Rest, [C | Cur], Args, false, false);
        {$\", false, _} -> take_call(Rest, [C | Cur], Args, true, false);
        {$,, false, _} -> take_call(Rest, [], [lists:reverse(Cur) | Args], false, false);
        {$), false, _} -> {lists:reverse([lists:reverse(Cur) | Args]), Rest};
        _ -> take_call(Rest, [C | Cur], Args, InString, false)
    end.

split_args(Args) -> [string:trim(A) || A <- Args].

decode_c_string([$\" | Rest]) ->
    {Bytes, Tail} = c_chars(Rest, []),
    case skip_space(Tail) of
        [] -> list_to_binary(Bytes);
        _ -> throw({def_error, "unexpected text after third string literal"})
    end;
decode_c_string(_) ->
    throw({def_error, "third argument is not a C string literal"}).

c_chars([], _) ->
    throw({def_error, "unterminated C string literal"});
c_chars([$\" | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
c_chars([$\\, $x | Rest], Acc) ->
    {Digits, Tail} = take_hex(Rest, []),
    case Digits of
        [] -> throw({def_error, "empty hexadecimal escape"});
        _ -> c_chars(Tail, [list_to_integer(Digits, 16) band 16#FF | Acc])
    end;
c_chars([$\\, C | Rest], Acc) when C >= $0, C =< $7 ->
    {Digits, Tail} = take_octal(Rest, [C], 2),
    c_chars(Tail, [list_to_integer(Digits, 8) band 16#FF | Acc]);
c_chars([$\\, C | Rest], Acc) ->
    Value =
        case C of
            $n -> $\n;
            $r -> $\r;
            $t -> $\t;
            $b -> 8;
            $f -> 12;
            $v -> 11;
            $a -> 7;
            $\\ -> $\\;
            $\" -> $\";
            $? -> $?;
            _ -> throw({def_error, io_lib:format("unsupported C escape \\~c", [C])})
        end,
    c_chars(Rest, [Value | Acc]);
c_chars([C | Rest], Acc) when C =/= $\n, C =< 255 -> c_chars(Rest, [C | Acc]);
c_chars(_, _) ->
    throw({def_error, "non-byte or newline in C string literal"}).

take_hex([C | Rest], Acc) when C >= $0, C =< $9; C >= $a, C =< $f; C >= $A, C =< $F ->
    take_hex(Rest, [C | Acc]);
take_hex(Rest, Acc) ->
    {lists:reverse(Acc), Rest}.
take_octal([C | Rest], Acc, N) when N > 0, C >= $0, C =< $7 -> take_octal(Rest, [C | Acc], N - 1);
take_octal(Rest, Acc, _) -> {lists:reverse(Acc), Rest}.

read_avm(Path, Mode) ->
    case read_file(Path) of
        <<"#!/usr/bin/env AtomVM\n", 0, 0, Rest/binary>> -> parse_members(Rest, Path, 24, [], Mode);
        _ -> throw({error, io_lib:format("~s: invalid AVM v1 header", [Path])})
    end.

parse_members(<<0:32/big, Flags:32/big, Reserved:32/big, Tail/binary>>, Path, Offset, Acc, _) ->
    case {Flags, Reserved, Tail} of
        {0, 0, <<"end", 0>>} -> lists:append(lists:reverse(Acc));
        _ -> throw({error, io_lib:format("~s: malformed end marker at byte ~B", [Path, Offset])})
    end;
parse_members(
    <<Size:32/big, Flags:32/big, Reserved:32/big, _/binary>> = All, Path, Offset, Acc, Mode
) ->
    if
        Size < 16 ->
            throw(
                {error,
                    io_lib:format("~s: invalid member size ~B at byte ~B", [Path, Size, Offset])}
            );
        Size rem 4 =/= 0 ->
            throw({error, io_lib:format("~s: unaligned member size at byte ~B", [Path, Offset])});
        byte_size(All) < Size ->
            throw({error, io_lib:format("~s: truncated member at byte ~B", [Path, Offset])});
        Reserved =/= 0 ->
            throw({error, io_lib:format("~s: nonzero reserved field at byte ~B", [Path, Offset])});
        true ->
            <<Member:Size/binary, Rest/binary>> = All,
            <<_:12/binary, Body/binary>> = Member,
            {Name, Beam} = member_body(Body, Path, Offset),
            NewAcc =
                case Flags band 2 of
                    2 -> [beam_atoms(Beam, Path, Offset, Name, Mode) | Acc];
                    _ -> Acc
                end,
            parse_members(Rest, Path, Offset + Size, NewAcc, Mode)
    end;
parse_members(_, Path, Offset, _, _) ->
    throw({error, io_lib:format("~s: truncated member header at byte ~B", [Path, Offset])}).

member_body(Body, Path, Offset) ->
    case binary:match(Body, <<0>>) of
        nomatch ->
            throw(
                {error, io_lib:format("~s: unterminated member name at byte ~B", [Path, Offset])}
            );
        {N, 1} ->
            Pad = (N + 1 + 3) band bnot 3,
            if
                byte_size(Body) < Pad ->
                    throw(
                        {error,
                            io_lib:format("~s: truncated member name at byte ~B", [Path, Offset])}
                    );
                true ->
                    <<Name:N/binary, 0, Padding:(Pad - N - 1)/binary, Beam/binary>> = Body,
                    case Padding of
                        <<0:(byte_size(Padding))/unit:8>> ->
                            {Name, Beam};
                        _ ->
                            throw(
                                {error,
                                    io_lib:format("~s: nonzero member-name padding at byte ~B", [
                                        Path, Offset
                                    ])}
                            )
                    end
            end
    end.

beam_atoms(Beam, Path, Offset, MemberName, Mode) ->
    try
        case beam_lib:chunks(Beam, [atoms]) of
            {ok, {Module, [{atoms, [{1, Module} | _] = Atoms}]}} when is_atom(Module) ->
                Names =
                    case Mode of
                        module_names -> [Module];
                        all_atoms -> [Atom || {_, Atom} <- Atoms]
                    end,
                [atom_to_binary(Atom, utf8) || Atom <- Names];
            {error, beam_lib, Why} ->
                erlang:error(Why);
            _ ->
                erlang:error(missing_module_name)
        end
    catch
        Class:Reason ->
            throw(
                {error,
                    io_lib:format(
                        "~s: cannot extract atoms from BEAM member ~ts at byte ~B "
                        "(~p:~0P; generator OTP ~s). Use the archive-producing OTP toolchain "
                        "or a compatible newer release.",
                        [
                            Path,
                            MemberName,
                            Offset,
                            Class,
                            Reason,
                            6,
                            erlang:system_info(otp_release)
                        ]
                    )}
            )
    end.

check_lengths(Names) ->
    case [N || N <- Names, byte_size(N) > ?MAX_ATOM_LEN] of
        [] ->
            ok;
        [N | _] ->
            throw(
                {error,
                    io_lib:format(
                        "atom is ~B bytes; maximum is ~B: ~tp",
                        [byte_size(N), ?MAX_ATOM_LEN, N]
                    )}
            )
    end.

generate(BuiltinCount, Names) ->
    Lengths = [byte_size(N) || N <- Names],
    MaxLen =
        case Lengths of
            [] -> 0;
            _ -> lists:max(Lengths)
        end,
    Data = iolist_to_binary(Names),
    Offsets = offsets(Lengths, 0, [0]),
    Indexed = lists:zip(lists:seq(0, length(Names) - 1), Names),
    %% Names are already byte-sorted; filtering preserves each length group's order.
    ByLen = lists:append([
        [I || {I, N} <- Indexed, byte_size(N) =:= L]
     || L <- lists:seq(0, MaxLen)
    ]),
    LenOffsets = offsets(
        [length([ok || N <- Names, byte_size(N) =:= L]) || L <- lists:seq(0, MaxLen)], 0, [0]
    ),
    [
        "/* Generated by tools/gen_library_atoms.escript.  Do not edit. */\n",
        "/* SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later */\n",
        "#ifndef ATOMVM_LIBRARY_ATOMS_H\n#define ATOMVM_LIBRARY_ATOMS_H\n\n#include <stdint.h>\n\n",
        io_lib:format(
            "#define LIBRARY_ATOMS_COUNT ~B\n#define LIBRARY_ATOMS_BUILTIN_COUNT ~B\n#define LIBRARY_ATOMS_MAX_LEN ~B\n\n",
            [length(Names), BuiltinCount, MaxLen]
        ),
        c_array("uint8_t", "library_atoms_data", binary_to_list(Data)),
        c_array("uint32_t", "library_atoms_offsets", Offsets),
        c_array("uint32_t", "library_atoms_by_len", ByLen),
        c_array("uint32_t", "library_atoms_len_offsets", LenOffsets),
        "\n#endif\n"
    ].

offsets([], _, Acc) -> lists:reverse(Acc);
offsets([N | Rest], Sum, Acc) -> offsets(Rest, Sum + N, [Sum + N | Acc]).
c_array(Type, Name, []) ->
    io_lib:format("static const ~s ~s[1] = { 0 };\n", [Type, Name]);
c_array(Type, Name, Values) ->
    Text = lists:join(", ", [integer_to_list(V) || V <- Values]),
    io_lib:format("static const ~s ~s[~B] = { ~s };\n", [Type, Name, length(Values), Text]).

write_if_changed(Path, Content) ->
    Binary = iolist_to_binary(Content),
    case file:read_file(Path) of
        {ok, Binary} ->
            "unchanged";
        _ ->
            case file:write_file(Path, Binary) of
                ok ->
                    "updated";
                {error, Reason} ->
                    throw(
                        {error,
                            io_lib:format("cannot write ~s: ~s", [Path, file:format_error(Reason)])}
                    )
            end
    end.
