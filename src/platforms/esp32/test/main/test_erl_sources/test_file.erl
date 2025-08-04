%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_file).

-export([start/0]).

start() ->
    io:format("Starting file tests on SD card~n"),
    io:format("Running basic file test...~n"),
    ok = test_basic_file(),
    io:format("Basic file test passed~n"),
    io:format("Running garbage collection test...~n"),
    ok = test_gc(),
    io:format("Garbage collection test passed~n"),
    io:format("All file tests completed successfully~n"),
    ok.

% esp-idf limitations:
% filenames should be up to 8+3
% O_CREAT | O_WRONLY doesn't work in v4.4.4 (https://github.com/espressif/esp-idf/issues/1817)
% files are not selectable

test_basic_file() ->
    Path = "/sdcard/atomvm-1.txt",
    io:format("Testing basic file operations on: ~s~n", [Path]),
    
    io:format("Opening file for writing...~n"),
    {ok, Fd} = atomvm:posix_open(Path, [o_wronly, o_creat, o_excl], 8#644),
    io:format("File opened successfully, fd=~p~n", [Fd]),
    
    io:format("Writing initial data...~n"),
    {ok, 5} = atomvm:posix_write(Fd, <<"Hello">>),
    io:format("Data written successfully~n"),
    
    io:format("Closing file...~n"),
    ok = atomvm:posix_close(Fd),
    io:format("File closed successfully~n"),
    
    io:format("Reopening file for reading/writing...~n"),
    {ok, Fd2} = atomvm:posix_open(Path, [o_rdwr]),
    io:format("File reopened successfully, fd=~p~n", [Fd2]),
    
    io:format("Reading first 2 bytes...~n"),
    {ok, <<"He">>} = atomvm:posix_read(Fd2, 2),
    io:format("Read successful: <<\"He\">>~n"),
    
    io:format("Reading remaining bytes...~n"),
    {ok, <<"llo">>} = atomvm:posix_read(Fd2, 10),
    io:format("Read successful: <<\"llo\">>~n"),
    
    io:format("Checking EOF...~n"),
    eof = atomvm:posix_read(Fd2, 10),
    io:format("EOF reached as expected~n"),
    
    io:format("Appending more data...~n"),
    {ok, 6} = atomvm:posix_write(Fd2, <<" World">>),
    io:format("Additional data written successfully~n"),
    
    io:format("Checking EOF again...~n"),
    eof = atomvm:posix_read(Fd2, 10),
    io:format("EOF confirmed~n"),
    
    io:format("Closing file...~n"),
    ok = atomvm:posix_close(Fd2),
    io:format("File closed successfully~n"),
    
    io:format("Deleting test file...~n"),
    ok = atomvm:posix_unlink(Path),
    io:format("File deleted successfully~n"),
    
    ok.

% Test is based on the fact that `erlang:memory(binary)` count resources.
test_gc() ->
    Path = "/sdcard/atomvm-2.txt",
    io:format("Starting garbage collection test on: ~s~n", [Path]),
    
    io:format("Spawning GC test process...~n"),
    GCSubPid = spawn(fun() -> gc_loop(Path, undefined) end),
    
    io:format("Measuring initial memory...~n"),
    MemorySize0 = erlang:memory(binary),
    io:format("Initial memory size: ~p bytes~n", [MemorySize0]),
    
    io:format("Opening file...~n"),
    call_gc_loop(GCSubPid, open),
    MemorySize1 = erlang:memory(binary),
    io:format("Memory after open: ~p bytes~n", [MemorySize1]),
    true = MemorySize1 > MemorySize0,
    
    io:format("Closing file...~n"),
    call_gc_loop(GCSubPid, close),
    io:format("Triggering garbage collection...~n"),
    call_gc_loop(GCSubPid, gc),
    MemorySize2 = erlang:memory(binary),
    io:format("Memory after GC: ~p bytes~n", [MemorySize2]),
    true = MemorySize2 =:= MemorySize0,

    io:format("Reopening file...~n"),
    call_gc_loop(GCSubPid, open),
    MemorySize3 = erlang:memory(binary),
    io:format("Memory after reopen: ~p bytes~n", [MemorySize3]),
    true = MemorySize3 =:= MemorySize1,
    
    io:format("Testing multiple opens...~n"),
    call_gc_loop(GCSubPid, open),
    call_gc_loop(GCSubPid, gc),
    MemorySize4 = erlang:memory(binary),
    io:format("Memory after multiple opens: ~p bytes~n", [MemorySize4]),
    true = MemorySize4 =:= MemorySize1,
    
    io:format("Forgetting reference and GC...~n"),
    call_gc_loop(GCSubPid, forget),
    call_gc_loop(GCSubPid, gc),
    MemorySize5 = erlang:memory(binary),
    io:format("Final memory size: ~p bytes~n", [MemorySize5]),
    true = MemorySize5 =:= MemorySize0,

    io:format("Cleaning up GC test...~n"),
    call_gc_loop(GCSubPid, quit),
    ok = atomvm:posix_unlink(Path),
    io:format("GC test completed successfully~n"),
    ok.

call_gc_loop(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {Pid, Message} -> ok
    end.

gc_loop(Path, File) ->
    receive
        {Caller, open} ->
            io:format("GC: Opening file ~s~n", [Path]),
            {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat, o_append], 8#644),
            io:format("GC: File opened, fd=~p~n", [Fd]),
            Caller ! {self(), open},
            gc_loop(Path, Fd);
        {Caller, forget} ->
            io:format("GC: Forgetting file reference~n"),
            Caller ! {self(), forget},
            gc_loop(Path, undefined);
        {Caller, gc} ->
            io:format("GC: Triggering garbage collection~n"),
            erlang:garbage_collect(),
            Caller ! {self(), gc},
            gc_loop(Path, File);
        {Caller, close} ->
            io:format("GC: Closing file, fd=~p~n", [File]),
            atomvm:posix_close(File),
            Caller ! {self(), close},
            gc_loop(Path, undefined);
        {Caller, quit} ->
            io:format("GC: Quitting GC loop~n"),
            Caller ! {self(), quit}
    end.
