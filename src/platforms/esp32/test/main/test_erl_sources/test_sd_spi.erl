%
% This file is part of AtomVM.
%
% Copyright 2024 AtomVM Contributors
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

-module(test_sd_spi).

-export([start/0]).

-import(spi, [open/1]).
-import(esp, [mount/4, umount/1]).

start() ->
    io:format("=== Starting SDSPI Test ===~n"),
    % Ensure output is flushed
    timer:sleep(100),
    
    try
        % Mount SD card using SDSPI interface
        case mount_sd_spi() of
            {ok, MountedRef} ->
                io:format("SDSPI mount successful, proceeding with file tests~n"),
                timer:sleep(100),
                
                try
                    % Run the test_file module tests on the mounted SD card
                    Result = test_file:start(),
                    io:format("File tests completed with result: ~p~n", [Result]),
                    timer:sleep(100),
                    
                    % Check if the file test result is ok
                    case Result of
                        ok ->
                            io:format("File tests passed, proceeding to unmount~n"),
                            timer:sleep(100);
                        _ ->
                            io:format("File tests failed with result: ~p~n", [Result]),
                            timer:sleep(100)
                    end,
                    
                    % Unmount the SD card
                    case unmount_sd_spi(MountedRef) of
                        ok ->
                            io:format("SDSPI test completed successfully~n"),
                            timer:sleep(100),
                            % Return ok only if file tests also passed
                            case Result of
                                ok -> ok;
                                _ -> {error, {file_tests_failed, Result}}
                            end;
                        Error ->
                            io:format("Failed to unmount SD card: ~p~n", [Error]),
                            timer:sleep(100),
                            {error, {unmount_failed, Error}}
                    end
                catch
                    Class:Reason:Stacktrace ->
                        io:format("Exception during file tests: ~p:~p~n", [Class, Reason]),
                        io:format("Stacktrace: ~p~n", [Stacktrace]),
                        timer:sleep(100),
                        % Try to unmount even after exception
                        try unmount_sd_spi(MountedRef) catch _:_ -> ok end,
                        {error, {file_test_exception, Class, Reason}}
                end;
            Error ->
                io:format("SDSPI test failed during mount: ~p~n", [Error]),
                timer:sleep(100),
                {error, {mount_failed, Error}}
        end
    catch
        Class:Reason:Stacktrace ->
            io:format("Exception during SDSPI test: ~p:~p~n", [Class, Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            timer:sleep(100),
            {error, {test_exception, Class, Reason}}
    end.

mount_sd_spi() ->
    io:format("Starting SDSPI mount process~n"),
    timer:sleep(100),
    
    try
        % SPI configuration matching the test_main.c configuration
        SPIConfig = [
            {bus_config, [
                {mosi, 23},
                {miso, 19},
                {sclk, 18},
                {quadwp, -1},
                {quadhd, -1},
                {max_transfer_sz, 4000},
                {peripheral, "spi2"}
            ]}
        ],
        io:format("SPI configuration: ~p~n", [SPIConfig]),
        timer:sleep(100),
        
        io:format("About to call spi:open/1~n"),
        timer:sleep(100),
        
        % Open SPI bus
        case spi:open(SPIConfig) of
            {ok, SPI} ->
                io:format("SPI bus opened successfully, handle=~p~n", [SPI]),
                timer:sleep(100),
                
                % Mount SD card using SDSPI
                MountOpts = [
                    {spi_host, SPI},
                    {cs, 5}
                ],
                io:format("Mount options: ~p~n", [MountOpts]),
                timer:sleep(100),
                
                io:format("About to call esp:mount/4~n"),
                timer:sleep(100),
                
                case esp:mount("sdspi", "/sdcard", fat, MountOpts) of
                    {ok, MountedRef} ->
                        io:format("SDSPI mount successful, ref=~p~n", [MountedRef]),
                        timer:sleep(100),
                        {ok, MountedRef};
                    {error, Reason} ->
                        io:format("SDSPI mount failed with error: ~p~n", [Reason]),
                        timer:sleep(100),
                        {error, {mount_error, Reason}};
                    Error ->
                        io:format("SDSPI mount failed with unexpected result: ~p~n", [Error]),
                        timer:sleep(100),
                        {error, {mount_unexpected, Error}}
                end;
            {error, Reason} ->
                io:format("SPI bus initialization failed with error: ~p~n", [Reason]),
                timer:sleep(100),
                {error, {spi_open_error, Reason}};
            Error ->
                io:format("SPI bus initialization failed with unexpected result: ~p~n", [Error]),
                timer:sleep(100),
                {error, {spi_open_unexpected, Error}}
        end
    catch
        Class:Reason:Stacktrace ->
            io:format("Exception in mount_sd_spi: ~p:~p~n", [Class, Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            timer:sleep(100),
            {error, {mount_exception, Class, Reason}}
    end.

unmount_sd_spi(MountedRef) ->
    % Unmount the SD card
    io:format("Starting SDSPI unmount process~n"),
    timer:sleep(100),
    case esp:umount(MountedRef) of
        ok ->
            io:format("SDSPI unmount successful~n"),
            timer:sleep(100),
            ok;
        Error ->
            io:format("SDSPI unmount failed: ~p~n", [Error]),
            timer:sleep(100),
            Error
    end.