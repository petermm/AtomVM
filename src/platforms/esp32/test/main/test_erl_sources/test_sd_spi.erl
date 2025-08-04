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
    % Mount SD card using SDSPI interface
    case mount_sd_spi() of
        {ok, MountedRef} ->
            io:format("SDSPI mount successful, proceeding with file tests~n"),
            % Run the test_file module tests on the mounted SD card
            Result = test_file:start(),
            io:format("File tests completed with result: ~p~n", [Result]),
            % Unmount the SD card
            case unmount_sd_spi(MountedRef) of
                ok ->
                    io:format("SDSPI test completed successfully~n"),
                    Result;
                Error ->
                    io:format("Failed to unmount SD card: ~p~n", [Error]),
                    Error
            end;
        Error ->
            io:format("SDSPI test failed during mount: ~p~n", [Error]),
            Error
    end.

mount_sd_spi() ->
    io:format("Starting SDSPI mount process~n"),
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
    
    % Open SPI bus
    case spi:open(SPIConfig) of
        {ok, SPI} ->
            io:format("SPI bus opened successfully, handle=~p~n", [SPI]),
            % Mount SD card using SDSPI
            MountOpts = [
                {spi_host, SPI},
                {cs, 5}
            ],
            io:format("Mount options: ~p~n", [MountOpts]),
            
            case esp:mount("sdspi", "/sdcard", fat, MountOpts) of
                {ok, MountedRef} ->
                    io:format("SDSPI mount successful, ref=~p~n", [MountedRef]),
                    {ok, MountedRef};
                Error ->
                    io:format("SDSPI mount failed with error: ~p~n", [Error]),
                    Error
            end;
        Error ->
            io:format("SPI bus initialization failed with error: ~p~n", [Error]),
            Error
    end.

unmount_sd_spi(MountedRef) ->
    % Unmount the SD card
    case esp:umount(MountedRef) of
        ok ->
            io:format("SDSPI unmount successful~n"),
            ok;
        Error ->
            io:format("SDSPI unmount failed: ~p~n", [Error]),
            Error
    end.