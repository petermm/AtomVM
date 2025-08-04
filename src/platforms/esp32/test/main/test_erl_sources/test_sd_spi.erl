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
    % Mount SD card using SDSPI interface
    case mount_sd_spi() of
        {ok, MountedRef} ->
            % Run the test_file module tests on the mounted SD card
            Result = test_file:start(),
            % Unmount the SD card
            ok = unmount_sd_spi(MountedRef),
            Result;
        Error ->
            Error
    end.

mount_sd_spi() ->
    % SPI configuration matching the C code in test_main.c
    SPIConfig = [
        {bus_config, [
            {miso, 19},
            {mosi, 23},
            {sclk, 18},
            {peripheral, "spi2"}
        ]}
    ],
    
    % Open SPI bus
    case spi:open(SPIConfig) of
        {ok, SPI} ->
            % Mount SD card using SDSPI
            MountOpts = [
                {spi_host, SPI},
                {cs, 5},
                {format_if_mount_failed, true},
                {max_files, 5},
                {allocation_unit_size, 16 * 1024}
            ],
            
            case esp:mount("sdspi", "/sdcard", fat, MountOpts) of
                {ok, MountedRef} ->
                    io:format("SDSPI mount successful~n"),
                    {ok, MountedRef};
                Error ->
                    io:format("SDSPI mount failed: ~p~n", [Error]),
                    Error
            end;
        Error ->
            io:format("SPI bus initialization failed: ~p~n", [Error]),
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