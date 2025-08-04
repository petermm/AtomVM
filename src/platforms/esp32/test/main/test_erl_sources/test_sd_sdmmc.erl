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

-module(test_sd_sdmmc).

-export([start/0]).

-import(esp, [mount/4, umount/1]).

start() ->
    % Mount SD card using SDMMC interface
    case mount_sd_sdmmc() of
        {ok, MountedRef} ->
            % Run the test_file module tests on the mounted SD card
            Result = test_file:start(),
            % Unmount the SD card
            ok = unmount_sd_sdmmc(MountedRef),
            Result;
        Error ->
            Error
    end.

mount_sd_sdmmc() ->
    % Mount SD card using SDMMC
    MountOpts = [
        {format_if_mount_failed, true},
        {max_files, 5},
        {allocation_unit_size, 16 * 1024}
    ],
    
    case esp:mount("sdmmc", "/sdcard", fat, MountOpts) of
        {ok, MountedRef} ->
            io:format("SDMMC mount successful~n"),
            {ok, MountedRef};
        Error ->
            io:format("SDMMC mount failed: ~p~n", [Error]),
            Error
    end.

unmount_sd_sdmmc(MountedRef) ->
    % Unmount the SD card
    case esp:umount(MountedRef) of
        ok ->
            io:format("SDMMC unmount successful~n"),
            ok;
        Error ->
            io:format("SDMMC unmount failed: ~p~n", [Error]),
            Error
    end.