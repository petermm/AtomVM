#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M <petermm@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

cmake_minimum_required(VERSION 3.16)

project(AtomVM_OpenC6 C)

# Path to the openC6 BIOS source tree (must be provided at configure time:
#   cmake -B build -DOPENC6_BIOS_PATH=/path/to/openc6-bios )
set(OPENC6_BIOS_PATH "" CACHE PATH "Path to openC6 BIOS source code")
if(NOT OPENC6_BIOS_PATH OR NOT EXISTS "${OPENC6_BIOS_PATH}/components/boot_manager/include/openc6_abi.h")
    message(FATAL_ERROR "OPENC6_BIOS_PATH is not set or invalid. Pass -DOPENC6_BIOS_PATH=/path/to/openc6-bios")
endif()

# Append CMakeModules path to locate DefineIfExists and other custom scripts
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules")

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Flags for rv32imac
add_compile_options(
    -march=rv32imac
    -mabi=ilp32
    -Os
    -ffunction-sections
    -fdata-sections
    -fno-pic
    -fno-PIE
    -fno-jump-tables
    -fno-tree-switch-conversion
    -nostdlib
    -Wall
    -Wextra
)

# Force unsupported features to off BEFORE adding libAtomVM subdirectory
foreach(sym
    HAVE_OPEN HAVE_CLOSE HAVE_PREAD HAVE_PWRITE HAVE_LSEEK HAVE_FSYNC
    HAVE_FTRUNCATE HAVE_MKDIR HAVE_MKFIFO HAVE_RMDIR HAVE_UNLINK
    HAVE_RENAME HAVE_STAT HAVE_FSTAT HAVE_OPENDIR HAVE_READDIR
    HAVE_CLOSEDIR HAVE_EXECVE HAVE_GETCWD HAVE_CLOCK_SETTIME
    HAVE_SETTIMEOFDAY HAVE_SELECT HAVE_SCHED_YIELD)
    set(${sym} "" CACHE INTERNAL "Unsupported on openC6 bare-metal" FORCE)
endforeach()

# AtomVM configuration options
set(AVM_DISABLE_SMP ON CACHE BOOL "Disable SMP support" FORCE)
set(AVM_USE_32BIT_FLOAT ON CACHE BOOL "Use 32-bit single precision floats" FORCE)
set(AVM_DISABLE_JIT ON CACHE BOOL "Disable JIT compiler" FORCE)
set(AVM_CREATE_STACKTRACES ON CACHE BOOL "Create stacktraces" FORCE)
set(AVM_DISABLE_TASK_DRIVER ON FORCE)

add_definitions(
    -DAVM_NO_SMP
    -DAVM_USE_SINGLE_PRECISION
    -DAVM_DISABLE_JIT
)

# Include directories
set(ABI_INCLUDE_DIR "${OPENC6_BIOS_PATH}/components/boot_manager/include")
include_directories(
    .
    ${ABI_INCLUDE_DIR}
)

# Mock standard math library target to satisfy libAtomVM linkage under -nostdlib
add_library(m INTERFACE)

# Core libAtomVM
add_subdirectory(../../libAtomVM libAtomVM)

# Linker script path
set(LINKER_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/payload.ld")

# Query libgcc path
execute_process(
    COMMAND ${CMAKE_C_COMPILER} -print-libgcc-file-name
    OUTPUT_VARIABLE LIBGCC_FILE
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Query libm path (toolchain's math library)
execute_process(
    COMMAND ${CMAKE_C_COMPILER} -print-file-name=libm.a
    OUTPUT_VARIABLE LIBM_FILE
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Query libc path (toolchain's C library)
execute_process(
    COMMAND ${CMAKE_C_COMPILER} -print-file-name=libc.a
    OUTPUT_VARIABLE LIBC_FILE
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Exclude standard libraries and specify linker script
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -no-pie -nostdlib -Wl,-T,${LINKER_SCRIPT} -Wl,--no-warn-rwx-segments -Wl,--gc-sections")

# Define target
add_executable(payload.elf
    main.c
    sys.c
    libc_stubs.c
    platform_nifs.c
    ../../libAtomVM/portnifloader.c
)

target_link_libraries(payload.elf PRIVATE libAtomVM "${LIBC_FILE}" "${LIBM_FILE}" "${LIBGCC_FILE}")

# Custom target to convert ELF to binary payload.bin
find_program(OBJCOPY riscv32-esp-elf-objcopy)
if(NOT OBJCOPY)
    message(FATAL_ERROR "riscv32-esp-elf-objcopy tool not found. Ensure toolchain is in PATH.")
endif()

add_custom_command(TARGET payload.elf POST_BUILD
    COMMAND ${OBJCOPY} -O binary $<TARGET_FILE:payload.elf> ${CMAKE_CURRENT_BINARY_DIR}/payload.bin
    COMMENT "Generating flat payload.bin for openC6 BIOS..."
)

# Compute relative path from build directory to openC6 BIOS directory
file(RELATIVE_PATH OPENC6_BIOS_RELATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}" "${OPENC6_BIOS_PATH}")

# Configure Wokwi simulation files in the build directory
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/wokwi/wokwi.toml.in"
    "${CMAKE_CURRENT_BINARY_DIR}/wokwi.toml"
    @ONLY
)

configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/wokwi/flasher_args.json.in"
    "${CMAKE_CURRENT_BINARY_DIR}/flasher_args.json"
    @ONLY
)

configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/wokwi/diagram.json"
    "${CMAKE_CURRENT_BINARY_DIR}/diagram.json"
    COPYONLY
)
