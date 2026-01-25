#
# This file is part of AtomVM.
#
# Copyright 2019-2020 Riccardo Binetti <rbino@gmx.com>
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

# Only execute side effects once per CMake configure
if(NOT DEFINED _ATOMVM_ELIXIR_BUILD_INITIALIZED)
    set(_ATOMVM_ELIXIR_BUILD_INITIALIZED TRUE CACHE INTERNAL "BuildElixir.cmake initialized")

    # Create a job pool with 1 slot to serialize Elixir compilation
    set_property(GLOBAL APPEND PROPERTY JOB_POOLS elixir_pool=1)

    # Check Elixir version and set compiler options accordingly
    # infer_signatures was added in Elixir 1.18
    execute_process(
        COMMAND elixir --version
        OUTPUT_VARIABLE ELIXIR_VERSION_OUTPUT
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    string(REGEX MATCH "Elixir ([0-9]+)\\.([0-9]+)" ELIXIR_VERSION_MATCH "${ELIXIR_VERSION_OUTPUT}")
    set(ELIXIR_VERSION_MAJOR "${CMAKE_MATCH_1}" CACHE INTERNAL "Elixir major version")
    set(ELIXIR_VERSION_MINOR "${CMAKE_MATCH_2}" CACHE INTERNAL "Elixir minor version")

    if(ELIXIR_VERSION_MAJOR GREATER 1 OR (ELIXIR_VERSION_MAJOR EQUAL 1 AND ELIXIR_VERSION_MINOR GREATER_EQUAL 18))
        set(ELIXIRC_EXPR elixirc -e "Code.put_compiler_option(:infer_signatures, false)" CACHE INTERNAL "Elixir compiler command")
    else()
        set(ELIXIRC_EXPR elixirc CACHE INTERNAL "Elixir compiler command")
    endif()
endif()

# Macros are always defined (not guarded) so they're available after each include

macro(pack_archive avm_name)
    find_package(Elixir REQUIRED)

    foreach(module_name ${ARGN})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/beams/Elixir.${module_name}.beam
            COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/beams && ${ELIXIRC_EXPR} --no-docs --no-debug-info --ignore-module-conflict -o ${CMAKE_CURRENT_BINARY_DIR}/beams ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            COMMENT "Compiling ${module_name}.ex"
            JOB_POOL elixir_pool
            VERBATIM
        )
        set(BEAMS ${BEAMS} ${CMAKE_CURRENT_BINARY_DIR}/beams/Elixir.${module_name}.beam)
    endforeach()

    add_custom_target(
        ${avm_name}_beams ALL
        DEPENDS ${BEAMS}
    )

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${avm_name}_beams PackBEAM
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM -a ${INCLUDE_LINES} ${avm_name}.avm ${BEAMS}
        COMMENT "Packing archive ${avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}.avm
    )
endmacro()

macro(pack_runnable avm_name main)
    find_package(Elixir REQUIRED)

    add_custom_command(
        OUTPUT Elixir.${main}.beam
        COMMAND ${ELIXIRC_EXPR} ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        COMMENT "Compiling ${main}.ex"
        JOB_POOL elixir_pool
        VERBATIM
    )

    add_custom_target(
        ${avm_name}_main
        DEPENDS Elixir.${main}.beam
    )

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    foreach(archive_name ${ARGN})
        if(${archive_name} STREQUAL "exavmlib")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        else()
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        endif()
        set(ARCHIVE_TARGETS ${ARCHIVE_TARGETS} ${archive_name})
    endforeach()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${avm_name}_main ${ARCHIVE_TARGETS} PackBEAM
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${INCLUDE_LINES} ${avm_name}.avm Elixir.${main}.beam ${ARCHIVES}
        COMMENT "Packing runnable ${avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}.avm
    )

endmacro()


macro(pack_test avm_name main)
    find_package(Elixir REQUIRED)

    if(AVM_DISABLE_JIT)
        set(precompiled_suffix "")
    else()
        set(precompiled_suffix "-${AVM_JIT_TARGET_ARCH}")
        set(ARCHIVES_TARGETS jit)
    endif()

    # Compile the main module
    add_custom_command(
        OUTPUT Elixir.${main}.beam
        COMMAND ${ELIXIRC_EXPR} ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${main}.ex
        COMMENT "Compiling ${main}.ex"
        JOB_POOL elixir_pool
        VERBATIM
    )

    add_custom_target(
        ${avm_name}_main
        DEPENDS Elixir.${main}.beam
    )

    # Compile test modules
    foreach(module_name ${ARGN})
        add_custom_command(
            OUTPUT Elixir.${module_name}.beam
            COMMAND ${ELIXIRC_EXPR} ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.ex
            COMMENT "Compiling ${module_name}.ex"
            JOB_POOL elixir_pool
            VERBATIM
        )
        set(TEST_BEAMS ${TEST_BEAMS} Elixir.${module_name}.beam)
    endforeach()

    add_custom_target(
        ${avm_name}_tests
        DEPENDS ${TEST_BEAMS}
    )

    if(AVM_RELEASE)
        set(INCLUDE_LINES "")
    else()
        set(INCLUDE_LINES "-i")
    endif()

    # Set up standard libraries
    set(ARCHIVE_TARGETS ${ARCHIVES_TARGETS} estdlib eavmlib exavmlib etest)
    foreach(archive_name ${ARCHIVE_TARGETS})
        if(${archive_name} STREQUAL "exavmlib")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/lib/${archive_name}.avm)
        elseif(${archive_name} MATCHES "^estdlib$")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}${precompiled_suffix}.avm)
        elseif(${archive_name} MATCHES "^jit$")
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/jit/src/jit${precompiled_suffix}.avm)
        else()
            set(ARCHIVES ${ARCHIVES} ${CMAKE_BINARY_DIR}/libs/${archive_name}/src/${archive_name}.avm)
        endif()
    endforeach()

    add_custom_command(
        OUTPUT ${avm_name}.avm
        DEPENDS ${avm_name}_main ${avm_name}_tests ${ARCHIVE_TARGETS} PackBEAM
        COMMAND ${CMAKE_BINARY_DIR}/tools/packbeam/PackBEAM ${INCLUDE_LINES} ${avm_name}.avm Elixir.${main}.beam ${TEST_BEAMS} ${ARCHIVES}
        COMMENT "Packing test ${avm_name}.avm"
        VERBATIM
    )

    add_custom_target(
        ${avm_name} ALL
        DEPENDS ${avm_name}.avm
    )
endmacro()
