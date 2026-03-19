#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

cmake_minimum_required (VERSION 3.13)
project (libAtomVMPlatformEmscriptenNoSMP)
include(FetchMbedTLS)

set(AVM_EMSCRIPTEN_SHARED_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../../emscripten/src/lib)

set(HEADER_FILES
    ${AVM_EMSCRIPTEN_SHARED_DIR}/emscripten_sys.h
    ${AVM_EMSCRIPTEN_SHARED_DIR}/websocket_nifs.h
    "../../../../libAtomVM/otp_crypto.h"
)

set(SOURCE_FILES
    ${AVM_EMSCRIPTEN_SHARED_DIR}/platform_defaultatoms.c
    ${AVM_EMSCRIPTEN_SHARED_DIR}/platform_nifs.c
    ${AVM_EMSCRIPTEN_SHARED_DIR}/sys.c
    ${AVM_EMSCRIPTEN_SHARED_DIR}/websocket_nifs.c
    "../../../../libAtomVM/otp_crypto.c"
)

set(
    PLATFORM_LIB_SUFFIX
    ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}
)

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${SOURCE_FILES} ${HEADER_FILES})
target_compile_features(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC c_std_11)
target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} PRIVATE ${AVM_EMSCRIPTEN_SHARED_DIR})

target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)
target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ATOMVM_HAS_MBEDTLS AVM_EMSCRIPTEN_NOSMP)
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC MbedTLS::mbedcrypto)
include(CheckCSourceCompiles)
get_target_property(_mbedcrypto_includes MbedTLS::mbedcrypto INTERFACE_INCLUDE_DIRECTORIES)
set(CMAKE_REQUIRED_INCLUDES ${_mbedcrypto_includes})
check_c_source_compiles("
    #include <mbedtls/version.h>
    #ifndef MBEDTLS_PSA_CRYPTO_C
    #error PSA Crypto not available
    #endif
    int main(void) { return 0; }
" HAVE_PSA_CRYPTO)
unset(CMAKE_REQUIRED_INCLUDES)
unset(_mbedcrypto_includes)
if (HAVE_PSA_CRYPTO)
    target_compile_definitions(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC HAVE_PSA_CRYPTO)
endif()
