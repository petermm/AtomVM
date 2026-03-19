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

project(avm_emscripten_nosmp)

include(BuildErlang)

set(ERLANG_MODULES
    emscripten
    websocket
)

set(AVM_EMSCRIPTEN_NOSMP_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../avm_emscripten/src)

pack_archive(avm_emscripten_nosmp ${ERLANG_MODULES}
    SOURCE_DIR ${AVM_EMSCRIPTEN_NOSMP_SOURCE_DIR}
)

include(../../../version.cmake)

set(AVM_EMSCRIPTEN_NOSMP_VERSION ${ATOMVM_BASE_VERSION})

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/beams/
    DESTINATION lib/atomvm/lib/avm_emscripten_nosmp-${AVM_EMSCRIPTEN_NOSMP_VERSION}/ebin
    FILES_MATCHING PATTERN "*.beam"
)

install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/avm_emscripten_nosmp.avm
    DESTINATION lib/atomvm/lib/avm_emscripten_nosmp-${AVM_EMSCRIPTEN_NOSMP_VERSION}/ebin/
)

install(
    DIRECTORY ${AVM_EMSCRIPTEN_NOSMP_SOURCE_DIR}/
    DESTINATION lib/atomvm/lib/avm_emscripten_nosmp-${AVM_EMSCRIPTEN_NOSMP_VERSION}/src
    FILES_MATCHING PATTERN "*.erl"
)
