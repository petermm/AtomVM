# SPDX-FileCopyrightText: 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

file(SIZE "${BOOT_LIB_PATH}" boot_lib_size)
math(EXPR partition_size "${AVM_LIB_SIZE}")
if(boot_lib_size GREATER partition_size)
    message(FATAL_ERROR "Boot library ${BOOT_LIB_PATH} is ${boot_lib_size} bytes but boot.avm holds only ${partition_size} bytes.")
endif()
