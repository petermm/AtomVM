/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include "platform_defaultatoms.h"

#include <string.h>

static const char *const pico_atom = ATOM_STR("\x4", "pico");

const uint8_t *platform_defaultatoms_get_atom_string(atom_index_t index, size_t *out_len)
{
    if (index == PICO_ATOM_INDEX) {
        *out_len = 4;
        return (const uint8_t *) (pico_atom + 1);
    }
    return NULL;
}

bool platform_defaultatoms_lookup(const uint8_t *atom_data, size_t atom_len, atom_index_t *out_index)
{
    if (atom_len == 4 && memcmp(atom_data, pico_atom + 1, 4) == 0) {
        *out_index = PICO_ATOM_INDEX;
        return true;
    }
    return false;
}

atom_index_t platform_defaultatoms_count(void)
{
    return PICO_ATOM_INDEX + 1;
}

void platform_defaultatoms_init(GlobalContext *glb)
{
    UNUSED(glb);
}
