/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
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

#include "defaultatoms.h"
#include "atom_table.h"
#include "term.h"

#include <stdlib.h>
#include <string.h>

// About X macro: https://en.wikipedia.org/wiki/X_macro
#define X(name, lenstr, str) \
    lenstr str,

static const char *const generic_atoms[] = {
#include "defaultatoms.def"

    // dummy value
    NULL
};
#undef X

const uint8_t *defaultatoms_get_atom_string(atom_index_t index, size_t *out_len)
{
    if (index < PLATFORM_ATOMS_BASE_INDEX) {
        const char *entry = generic_atoms[index];
        *out_len = (size_t) (uint8_t) entry[0];
        return (const uint8_t *) (entry + 1);
    }
    return platform_defaultatoms_get_atom_string(index, out_len);
}

bool defaultatoms_lookup(const uint8_t *atom_data, size_t atom_len, atom_index_t *out_index)
{
    if (atom_len >= 2 && atom_len <= 19) {
        for (size_t i = 0; i < PLATFORM_ATOMS_BASE_INDEX; i++) {
            const char *entry = generic_atoms[i];
            if ((uint8_t) entry[0] == atom_len && memcmp(entry + 1, atom_data, atom_len) == 0) {
                *out_index = (atom_index_t) i;
                return true;
            }
        }
    }
    return platform_defaultatoms_lookup(atom_data, atom_len, out_index);
}

void defaultatoms_init(GlobalContext *glb)
{
    atom_table_set_default_atoms(glb->atom_table, platform_defaultatoms_count());
    platform_defaultatoms_init(glb);
}
