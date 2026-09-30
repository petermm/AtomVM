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

#include "platform_defaultatoms.h"

#include <stdlib.h>
#include <string.h>

// About X macro: https://en.wikipedia.org/wiki/X_macro
#define X(name, lenstr, str) \
    lenstr str,

static const char *const platform_atoms[] = {
#include "platform_defaultatoms.def"

    // dummy value
    NULL
};
#undef X

const uint8_t *platform_defaultatoms_get_atom_string(atom_index_t index, size_t *out_len)
{
    if (index >= PLATFORM_ATOMS_BASE_INDEX && index < ATOM_FIRST_AVAIL_INDEX) {
        const char *entry = platform_atoms[index - PLATFORM_ATOMS_BASE_INDEX];
        *out_len = (size_t) (uint8_t) entry[0];
        return (const uint8_t *) (entry + 1);
    }
    return NULL;
}

#include "platform_defaultatoms_tables.h"

_Static_assert(PLATFORM_DEFAULTATOMS_COUNT == (ATOM_FIRST_AVAIL_INDEX - PLATFORM_ATOMS_BASE_INDEX),
    "platform defaultatoms count mismatch");

bool platform_defaultatoms_lookup(const uint8_t *atom_data, size_t atom_len, atom_index_t *out_index)
{
    if (atom_len >= PLATFORM_DEFAULTATOMS_MIN_LEN && atom_len <= PLATFORM_DEFAULTATOMS_MAX_LEN) {
        size_t low = platform_len_offsets[atom_len];
        size_t high = platform_len_offsets[atom_len + 1];
        while (low < high) {
            size_t mid = low + (high - low) / 2;
            uint8_t idx = platform_atoms_by_len[mid];
            const char *entry = platform_atoms[idx];
            int cmp = memcmp(atom_data, entry + 1, atom_len);
            if (cmp == 0) {
                *out_index = (atom_index_t) (idx + PLATFORM_ATOMS_BASE_INDEX);
                return true;
            } else if (cmp < 0) {
                high = mid;
            } else {
                low = mid + 1;
            }
        }
    }
    return false;
}

atom_index_t platform_defaultatoms_count(void)
{
    return ATOM_FIRST_AVAIL_INDEX;
}

void platform_defaultatoms_init(GlobalContext *glb)
{
    UNUSED(glb);
}
