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

#include "defaultatoms_tables.h"

_Static_assert(GENERIC_DEFAULTATOMS_COUNT == PLATFORM_ATOMS_BASE_INDEX,
    "generic defaultatoms count mismatch");

const uint8_t *defaultatoms_get_atom_string(atom_index_t index, size_t *out_len)
{
    if (index < PLATFORM_ATOMS_BASE_INDEX) {
        const char *entry = generic_atoms[index];
        *out_len = (uint8_t) entry[0];
        return (const uint8_t *) (entry + 1);
    }

    return platform_defaultatoms_get_atom_string(index, out_len);
}

bool defaultatoms_lookup(const uint8_t *atom_data, size_t atom_len, atom_index_t *out_index)
{
    if (atom_len >= GENERIC_DEFAULTATOMS_MIN_LEN && atom_len <= GENERIC_DEFAULTATOMS_MAX_LEN) {
        size_t low = generic_len_offsets[atom_len];
        size_t high = generic_len_offsets[atom_len + 1];
        while (low < high) {
            size_t mid = low + (high - low) / 2;
            size_t index = generic_atoms_by_len[mid];
            int cmp = memcmp(atom_data, generic_atoms[index] + 1, atom_len);
            if (cmp == 0) {
                *out_index = (atom_index_t) index;
                return true;
            } else if (cmp < 0) {
                high = mid;
            } else {
                low = mid + 1;
            }
        }
    }

    return platform_defaultatoms_lookup(atom_data, atom_len, out_index);
}

void defaultatoms_init(GlobalContext *glb)
{
#if AVM_DEFAULT_ATOMS_IN_RAM
    for (size_t i = 0; i < PLATFORM_ATOMS_BASE_INDEX; i++) {
        if (UNLIKELY((size_t) (uint8_t) generic_atoms[i][0] != strlen(generic_atoms[i] + 1))) {
            AVM_ABORT();
        }

        term atom_term = globalcontext_make_atom(glb, generic_atoms[i]);
        if (UNLIKELY(term_to_atom_index(atom_term) != i)) {
            AVM_ABORT();
        }
    }
#else
    atom_table_set_default_atoms(glb->atom_table, platform_defaultatoms_count());
#endif
    platform_defaultatoms_init(glb);
}
