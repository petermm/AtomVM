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

static const uint8_t generic_len_offsets[21] = {
    0, 0, 0, 1, 8, 29, 51, 74, 89, 103, 113, 129, 140, 144, 152, 156, 161, 163, 165, 167, 168
};

static const uint8_t generic_atoms_by_len[168] = {
    // Len 2 (1 atom)
    2,
    // Len 3 (7 atoms)
    15, 90, 125, 146, 148, 149, 166,
    // Len 4 (21 atoms)
    1, 16, 28, 43, 44, 48, 51, 57, 60, 80, 84, 88, 96, 97, 106, 118, 120, 121, 133, 143, 152,
    // Len 5 (22 atoms)
    0, 3, 13, 22, 41, 45, 46, 81, 82, 83, 85, 94, 108, 114, 115, 122, 124, 131, 141, 147, 161, 165,
    // Len 6 (23 atoms)
    4, 6, 24, 26, 29, 30, 50, 55, 58, 59, 65, 73, 75, 77, 78, 79, 91, 95, 98, 99, 107, 117, 132,
    // Len 7 (15 atoms)
    40, 49, 52, 53, 76, 92, 103, 104, 112, 116, 130, 134, 136, 137, 142,
    // Len 8 (14 atoms)
    5, 10, 19, 21, 37, 38, 47, 62, 69, 89, 119, 123, 127, 167,
    // Len 9 (10 atoms)
    12, 17, 18, 23, 56, 63, 113, 135, 155, 162,
    // Len 10 (16 atoms)
    8, 31, 34, 35, 39, 61, 68, 71, 102, 105, 129, 150, 153, 156, 159, 160,
    // Len 11 (11 atoms)
    11, 14, 42, 66, 67, 100, 139, 151, 154, 157, 158,
    // Len 12 (4 atoms)
    20, 101, 111, 144,
    // Len 13 (8 atoms)
    9, 25, 32, 33, 70, 93, 128, 140,
    // Len 14 (4 atoms)
    64, 74, 87, 138,
    // Len 15 (5 atoms)
    7, 86, 109, 126, 164,
    // Len 16 (2 atoms)
    54, 163,
    // Len 17 (2 atoms)
    27, 72,
    // Len 18 (2 atoms)
    110, 145,
    // Len 19 (1 atom)
    36
};

bool defaultatoms_lookup(const uint8_t *atom_data, size_t atom_len, atom_index_t *out_index)
{
    if (atom_len >= 2 && atom_len <= 19) {
        size_t start = generic_len_offsets[atom_len];
        size_t end = generic_len_offsets[atom_len + 1];
        for (size_t i = start; i < end; i++) {
            uint8_t idx = generic_atoms_by_len[i];
            const char *entry = generic_atoms[idx];
            if (memcmp(entry + 1, atom_data, atom_len) == 0) {
                *out_index = (atom_index_t) idx;
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
