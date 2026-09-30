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
    0, 0, 0, 1, 9, 34, 59, 84, 101, 115, 127, 145, 162, 166, 174, 178, 184, 186, 188, 190, 191
};

static const uint8_t generic_atoms_by_len[191] = {
    // Len 2 (1 atom)
    2,
    // Len 3 (8 atoms)
    90, 15, 148, 125, 149, 146, 173, 166,
    // Len 4 (25 atoms)
    51, 57, 174, 84, 16, 133, 97, 143, 106, 48, 152, 172, 121, 60, 96, 28, 44, 175, 88, 176, 118, 1, 120, 43, 80,
    // Len 5 (25 atoms)
    114, 115, 131, 161, 189, 122, 94, 3, 0, 83, 22, 108, 171, 124, 165, 85, 141, 41, 45, 182, 13, 46, 81, 82, 147,
    // Len 6 (25 atoms)
    73, 4, 6, 59, 58, 75, 95, 170, 132, 180, 117, 107, 24, 77, 26, 98, 78, 55, 50, 65, 99, 91, 30, 29, 79,
    // Len 7 (17 atoms)
    130, 40, 103, 137, 104, 76, 92, 112, 49, 53, 142, 183, 52, 136, 179, 134, 116,
    // Len 8 (14 atoms)
    5, 19, 10, 38, 123, 89, 69, 62, 21, 167, 127, 119, 47, 37,
    // Len 9 (12 atoms)
    17, 162, 135, 113, 23, 12, 155, 63, 188, 177, 56, 18,
    // Len 10 (18 atoms)
    35, 102, 150, 181, 105, 61, 156, 159, 153, 160, 68, 129, 34, 71, 39, 31, 178, 8,
    // Len 11 (17 atoms)
    11, 187, 151, 184, 185, 186, 154, 157, 158, 42, 67, 66, 168, 139, 100, 190, 14,
    // Len 12 (4 atoms)
    111, 144, 101, 20,
    // Len 13 (8 atoms)
    93, 140, 25, 32, 128, 9, 33, 70,
    // Len 14 (4 atoms)
    64, 87, 74, 138,
    // Len 15 (6 atoms)
    86, 7, 169, 126, 164, 109,
    // Len 16 (2 atoms)
    163, 54,
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
        size_t low = generic_len_offsets[atom_len];
        size_t high = generic_len_offsets[atom_len + 1];
        while (low < high) {
            size_t mid = low + (high - low) / 2;
            uint8_t idx = generic_atoms_by_len[mid];
            const char *entry = generic_atoms[idx];
            int cmp = memcmp(atom_data, entry + 1, atom_len);
            if (cmp == 0) {
                *out_index = (atom_index_t) idx;
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
    atom_table_set_default_atoms(glb->atom_table, platform_defaultatoms_count());
    platform_defaultatoms_init(glb);
}
