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
#include "globalcontext.h"
#include "term.h"
#include "utils.h"

#include <stdlib.h>
#include <string.h>

#ifdef AVM_STATIC_LIBRARY_ATOMS
#include "library_atoms.h"
#include "platform_defaultatoms.h"

_Static_assert(LIBRARY_ATOMS_BUILTIN_COUNT == ATOM_FIRST_AVAIL_INDEX, "library atom defaults mismatch");
_Static_assert((size_t) LIBRARY_ATOMS_BUILTIN_COUNT + LIBRARY_ATOMS_COUNT <= (atom_index_t) -1,
    "static library atoms exceed the atom index limit");
_Static_assert((size_t) LIBRARY_ATOMS_BUILTIN_COUNT + LIBRARY_ATOMS_COUNT - 1 <= UINTPTR_MAX >> TERM_IMMED2_TAG_SIZE,
    "static library atoms exceed the atom term limit");
#endif

// About X macro: https://en.wikipedia.org/wiki/X_macro
#define X(name, lenstr, str) \
    lenstr str,

static const char *const generic_atoms[] = {
#include "defaultatoms.def"

    // dummy value
    NULL
};
#undef X

atom_index_t defaultatoms_count(void)
{
#ifdef AVM_STATIC_LIBRARY_ATOMS
    return LIBRARY_ATOMS_BUILTIN_COUNT + LIBRARY_ATOMS_COUNT;
#else
    return platform_defaultatoms_count();
#endif
}

const uint8_t *defaultatoms_get_atom_string(atom_index_t index, size_t *out_len)
{
    if (index < PLATFORM_ATOMS_BASE_INDEX) {
        const char *entry = generic_atoms[index];
        *out_len = (size_t) (uint8_t) entry[0];
        return (const uint8_t *) (entry + 1);
    }
#ifdef AVM_STATIC_LIBRARY_ATOMS
    if (index >= LIBRARY_ATOMS_BUILTIN_COUNT) {
        size_t local_index = index - LIBRARY_ATOMS_BUILTIN_COUNT;
        if (local_index >= LIBRARY_ATOMS_COUNT) {
            return NULL;
        }
        *out_len = library_atoms_offsets[local_index + 1] - library_atoms_offsets[local_index];
        return library_atoms_data + library_atoms_offsets[local_index];
    }
#endif
    return platform_defaultatoms_get_atom_string(index, out_len);
}

#include "defaultatoms_tables.h"

_Static_assert(GENERIC_DEFAULTATOMS_COUNT == PLATFORM_ATOMS_BASE_INDEX, "generic defaultatoms count mismatch");

bool defaultatoms_lookup(const uint8_t *atom_data, size_t atom_len, atom_index_t *out_index)
{
    if (atom_len >= GENERIC_DEFAULTATOMS_MIN_LEN && atom_len <= GENERIC_DEFAULTATOMS_MAX_LEN) {
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
    if (platform_defaultatoms_lookup(atom_data, atom_len, out_index)) {
        return true;
    }
#ifdef AVM_STATIC_LIBRARY_ATOMS
    if (atom_len <= LIBRARY_ATOMS_MAX_LEN) {
        size_t low = library_atoms_len_offsets[atom_len];
        size_t high = library_atoms_len_offsets[atom_len + 1];
        while (low < high) {
            size_t mid = low + (high - low) / 2;
            uint32_t index = library_atoms_by_len[mid];
            const uint8_t *data = library_atoms_data + library_atoms_offsets[index];
            int cmp = memcmp(atom_data, data, atom_len);
            if (cmp == 0) {
                *out_index = (atom_index_t) (LIBRARY_ATOMS_BUILTIN_COUNT + index);
                return true;
            } else if (cmp < 0) {
                high = mid;
            } else {
                low = mid + 1;
            }
        }
    }
#endif
    return false;
}

void defaultatoms_init(GlobalContext *glb)
{
#if defined(CONFIG_AVM_DEFAULT_ATOMS_IN_RAM) || defined(AVM_DEFAULT_ATOMS_IN_RAM)
    for (size_t i = 0; i < PLATFORM_ATOMS_BASE_INDEX; i++) {
        const char *entry = generic_atoms[i];
        size_t len = (size_t) (uint8_t) entry[0];
        term atom_term = globalcontext_insert_atom_maybe_copy(glb, (const uint8_t *) (entry + 1), len, true);
        if (UNLIKELY(term_to_atom_index(atom_term) != i)) {
            AVM_ABORT();
        }
    }
#else
    atom_table_set_default_atoms(glb->atom_table, defaultatoms_count());
#endif
    platform_defaultatoms_init(glb);
#if defined(AVM_STATIC_LIBRARY_ATOMS) && (defined(CONFIG_AVM_DEFAULT_ATOMS_IN_RAM) || defined(AVM_DEFAULT_ATOMS_IN_RAM))
    for (size_t i = LIBRARY_ATOMS_BUILTIN_COUNT; i < defaultatoms_count(); i++) {
        size_t len;
        const uint8_t *data = defaultatoms_get_atom_string((atom_index_t) i, &len);
        term atom_term = globalcontext_insert_atom_maybe_copy(glb, data, len, true);
        if (UNLIKELY(term_to_atom_index(atom_term) != i)) {
            AVM_ABORT();
        }
    }
#endif
}
