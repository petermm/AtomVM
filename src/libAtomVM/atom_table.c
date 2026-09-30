/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Davide Bettio <davide@uninstall.it>
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

#include "atom_table.h"
#include "defaultatoms.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "smp.h"
#include "unicode.h"
#include "utils.h"

#ifndef AVM_NO_SMP
#define SMP_RDLOCK(htable) smp_rwlock_rdlock(htable->lock)
#define SMP_WRLOCK(htable) smp_rwlock_wrlock(htable->lock)
#define SMP_UNLOCK(htable) smp_rwlock_unlock(htable->lock)
#else
#define SMP_RDLOCK(htable) UNUSED(htable)
#define SMP_WRLOCK(htable) UNUSED(htable)
#define SMP_UNLOCK(htable) UNUSED(htable)
#endif

#define DEFAULT_SIZE 32
#define MAX_ATOM_LEN ((1 << 12) - 1)
#define ATOM_TABLE_NOT_FOUND_MARKER ((atom_index_t) 0xFFFF)

#define ATOM_TABLE_THRESHOLD(capacity) (capacity + (capacity >> 2))

struct AtomEntry
{
    const uint8_t *data;
    uint16_t len;
    atom_index_t next;
};

struct AtomTable
{
    size_t capacity;
    size_t count;
    size_t entries_capacity;
    atom_index_t base_index;
#ifndef AVM_NO_SMP
    RWLock *lock;
#endif
    atom_index_t *buckets;
    struct AtomEntry *entries;
};

struct AtomTable *atom_table_new(void)
{
    struct AtomTable *htable = malloc(sizeof(struct AtomTable));
    if (IS_NULL_PTR(htable)) {
        return NULL;
    }
    htable->buckets = malloc(DEFAULT_SIZE * sizeof(atom_index_t));
    if (IS_NULL_PTR(htable->buckets)) {
        free(htable);
        return NULL;
    }
    memset(htable->buckets, 0xFF, DEFAULT_SIZE * sizeof(atom_index_t));

    htable->entries = malloc(DEFAULT_SIZE * sizeof(struct AtomEntry));
    if (IS_NULL_PTR(htable->entries)) {
        free(htable->buckets);
        free(htable);
        return NULL;
    }

    htable->count = 0;
    htable->base_index = 0;
    htable->capacity = DEFAULT_SIZE;
    htable->entries_capacity = DEFAULT_SIZE;

#ifndef AVM_NO_SMP
    htable->lock = smp_rwlock_create();
#endif

    return htable;
}

void atom_table_set_default_atoms(struct AtomTable *table, atom_index_t default_atoms_count)
{
    SMP_WRLOCK(table);
    table->base_index = default_atoms_count;
    table->count = default_atoms_count;
    SMP_UNLOCK(table);
}

void atom_table_destroy(struct AtomTable *table)
{
    if (IS_NULL_PTR(table)) {
        return;
    }
#ifndef AVM_NO_SMP
    smp_rwlock_destroy(table->lock);
#endif
    free(table->entries);
    free(table->buckets);
    free(table);
}

size_t atom_table_count(struct AtomTable *table)
{
    SMP_RDLOCK(table);
    size_t count = table->count;
    SMP_UNLOCK(table);

    return count;
}

static unsigned long sdbm_hash(const unsigned char *str, int len)
{
    unsigned long hash = len;
    int c;

    for (int i = 0; i < len; i++) {
        c = *str++;
        hash = c + (hash << 6) + (hash << 16) - hash;
    }

    return hash;
}

static inline atom_index_t get_dynamic_index_from_bucket(
    const struct AtomTable *table, unsigned long bucket_index, const uint8_t *string, size_t string_len)
{
    atom_index_t curr = table->buckets[bucket_index];
    while (curr != ATOM_TABLE_NOT_FOUND_MARKER) {
        const struct AtomEntry *entry = &table->entries[curr];
        if (entry->len == string_len && memcmp(entry->data, string, string_len) == 0) {
            return curr;
        }
        curr = entry->next;
    }

    return ATOM_TABLE_NOT_FOUND_MARKER;
}

static inline unsigned long bucket_index_from_hash(const struct AtomTable *table, unsigned long hash)
{
    return hash & (table->capacity - 1);
}

static inline atom_index_t get_dynamic_index_with_hash(
    const struct AtomTable *table, const uint8_t *string, size_t string_len, unsigned long hash)
{
    unsigned long bucket_index = bucket_index_from_hash(table, hash);
    return get_dynamic_index_from_bucket(table, bucket_index, string, string_len);
}

static inline atom_index_t get_dynamic_index(
    const struct AtomTable *table, const uint8_t *string, size_t string_len)
{
    unsigned long hash = sdbm_hash(string, string_len);
    return get_dynamic_index_with_hash(table, string, string_len, hash);
}

const uint8_t *atom_table_get_atom_string(struct AtomTable *table, atom_index_t index, size_t *out_size)
{
    if (table->base_index > 0 && index < table->base_index) {
        return defaultatoms_get_atom_string(index, out_size);
    }

    SMP_RDLOCK(table);

    if (UNLIKELY(index < table->base_index || index >= table->count)) {
        SMP_UNLOCK(table);
        return NULL;
    }

    size_t dyn_index = index - table->base_index;
    const struct AtomEntry *entry = &table->entries[dyn_index];
    const uint8_t *result = entry->data;
    *out_size = entry->len;

    SMP_UNLOCK(table);
    return result;
}

bool atom_table_is_equal_to_atom_string(struct AtomTable *table, atom_index_t t_atom_index, AtomString string)
{
    size_t t_atom_len;
    const uint8_t *t_atom_data = atom_table_get_atom_string(table, t_atom_index, &t_atom_len);
    if (IS_NULL_PTR(t_atom_data)) {
        return false;
    }

    return (t_atom_len == atom_string_len(string)) && (memcmp(t_atom_data, atom_string_data(string), t_atom_len) == 0);
}

int atom_table_cmp_using_atom_index(struct AtomTable *table, atom_index_t t_atom_index, atom_index_t other_atom_index)
{
    size_t t_atom_len;
    const uint8_t *t_atom_data = atom_table_get_atom_string(table, t_atom_index, &t_atom_len);
    if (IS_NULL_PTR(t_atom_data)) {
        return -1;
    }

    size_t other_atom_len;
    const uint8_t *other_atom_data = atom_table_get_atom_string(table, other_atom_index, &other_atom_len);
    if (IS_NULL_PTR(other_atom_data)) {
        return 1;
    }

    int cmp_size = (t_atom_len > other_atom_len) ? other_atom_len : t_atom_len;

    int memcmp_result = memcmp(t_atom_data, other_atom_data, cmp_size);

    if (memcmp_result == 0) {
        if (t_atom_len == other_atom_len) {
            return 0;
        } else {
            return (t_atom_len > other_atom_len) ? 1 : -1;
        }
    }

    return memcmp_result;
}

atom_ref_t atom_table_get_atom_ptr_and_len(struct AtomTable *table, atom_index_t index, size_t *out_len)
{
    return (atom_ref_t) atom_table_get_atom_string(table, index, out_len);
}

atom_index_t atom_table_get_index(struct AtomTable *table, AtomString string)
{
    size_t len = atom_string_len(string);
    const uint8_t *data = atom_string_data(string);
    atom_index_t result = ATOM_TABLE_NOT_FOUND_MARKER;
    if (table->base_index > 0 && defaultatoms_lookup(data, len, &result)) {
        return result;
    }
    SMP_RDLOCK(table);
    atom_index_t found = get_dynamic_index(table, data, len);
    if (found != ATOM_TABLE_NOT_FOUND_MARKER) {
        result = found + table->base_index;
    }
    SMP_UNLOCK(table);
    return result;
}

atom_index_t atom_table_get_index_from_cstring(struct AtomTable *table, const char *name)
{
    size_t len = strlen(name);
    const uint8_t *data = (const uint8_t *) name;
    atom_index_t result = ATOM_TABLE_NOT_FOUND_MARKER;
    if (table->base_index > 0 && defaultatoms_lookup(data, len, &result)) {
        return result;
    }
    SMP_RDLOCK(table);
    atom_index_t found = get_dynamic_index(table, data, len);
    if (found != ATOM_TABLE_NOT_FOUND_MARKER) {
        result = found + table->base_index;
    }
    SMP_UNLOCK(table);
    return result;
}

static bool ensure_entries_capacity(struct AtomTable *table, size_t needed)
{
    if (needed <= table->entries_capacity) {
        return true;
    }
    size_t new_cap = table->entries_capacity * 2;
    while (new_cap < needed) {
        new_cap *= 2;
    }
    if (new_cap > ATOM_TABLE_NOT_FOUND_MARKER) {
        new_cap = ATOM_TABLE_NOT_FOUND_MARKER;
    }
    if (new_cap < needed) {
        return false;
    }
    struct AtomEntry *new_entries = realloc(table->entries, new_cap * sizeof(struct AtomEntry));
    if (IS_NULL_PTR(new_entries)) {
        return false;
    }
    table->entries = new_entries;
    table->entries_capacity = new_cap;
    return true;
}

static bool do_rehash(struct AtomTable *table, size_t new_capacity)
{
    size_t new_size_bytes = sizeof(atom_index_t) * new_capacity;
    atom_index_t *new_buckets = malloc(new_size_bytes);
    if (IS_NULL_PTR(new_buckets)) {
        // Allocation failure can be ignored, the hash table will continue with the previous bucket
        return false;
    }
    memset(new_buckets, 0xFF, new_size_bytes);
    free(table->buckets);
    table->buckets = new_buckets;
    table->capacity = new_capacity;

    size_t dyn_count = table->count - table->base_index;
    for (size_t i = 0; i < dyn_count; i++) {
        unsigned long hash = sdbm_hash(table->entries[i].data, table->entries[i].len);
        unsigned long bucket_index = bucket_index_from_hash(table, hash);

        table->entries[i].next = table->buckets[bucket_index];
        table->buckets[bucket_index] = (atom_index_t) i;
    }

    return true;
}

static inline bool maybe_rehash(struct AtomTable *table, size_t new_entries)
{
    size_t new_count = (table->count - table->base_index) + new_entries;
    size_t threshold = ATOM_TABLE_THRESHOLD(table->capacity);
    if (new_count <= threshold) {
        return false;
    }

    size_t new_capacity = table->capacity * 2;
    while (new_count > ATOM_TABLE_THRESHOLD(new_capacity)) {
        new_capacity *= 2;
    }
    return do_rehash(table, new_capacity);
}

static inline atom_index_t insert_entry(
    struct AtomTable *table, unsigned long bucket_index, const uint8_t *atom_data, size_t atom_len)
{
    size_t dyn_index = table->count - table->base_index;
    atom_index_t global_index = (atom_index_t) table->count;
    table->count++;

    struct AtomEntry *entry = &table->entries[dyn_index];
    entry->data = atom_data;
    entry->len = (uint16_t) atom_len;
    entry->next = table->buckets[bucket_index];
    table->buckets[bucket_index] = (atom_index_t) dyn_index;

    return global_index;
}

enum AtomTableEnsureAtomResult atom_table_ensure_atom(
    struct AtomTable *table, const uint8_t *atom_data, size_t atom_len, enum AtomTableCopyOpt opts, atom_index_t *result)
{
    if (table->base_index > 0) {
        atom_index_t default_idx;
        if (defaultatoms_lookup(atom_data, atom_len, &default_idx)) {
            *result = default_idx;
            return AtomTableEnsureAtomOk;
        }
    }

    unsigned long hash = sdbm_hash(atom_data, atom_len);
    SMP_WRLOCK(table);
    unsigned long bucket_index = bucket_index_from_hash(table, hash);

    atom_index_t found_idx = get_dynamic_index_from_bucket(table, bucket_index, atom_data, atom_len);
    if (found_idx != ATOM_TABLE_NOT_FOUND_MARKER) {
        SMP_UNLOCK(table);
        *result = found_idx + table->base_index;
        return AtomTableEnsureAtomOk;
    }
    if (opts & AtomTableAlreadyExisting) {
        SMP_UNLOCK(table);
        return AtomTableEnsureAtomNotFound;
    }

    if (table->count >= ATOM_TABLE_NOT_FOUND_MARKER) {
        SMP_UNLOCK(table);
        return AtomTableEnsureAtomAllocFail;
    }

    if (!ensure_entries_capacity(table, (table->count - table->base_index) + 1)) {
        SMP_UNLOCK(table);
        return AtomTableEnsureAtomAllocFail;
    }

    if (opts & AtomTableCopyAtom) {
        if (atom_len > 0) {
            uint8_t *buf = malloc(atom_len);
            if (IS_NULL_PTR(buf)) {
                SMP_UNLOCK(table);
                return AtomTableEnsureAtomAllocFail;
            }
            memcpy(buf, atom_data, atom_len);
            atom_data = buf;
        } else {
            atom_data = (const uint8_t *) "";
        }
    }

    if (maybe_rehash(table, 1)) {
        bucket_index = bucket_index_from_hash(table, hash);
    }

    *result = insert_entry(table, bucket_index, atom_data, atom_len);

    SMP_UNLOCK(table);
    return AtomTableEnsureAtomOk;
}

static inline int read_encoded_len(const uint8_t **len_bytes)
{
    uint8_t byte0 = (*len_bytes)[0];

    if ((byte0 & 0x8) == 0) {
        (*len_bytes)++;
        return byte0 >> 4;

    } else if ((byte0 & 0x10) == 0) {
        uint8_t byte1 = (*len_bytes)[1];
        (*len_bytes) += 2;
        return ((byte0 >> 5) << 8) | byte1;

    } else {
        return -1;
    }
}

enum AtomTableEnsureAtomResult atom_table_ensure_atoms(struct AtomTable *table, const void *atoms, size_t count,
    atom_index_t *translate_table, enum EnsureAtomsOpt opt)
{
    bool is_long_format = (opt & EnsureLongEncoding) != 0;

    SMP_WRLOCK(table);

    size_t new_atoms_count = 0;

    const uint8_t *current_atom = atoms;

    for (size_t i = 0; i < count; i++) {
        int atom_len;
        if (is_long_format) {
            atom_len = read_encoded_len(&current_atom);
            if (UNLIKELY(atom_len < 0 || atom_len > MAX_ATOM_LEN)) {
                fprintf(stderr, "Found invalid atom len.\n");
                SMP_UNLOCK(table);
                return AtomTableEnsureAtomInvalidLen;
            }
        } else {
            atom_len = current_atom[0];
            current_atom++;
        }
        atom_index_t default_idx;
        if (table->base_index > 0 && defaultatoms_lookup(current_atom, atom_len, &default_idx)) {
            translate_table[i] = default_idx;
        } else {
            atom_index_t found_idx = get_dynamic_index(table, current_atom, atom_len);
            if (found_idx != ATOM_TABLE_NOT_FOUND_MARKER) {
                translate_table[i] = found_idx + table->base_index;
            } else {
                new_atoms_count++;
                translate_table[i] = ATOM_TABLE_NOT_FOUND_MARKER;
            }
        }
        current_atom += atom_len;
    }

    if (new_atoms_count > 0) {
        if (table->count + new_atoms_count >= ATOM_TABLE_NOT_FOUND_MARKER) {
            SMP_UNLOCK(table);
            return AtomTableEnsureAtomAllocFail;
        }

        if (!ensure_entries_capacity(table, (table->count - table->base_index) + new_atoms_count)) {
            SMP_UNLOCK(table);
            return AtomTableEnsureAtomAllocFail;
        }

        maybe_rehash(table, new_atoms_count);

        current_atom = atoms;
        size_t remaining_atoms = new_atoms_count;
        for (size_t i = 0; i < count; i++) {
            size_t atom_len;
            if (is_long_format) {
                // Size was checked above
                atom_len = (size_t) read_encoded_len(&current_atom);
            } else {
                atom_len = current_atom[0];
                current_atom++;
            }

            if (translate_table[i] == ATOM_TABLE_NOT_FOUND_MARKER) {
                unsigned long hash = sdbm_hash(current_atom, atom_len);
                unsigned long bucket_index = bucket_index_from_hash(table, hash);

                atom_index_t found_idx = get_dynamic_index_from_bucket(table, bucket_index, current_atom, atom_len);
                if (found_idx != ATOM_TABLE_NOT_FOUND_MARKER) {
                    translate_table[i] = found_idx + table->base_index;
                } else {
                    translate_table[i] = insert_entry(table, bucket_index, current_atom, atom_len);
                }

                remaining_atoms--;
                if (remaining_atoms == 0) {
                    break;
                }
            }
            current_atom += atom_len;
        }
    }

    SMP_UNLOCK(table);

    return AtomTableEnsureAtomOk;
}
