/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 AtomVM contributors
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

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "avmpack.h"

static unsigned int allocation_count;
static unsigned int fail_allocation;
static size_t copy_size;
static void *copy_ptr;
static bool copy_freed;

static void *module_test_malloc(size_t size)
{
    allocation_count++;
    if (allocation_count == fail_allocation) {
        return NULL;
    }
    void *ptr = malloc(size);
    if (allocation_count == 1) {
        copy_size = size;
        copy_ptr = ptr;
    }
    return ptr;
}

static void module_test_free(void *ptr)
{
    if (ptr != NULL && ptr == copy_ptr) {
        copy_freed = true;
    }
    free(ptr);
}

// Keep allocation injection local to the loader, not the whole runtime.
#define AVM_MODULES_IN_RAM "ram_module,ram_module_late"
#define malloc module_test_malloc
#define free module_test_free
#include "../src/libAtomVM/module.c"
#undef free
#undef malloc

static void write_u32(uint8_t *ptr, uint32_t value)
{
    ptr[0] = value >> 24;
    ptr[1] = value >> 16;
    ptr[2] = value >> 8;
    ptr[3] = value;
}

static size_t append_chunk(uint8_t *beam, size_t offset, const char *name, const uint8_t *data, size_t size)
{
    memcpy(beam + offset, name, 4);
    write_u32(beam + offset + 4, size);
    memcpy(beam + offset + 8, data, size);
    return offset + 8 + ((size + 3) & ~3);
}

// One final BEAM member with no padding beyond the archive's end marker.
static uint8_t *make_pack(const char *name, bool with_import, size_t *beam_size)
{
    uint8_t beam[128] = { 0 };
    memcpy(beam, "FOR1", 4);
    memcpy(beam + 8, "BEAM", 4);
    uint8_t atoms[32] = { 0 };
    write_u32(atoms, 1);
    atoms[4] = strlen(name);
    memcpy(atoms + 5, name, strlen(name));
    size_t offset = append_chunk(beam, 12, "AtU8", atoms, 5 + strlen(name));

    uint8_t imports[16] = { 0 };
    if (with_import) {
        write_u32(imports, 1);
        write_u32(imports + 4, 1);
        write_u32(imports + 8, 1);
    }
    offset = append_chunk(beam, offset, "ImpT", imports, with_import ? 16 : 4);

    // Code header followed by label 1 and int_code_end.
    const uint8_t code[] = {
        0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 3,
        0, 0, 0, 2, 0, 0, 0, 0, 1, 0x10, 3
    };
    offset = append_chunk(beam, offset, "Code", code, sizeof(code));
    const uint8_t exports[] = { 0, 0, 0, 0 };
    offset = append_chunk(beam, offset, "ExpT", exports, sizeof(exports));
    write_u32(beam + 4, offset - 8);
    *beam_size = offset;

    size_t header_size = 12 + ((strlen(name) + 6 + 3) & ~3);
    uint8_t *pack = calloc(1, 24 + header_size + offset + 16);
    assert(pack != NULL);
    memcpy(pack, "#!/usr/bin/env AtomVM\n", 21);
    write_u32(pack + 24, header_size + offset);
    write_u32(pack + 28, BEAM_START_FLAG | BEAM_CODE_FLAG);
    memcpy(pack + 36, name, strlen(name));
    memcpy(pack + 36 + strlen(name), ".beam", 6);
    memcpy(pack + 24 + header_size, beam, offset);
    memcpy(pack + 24 + header_size + offset + 12, "end", 4);
    return pack;
}

static void reset_allocations(unsigned int fail_at)
{
    allocation_count = 0;
    fail_allocation = fail_at;
    copy_size = 0;
    copy_ptr = NULL;
    copy_freed = false;
}

static void check_atom(GlobalContext *global, const uint8_t *beam, const char *name)
{
    atom_index_t index = atom_table_get_index_from_cstring(global->atom_table, name);
    size_t len;
    const uint8_t *data = atom_table_get_atom_string(global->atom_table, index, &len);
    // AtU8 is first: IFF header + chunk header + atom count + length byte.
    assert(data == beam + 25);
    assert(len == strlen(name));
    assert(memcmp(data, name, len) == 0);
}

static void test_module_in_ram(bool deferred, unsigned int fail_at)
{
    const char *name = deferred ? "ram_module_late" : "ram_module";
    size_t beam_size;
    uint8_t *pack = make_pack(name, fail_at == 3, &beam_size);
    const void *beam;
    uint32_t section_size;
    const char *section_name;
    assert(avmpack_find_section_by_flag(pack, BEAM_START_FLAG, BEAM_START_FLAG, &beam, &section_size, &section_name));
    assert(section_size > beam_size);
    GlobalContext *global = globalcontext_new();
    assert(global != NULL);
    Module *mod;
    if (deferred) {
        struct ConstAVMPack *avm = malloc(sizeof(struct ConstAVMPack));
        assert(avm != NULL);
        avmpack_data_init(&avm->base, &const_avm_pack_info);
        avm->base.data = pack;
        synclist_append(&global->avmpack_data, &avm->base.avmpack_head);
        reset_allocations(fail_at);
        mod = globalcontext_load_module_from_avm(global, section_name);
    } else {
        reset_allocations(fail_at);
        mod = module_new_from_iff_binary_maybe_in_ram(global, beam, section_size, section_name);
    }

    if (fail_at == 2 || fail_at == 3) {
        assert(mod == NULL);
        assert(copy_freed);
    } else {
        assert(mod != NULL);
        if (fail_at == 1) {
            assert(!mod->free_binary);
            assert(mod->binary == beam);
        } else {
            assert(mod->free_binary);
            assert(mod->binary == copy_ptr);
            assert(mod->binary_size == beam_size);
            assert(memcmp(mod->binary, beam, beam_size) == 0);
        }
        check_atom(global, beam, name);
        module_destroy(mod);
        assert(copy_freed == (fail_at != 1));
    }
    if (fail_at != 1) {
        assert(copy_size == beam_size);
    }
    if (fail_at != 2) {
        // Also checks atoms retained after import failure or module destruction.
        check_atom(global, beam, name);
    }
    globalcontext_destroy(global);
    free(pack);
}

static void test_truncated_beam(void)
{
    size_t beam_size;
    uint8_t *pack = make_pack("ram_module", false, &beam_size);
    const void *beam;
    uint32_t section_size;
    assert(avmpack_find_section_by_name(pack, "ram_module.beam", &beam, &section_size));
    GlobalContext *global = globalcontext_new();
    assert(global != NULL);
    reset_allocations(0);
    assert(module_new_from_iff_binary_maybe_in_ram(global, beam, 11, "ram_module") == NULL);
    assert(module_new_from_iff_binary_maybe_in_ram(global, beam, beam_size - 1, "ram_module") == NULL);
    assert(allocation_count == 0);
    globalcontext_destroy(global);
    free(pack);
}

#ifndef AVM_NO_EMU
static uint8_t *make_labt_beam(size_t opcode_size, uint32_t labels, size_t *beam_size)
{
    size_t template_size;
    uint8_t *pack = make_pack("labt_probe", false, &template_size);
    const void *template;
    uint32_t section_size;
    assert(avmpack_find_section_by_name(pack, "labt_probe.beam", &template, &section_size));
    unsigned long offsets[MAX_OFFS];
    unsigned long sizes[MAX_SIZES];
    scan_iff(template, template_size, offsets, sizes);

    uint8_t *beam = calloc(1, offsets[CODE] + 8 + ((20 + opcode_size + 3) & ~3) + 12);
    uint8_t *code = calloc(1, 20 + opcode_size);
    assert(beam != NULL && code != NULL);
    memcpy(beam, template, offsets[CODE]);
    write_u32(code, 16);
    write_u32(code + 8, OP_RETURN);
    write_u32(code + 12, labels);
    memset(code + 20, OP_RETURN, opcode_size);
    code[20] = OP_LABEL;
    code[21] = 0x10;
    code[20 + opcode_size - 3] = OP_LABEL;
    code[20 + opcode_size - 2] = 0x20;
    code[20 + opcode_size - 1] = OP_INT_CALL_END;
    size_t offset = append_chunk(beam, offsets[CODE], "Code", code, 20 + opcode_size);
    const uint8_t exports[] = { 0, 0, 0, 0 };
    *beam_size = append_chunk(beam, offset, "ExpT", exports, sizeof(exports));
    write_u32(beam + 4, *beam_size - 8);
    free(code);
    free(pack);
    return beam;
}

static uint8_t *beam_with_labt(const uint8_t *beam, size_t beam_size, const uint8_t *chunk, size_t chunk_size, size_t *out_size)
{
    *out_size = beam_size + ((chunk_size + 3) & ~3);
    uint8_t *result = calloc(1, *out_size);
    assert(result != NULL);
    memcpy(result, beam, beam_size);
    memcpy(result + beam_size, chunk, chunk_size);
    write_u32(result + 4, *out_size - 8);
    return result;
}

static void check_labt_load(const uint8_t *beam, size_t beam_size, size_t opcode_size, bool accepted, bool is_u16)
{
    GlobalContext *global = globalcontext_new();
    assert(global != NULL);
    Module *mod = module_new_from_iff_binary(global, beam, beam_size);
    assert(mod != NULL);
#ifdef AVM_NO_LABT
    accepted = false;
    is_u16 = opcode_size + 20 <= UINT16_MAX;
#endif
#ifdef AVM_NO_COMPACT_LABELS
    if (is_u16) {
        accepted = false;
    }
    is_u16 = false;
#endif
    assert(mod->labels_in_flash == accepted);
    assert(mod->labels_is_u16 == is_u16);
    assert(mod->end_instruction_ii == opcode_size - 1);
    assert(module_get_label_address(mod, 0) == mod->code->code);
    assert(module_get_label_address(mod, 1) == mod->code->code + 2);
    assert(module_get_label_address(mod, 2) == mod->code->code + opcode_size - 1);
    assert(module_label_code_offset(mod, 0) == 0);
    assert(module_label_code_offset(mod, 1) == 2);
    assert(module_label_code_offset(mod, 2) == opcode_size - 1);
    if (accepted) {
        unsigned long offsets[MAX_OFFS];
        unsigned long sizes[MAX_SIZES];
        scan_iff(beam, beam_size, offsets, sizes);
        assert(mod->labels == beam + offsets[LABT] + 8 + sizeof(struct LabTHeader));
    }
    module_destroy(mod);
    globalcontext_destroy(global);
}

static void test_labt_roundtrip(size_t opcode_size)
{
    size_t beam_size;
    uint8_t *beam = make_labt_beam(opcode_size, 3, &beam_size);
#ifdef AVM_NO_COMPACT_LABELS
    bool is_u16 = false;
#else
    bool is_u16 = opcode_size + 20 <= UINT16_MAX;
#endif
    check_labt_load(beam, beam_size, opcode_size, false, is_u16);
    size_t chunk_size;
    uint8_t *chunk = module_create_labt_chunk(beam, beam_size, &chunk_size);
    assert(chunk != NULL);
    struct LabTHeader header;
    memcpy(&header, chunk + 8, sizeof(header));
    uint16_t expected_flags = is_u16 ? 0 : LABT_FLAG_32BIT;
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    expected_flags |= LABT_FLAG_BIG_ENDIAN;
#endif
    assert(header.flags == expected_flags);
    assert(header.num_labels == 3);
    assert(header.end_instruction_ii == opcode_size - 1);
    assert(chunk_size == 16 + 3 * (is_u16 ? 2 : 4));
    size_t packed_size;
    uint8_t *packed = beam_with_labt(beam, beam_size, chunk, chunk_size, &packed_size);
    check_labt_load(packed, packed_size, opcode_size, true, is_u16);
    free(packed);
    free(chunk);
    free(beam);
}

static void test_labt_rejections(void)
{
    const size_t opcode_size = 17;
    size_t beam_size;
    uint8_t *beam = make_labt_beam(opcode_size, 3, &beam_size);
    size_t chunk_size;
    uint8_t *chunk = module_create_labt_chunk(beam, beam_size, &chunk_size);
    assert(chunk != NULL);
    struct LabTHeader original_header;
    memcpy(&original_header, chunk + 8, sizeof(original_header));
    bool is_u16 = !(original_header.flags & LABT_FLAG_32BIT);
    size_t entry_size = is_u16 ? 2 : 4;
    for (int test = 0; test < 13; test++) {
        uint8_t altered[32] = { 0 };
        memcpy(altered, chunk, chunk_size);
        struct LabTHeader header;
        memcpy(&header, altered + 8, sizeof(header));
        size_t altered_size = chunk_size;
        switch (test) {
            case 0: // Truncated header.
                altered_size = 12;
                break;
            case 1: // Missing final entry.
                altered_size -= entry_size;
                break;
            case 2: // Unspecified extension bytes.
                altered_size += 4;
                break;
            case 3:
                header.num_labels++;
                break;
            case 4:
                header.flags |= 4;
                break;
            case 5:
                header.flags ^= LABT_FLAG_BIG_ENDIAN;
                break;
            case 6: // Inside Code's payload, but outside its opcode bytes.
                header.end_instruction_ii = opcode_size;
                break;
            case 7: // In bounds, but not the end opcode.
                header.end_instruction_ii = 2;
                break;
            case 8: { // Label targets after the end instruction.
                if (is_u16) {
                    uint16_t invalid_offset = opcode_size;
                    memcpy(altered + 16 + entry_size, &invalid_offset, sizeof(invalid_offset));
                } else {
                    uint32_t invalid_offset = opcode_size;
                    memcpy(altered + 16 + entry_size, &invalid_offset, sizeof(invalid_offset));
                }
                break;
            }
            case 9: // Declared payload extends beyond the BEAM allocation.
                write_u32(altered + 4, 100);
                break;
            case 10: // Wrong entry width for the payload size.
                header.flags ^= LABT_FLAG_32BIT;
                break;
            case 11: // A header and entries actually encoded in the other byte order.
                header.end_instruction_ii = __builtin_bswap32(header.end_instruction_ii);
                header.num_labels = __builtin_bswap16(header.num_labels);
                header.flags = __builtin_bswap16(header.flags ^ LABT_FLAG_BIG_ENDIAN);
                for (int i = 0; i < 3; i++) {
                    for (size_t j = 0; j < entry_size / 2; j++) {
                        size_t first = 16 + i * entry_size + j;
                        size_t last = 16 + (i + 1) * entry_size - 1 - j;
                        uint8_t tmp = altered[first];
                        altered[first] = altered[last];
                        altered[last] = tmp;
                    }
                }
                break;
            case 12: // Legacy opposite-endian header with its endian bit missing.
                header.end_instruction_ii = __builtin_bswap32(header.end_instruction_ii);
                header.num_labels = __builtin_bswap16(header.num_labels);
                header.flags = 0;
                break;
        }
        memcpy(altered + 8, &header, sizeof(header));
        if (test != 9) {
            write_u32(altered + 4, altered_size - 8);
        }
        size_t packed_size;
        uint8_t *packed = beam_with_labt(beam, beam_size, altered, altered_size, &packed_size);
        check_labt_load(packed, packed_size, opcode_size, false, is_u16);
        free(packed);
    }
    size_t packed_size;
    uint8_t *packed = beam_with_labt(beam, beam_size, chunk, chunk_size, &packed_size);
    uint8_t *unaligned = malloc(packed_size + 1);
    assert(unaligned != NULL);
    memcpy(unaligned + 1, packed, packed_size);
    check_labt_load(unaligned + 1, packed_size, opcode_size, false, is_u16);
    free(unaligned);

    // Exactly the payload fits; outer IFF alignment padding is not table data.
    size_t payload_end = beam_size + chunk_size;
    write_u32(packed + 4, payload_end - 8);
    check_labt_load(packed, packed_size, opcode_size, true, is_u16);
    // The chunk fits the supplied buffer, but not the declared BEAM extent.
    write_u32(packed + 4, payload_end - 10);
    check_labt_load(packed, packed_size, opcode_size, false, is_u16);
    free(packed);
    free(chunk);
    free(beam);
}

static void test_labt_narrow_entries(void)
{
    size_t beam_size;
    uint8_t *beam = make_labt_beam(17, 3, &beam_size);
    // A narrow accelerator from a compact-labels build remains loadable when
    // compact labels are disabled, but must be rebuilt as a wide RAM table.
    uint8_t chunk[22] = { 'L', 'a', 'b', 'T' };
    write_u32(chunk + 4, 14);
    struct LabTHeader header = { .end_instruction_ii = 16, .flags = 0, .num_labels = 3 };
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    header.flags |= LABT_FLAG_BIG_ENDIAN;
#endif
    memcpy(chunk + 8, &header, sizeof(header));
    const uint16_t entries[] = { 0, 2, 16 };
    memcpy(chunk + 16, entries, sizeof(entries));
    size_t packed_size;
    uint8_t *packed = beam_with_labt(beam, beam_size, chunk, sizeof(chunk), &packed_size);
    check_labt_load(packed, packed_size, 17, true, true);
    free(packed);
    free(beam);
}

static void test_labt_wide_entries(void)
{
    size_t beam_size;
    uint8_t *beam = make_labt_beam(17, 3, &beam_size);
    // A valid 32-bit table for small Code: flags, not Code size, determine width.
    uint8_t chunk[28] = { 'L', 'a', 'b', 'T' };
    write_u32(chunk + 4, 20);
    struct LabTHeader header = { .end_instruction_ii = 16, .flags = LABT_FLAG_32BIT, .num_labels = 3 };
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    header.flags |= LABT_FLAG_BIG_ENDIAN;
#endif
    memcpy(chunk + 8, &header, sizeof(header));
    const uint32_t entries[] = { 0, 2, 16 };
    memcpy(chunk + 16, entries, sizeof(entries));
    size_t packed_size;
    uint8_t *packed = beam_with_labt(beam, beam_size, chunk, sizeof(chunk), &packed_size);
    check_labt_load(packed, packed_size, 17, true, false);
    free(packed);

    // Low 16 bits are in bounds; the full 32-bit offset must be rejected.
    uint32_t invalid_offset = 0x10002;
    memcpy(chunk + 20, &invalid_offset, sizeof(invalid_offset));
    packed = beam_with_labt(beam, beam_size, chunk, sizeof(chunk), &packed_size);
    check_labt_load(packed, packed_size, 17, false, true);
    free(packed);
    free(beam);
}

static void test_labt_label_count(void)
{
    size_t beam_size;
    uint8_t *beam = make_labt_beam(17, UINT16_MAX, &beam_size);
    size_t chunk_size;
    uint8_t *chunk = module_create_labt_chunk(beam, beam_size, &chunk_size);
    assert(chunk != NULL);
    struct LabTHeader header;
    memcpy(&header, chunk + 8, sizeof(header));
    assert(header.num_labels == UINT16_MAX);
    free(chunk);
    free(beam);

    beam = make_labt_beam(17, UINT16_MAX + 1U, &beam_size);
    assert(module_create_labt_chunk(beam, beam_size, &chunk_size) == NULL);
    free(beam);

    // A Code count with the same low 16 bits must not match a small LabT.
    beam = make_labt_beam(17, 3, &beam_size);
    chunk = module_create_labt_chunk(beam, beam_size, &chunk_size);
    assert(chunk != NULL);
    unsigned long offsets[MAX_OFFS];
    unsigned long sizes[MAX_SIZES];
    scan_iff(beam, beam_size, offsets, sizes);
    write_u32(beam + offsets[CODE] + 20, 0x10003);
    size_t packed_size;
    uint8_t *packed = beam_with_labt(beam, beam_size, chunk, chunk_size, &packed_size);
    check_labt_load(packed, packed_size, 17, false, true);
    free(packed);
    free(chunk);
    free(beam);
}
#endif

int main(void)
{
    // Fail the copy, Module allocation, or unresolved import after atom insertion.
    for (unsigned int fail_at = 0; fail_at <= 3; fail_at++) {
        test_module_in_ram(false, fail_at);
        test_module_in_ram(true, fail_at);
    }
    test_truncated_beam();
#ifndef AVM_NO_EMU
    test_labt_roundtrip(17);
    test_labt_roundtrip(UINT16_MAX - 20);
    test_labt_roundtrip(UINT16_MAX - 19);
    test_labt_roundtrip(UINT16_MAX + 17U);
    test_labt_rejections();
    test_labt_narrow_entries();
    test_labt_wide_entries();
    test_labt_label_count();
#endif
    return EXIT_SUCCESS;
}
