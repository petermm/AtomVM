/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
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

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "avmpack.h"
#include "iff.h"
#include "module.h"
#include "utils.h"

#define AVMPACK_HEADER_SIZE 24

static inline uint32_t align4(uint32_t size)
{
    return ((size + 4 - 1) >> 2) << 2;
}

static uint8_t *read_entire_file(const char *filename, size_t *out_size)
{
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Error: cannot open file %s\n", filename);
        return NULL;
    }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (sz < 0) {
        fclose(f);
        return NULL;
    }
    uint8_t *buf = malloc(sz);
    if (!buf) {
        fclose(f);
        return NULL;
    }
    if (fread(buf, 1, sz, f) != (size_t) sz) {
        fprintf(stderr, "Error: cannot read file %s\n", filename);
        free(buf);
        fclose(f);
        return NULL;
    }
    fclose(f);
    *out_size = (size_t) sz;
    return buf;
}

static bool write_entire_file(const char *filename, const void *data, size_t size)
{
    FILE *f = fopen(filename, "wb");
    if (!f) {
        fprintf(stderr, "Error: cannot open file %s for writing\n", filename);
        return false;
    }
    if (fwrite(data, 1, size, f) != size) {
        fprintf(stderr, "Error: failed to write %zu bytes to %s\n", size, filename);
        fclose(f);
        return false;
    }
    fclose(f);
    return true;
}

static uint8_t *process_beam_binary(const uint8_t *beam_data, size_t beam_size, size_t *out_beam_size, bool strip_lines)
{
    size_t labt_chunk_size = 0;
    void *labt_chunk = module_create_labt_chunk(beam_data, beam_size, &labt_chunk_size);
    if (!labt_chunk && !strip_lines) {
        // No LabT chunk generated (already has LabT or no Code chunk) and no line stripping
        *out_beam_size = beam_size;
        uint8_t *copy = malloc(beam_size);
        if (copy) {
            memcpy(copy, beam_data, beam_size);
        }
        return copy;
    }

    size_t aligned_labt = labt_chunk ? align4(labt_chunk_size) : 0;
    size_t max_size = beam_size + aligned_labt;
    uint8_t *new_beam = malloc(max_size);
    if (!new_beam) {
        free(labt_chunk);
        return NULL;
    }

    // Write IFF header
    memcpy(new_beam, beam_data, 12);
    size_t out_offset = 12;

    // Walk existing chunks, skipping Line chunk if strip_lines is enabled
    size_t in_offset = 12;
    while (in_offset + 8 <= beam_size) {
        const uint8_t *chunk_hdr = beam_data + in_offset;
        uint32_t chunk_data_size = READ_32_UNALIGNED(chunk_hdr + 4);
        uint32_t chunk_aligned_data_size = align4(chunk_data_size);
        size_t chunk_wire_size = 8 + chunk_aligned_data_size;
        if (in_offset + chunk_wire_size > beam_size) {
            break;
        }

        if (strip_lines && memcmp(chunk_hdr, "Line", 4) == 0) {
            in_offset += chunk_wire_size;
            continue;
        }

        memcpy(new_beam + out_offset, chunk_hdr, chunk_wire_size);
        out_offset += chunk_wire_size;
        in_offset += chunk_wire_size;
    }

    // Append LabT chunk if generated
    if (labt_chunk) {
        memcpy(new_beam + out_offset, labt_chunk, labt_chunk_size);
        if (aligned_labt > labt_chunk_size) {
            memset(new_beam + out_offset + labt_chunk_size, 0, aligned_labt - labt_chunk_size);
        }
        out_offset += aligned_labt;
        free(labt_chunk);
    }

    // Update IFF size at offset 4
    WRITE_32_UNALIGNED(new_beam + 4, (uint32_t) (out_offset - 8));

    *out_beam_size = out_offset;
    return new_beam;
}

static bool process_avm_archive(const uint8_t *avm_data, size_t avm_size, const char *output_file, bool strip_lines)
{
    // Allocate generous output buffer
    size_t out_capacity = avm_size + 1024 * 1024;
    uint8_t *out_buf = malloc(out_capacity);
    if (!out_buf) {
        return false;
    }

    // Copy AVM header
    memcpy(out_buf, avm_data, AVMPACK_HEADER_SIZE);
    size_t in_offset = AVMPACK_HEADER_SIZE;
    size_t out_offset = AVMPACK_HEADER_SIZE;

    int modules_optimized = 0;

    while (in_offset < avm_size) {
        const uint32_t *header = (const uint32_t *) (avm_data + in_offset);
        uint32_t section_total_size = ENDIAN_SWAP_32(header[0]);
        uint32_t flags = ENDIAN_SWAP_32(header[1]);
        const char *section_name = (const char *) (header + 3);

        if (section_total_size == 0) {
            // End marker
            size_t remaining = avm_size - in_offset;
            if (out_offset + remaining > out_capacity) {
                out_capacity = out_offset + remaining + 65536;
                out_buf = realloc(out_buf, out_capacity);
            }
            memcpy(out_buf + out_offset, avm_data + in_offset, remaining);
            out_offset += remaining;
            break;
        }

        int name_len = strlen(section_name);
        int aligned_header_len = 12 + align4(name_len + 1);
        int data_len = section_total_size - aligned_header_len;
        const uint8_t *file_data = avm_data + in_offset + aligned_header_len;

        if ((flags & 2) != 0 && iff_is_valid_beam(file_data)) {
            size_t new_beam_size = 0;
            uint8_t *new_beam = process_beam_binary(file_data, data_len, &new_beam_size, strip_lines);
            if (new_beam) {
                if (new_beam_size != (size_t) data_len) {
                    modules_optimized++;
                }
                size_t aligned_new_data = align4(new_beam_size);
                uint32_t new_section_total = aligned_header_len + aligned_new_data;

                if (out_offset + new_section_total + 65536 > out_capacity) {
                    out_capacity = out_capacity * 2 + new_section_total;
                    out_buf = realloc(out_buf, out_capacity);
                }

                // Write header with updated total size
                uint32_t *out_header = (uint32_t *) (out_buf + out_offset);
                out_header[0] = ENDIAN_SWAP_32(new_section_total);
                out_header[1] = ENDIAN_SWAP_32(flags);
                out_header[2] = 0;
                memcpy((char *) (out_header + 3), section_name, name_len + 1);
                // Zero padding in header
                int pad_hdr = aligned_header_len - (12 + name_len + 1);
                if (pad_hdr > 0) {
                    memset((uint8_t *) (out_header + 3) + name_len + 1, 0, pad_hdr);
                }

                // Write beam data
                memcpy(out_buf + out_offset + aligned_header_len, new_beam, new_beam_size);
                int pad_data = aligned_new_data - new_beam_size;
                if (pad_data > 0) {
                    memset(out_buf + out_offset + aligned_header_len + new_beam_size, 0, pad_data);
                }

                out_offset += new_section_total;
                free(new_beam);
                in_offset += section_total_size;
                continue;
            }
        }

        // Copy section unchanged
        if (out_offset + section_total_size > out_capacity) {
            out_capacity = out_capacity * 2 + section_total_size;
            out_buf = realloc(out_buf, out_capacity);
        }
        memcpy(out_buf + out_offset, avm_data + in_offset, section_total_size);
        out_offset += section_total_size;
        in_offset += section_total_size;
    }

    printf("Optimized %d module(s) (LabT%s)\n", modules_optimized, strip_lines ? ", stripped Line" : "");

    bool ok = write_entire_file(output_file, out_buf, out_offset);
    free(out_buf);
    return ok;
}

static bool is_boot_avm(const char *path)
{
    const char *base = strrchr(path, '/');
    base = base ? base + 1 : path;
    return (strstr(base, "boot") != NULL);
}

int main(int argc, char **argv)
{
    bool strip_lines = false;
    bool explicit_strip_lines = false;
    bool force_keep_lines = false;
    int arg_idx = 1;
    while (arg_idx < argc && argv[arg_idx][0] == '-') {
        if (!strcmp(argv[arg_idx], "--strip-lines") ||
            !strcmp(argv[arg_idx], "--strip_lines") ||
            !strcmp(argv[arg_idx], "--remove_lines") ||
            !strcmp(argv[arg_idx], "--remove-lines") ||
            !strcmp(argv[arg_idx], "-r")) {
            strip_lines = true;
            explicit_strip_lines = true;
            arg_idx++;
        } else if (!strcmp(argv[arg_idx], "--keep-lines") ||
                   !strcmp(argv[arg_idx], "--keep_lines") ||
                   !strcmp(argv[arg_idx], "--no-strip-lines")) {
            force_keep_lines = true;
            arg_idx++;
        } else {
            fprintf(stderr, "Unknown option: %s\n", argv[arg_idx]);
            return EXIT_FAILURE;
        }
    }

    if (arg_idx >= argc) {
        fprintf(stderr, "Usage: %s [--strip-lines|-r|--keep-lines] <input.avm|input.beam> [output_file]\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *input_file = argv[arg_idx];
    const char *output_file = (arg_idx + 1 < argc) ? argv[arg_idx + 1] : argv[arg_idx];

    // Default to stripping Line chunks for boot AVMs (e.g. esp32boot.avm, elixir_esp32boot.avm)
    // to guarantee they fit within fixed flash boot partition limits (e.g. 0x80000 / 512KB).
    if (!explicit_strip_lines && !force_keep_lines) {
        if (is_boot_avm(input_file) || is_boot_avm(output_file)) {
            strip_lines = true;
        }
    }

    size_t file_size = 0;
    uint8_t *data = read_entire_file(input_file, &file_size);
    if (!data) {
        return EXIT_FAILURE;
    }

    if (avmpack_is_valid(data, file_size)) {
        if (!process_avm_archive(data, file_size, output_file, strip_lines)) {
            free(data);
            return EXIT_FAILURE;
        }
    } else if (iff_is_valid_beam(data)) {
        size_t new_beam_size = 0;
        uint8_t *new_beam = process_beam_binary(data, file_size, &new_beam_size, strip_lines);
        if (!new_beam) {
            free(data);
            return EXIT_FAILURE;
        }
        if (!write_entire_file(output_file, new_beam, new_beam_size)) {
            free(new_beam);
            free(data);
            return EXIT_FAILURE;
        }
        free(new_beam);
        printf("Optimized %s (LabT%s)\n", output_file, strip_lines ? ", stripped Line" : "");
    } else {
        fprintf(stderr, "Error: %s is neither a valid PackBEAM (.avm) nor BEAM file\n", input_file);
        free(data);
        return EXIT_FAILURE;
    }

    free(data);
    return EXIT_SUCCESS;
}
