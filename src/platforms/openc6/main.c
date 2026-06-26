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

#include "openc6_abi.h"
#include <globalcontext.h>
#include <defaultatoms.h>
#include <avmpack.h>
#include <module.h>
#include <portnifloader.h>
#include <sys.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

const openc6_abi_t *openc6_abi = NULL;

#ifdef OPENC6_DEBUG
#define DEBUG_PRINT(abi, msg) (abi)->print(msg)
#else
#define DEBUG_PRINT(abi, msg)
#endif

extern uint32_t _sidata;
extern uint32_t _sdata;
extern uint32_t _edata;
extern uint32_t _sbss;
extern uint32_t _ebss;

static void init_sections(void)
{
    uint32_t *src = &_sidata;
    uint32_t *dst = &_sdata;
    while (dst < &_edata) {
        *dst++ = *src++;
    }
    dst = &_sbss;
    while (dst < &_ebss) {
        *dst++ = 0;
    }
}

void __attribute__((section(".text.entry"), noreturn)) payload_main(const openc6_abi_t *abi)
{
    // Initialize writeable memory sections (relocate .data and zero .bss)
    init_sections();

    if (!abi || abi->magic != OPENC6_ABI_MAGIC || abi->version != OPENC6_ABI_VERSION) {
        while (1) { __asm__ volatile("nop"); }
    }
    
    openc6_abi = abi;
    
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);
    
    abi->print("\n");
    abi->print("###########################################################\n");
    abi->print("   Starting AtomVM on openC6 BIOS\n");
    abi->print("###########################################################\n");
    
    DEBUG_PRINT(abi, "Debug: Creating GlobalContext...\n");
    GlobalContext *glb = globalcontext_new();
    if (!glb) {
        abi->print("Fatal error: GlobalContext is NULL!\n");
        abi->delay_ms(3000);
        abi->sys_reset();
    }
    DEBUG_PRINT(abi, "Debug: GlobalContext created successfully.\n");

    DEBUG_PRINT(abi, "Debug: Initializing NIF collections...\n");
    nif_collection_init_all(glb);
    DEBUG_PRINT(abi, "Debug: NIF collections initialized.\n");
    
    struct AVMPackData *avmpack_data = NULL;
    DEBUG_PRINT(abi, "Debug: Opening main.avm...\n");
    enum OpenAVMResult open_res = sys_open_avm_from_file(glb, "main.avm", &avmpack_data);
    if (open_res != AVM_OPEN_OK) {
        abi->print("Fatal error: Could not load main.avm from openC6 FS.\n");
        abi->delay_ms(5000);
        abi->sys_reset();
    }
    DEBUG_PRINT(abi, "Debug: main.avm loaded successfully.\n");
    
    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;
    
    if (!avmpack_find_section_by_flag(avmpack_data->data, BEAM_START_FLAG, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
        abi->print("Fatal error: Failed to locate start module in main.avm.\n");
        abi->delay_ms(5000);
        abi->sys_reset();
    }
    
    synclist_append(&glb->avmpack_data, &avmpack_data->avmpack_head);
    
    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    if (!mod) {
        abi->print("Fatal error: Unable to load startup module.\n");
        abi->delay_ms(5000);
        abi->sys_reset();
    }
    
    globalcontext_insert_module(glb, mod);
    mod->module_platform_data = NULL;
    
    abi->print("AtomVM VM starting startup module execution...\n");
    globalcontext_run(glb, mod, NULL, 0, NULL);
    
    nif_collection_destroy_all(glb);
    globalcontext_destroy(glb);
    
    abi->print("AtomVM execution terminated. Resetting to BIOS in 3 seconds...\n");
    abi->delay_ms(3000);
    abi->sys_reset();
}
