/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <interop.h>
#include <memory.h>
#include <module.h>
#include <scheduler.h>
#include <sys.h>

#include <emscripten.h>
#include <emscripten/promise.h>

#include "emscripten_sys.h"

static GlobalContext *global = NULL;
static Module *main_module = NULL;
static Context *main_context = NULL;
static Module *entry_module = NULL;
static const char *entry_function = NULL;
static int entry_arity = 0;
static bool entry_started = false;
static bool pump_scheduled = false;
static int scheduled_pump_delay_ms = -1;
static uintptr_t pump_token = 0;
static int pending_async_callbacks = 0;

static void atomvm_pump(void *token_ptr);

static int load_module(const char *path)
{
    const char *ext = strrchr(path, '.');
    if (ext && strcmp(ext, ".avm") == 0) {
        struct AVMPackData *avmpack_data;
        if (sys_open_avm_from_file(global, path, &avmpack_data) != AVM_OPEN_OK) {
            fprintf(stderr, "Failed opening %s.\n", path);
            return EXIT_FAILURE;
        }
        synclist_append(&global->avmpack_data, &avmpack_data->avmpack_head);

        if (IS_NULL_PTR(main_module)) {
            const void *startup_beam = NULL;
            uint32_t startup_beam_size;
            const char *startup_module_name;
            avmpack_find_section_by_flag(avmpack_data->data, BEAM_START_FLAG, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name);
            if (startup_beam) {
                avmpack_data->in_use = true;
                main_module = module_new_from_iff_binary(global, startup_beam, startup_beam_size);
                if (IS_NULL_PTR(main_module)) {
                    fprintf(stderr, "Cannot load startup module: %s\n", startup_module_name);
                    return EXIT_FAILURE;
                }
                globalcontext_insert_module(global, main_module);
                main_module->module_platform_data = NULL;
            }
        }
    } else if (ext && strcmp(ext, ".beam") == 0) {
        Module *module = sys_load_module_from_file(global, path);
        if (IS_NULL_PTR(module)) {
            fprintf(stderr, "Failed opening %s.\n", path);
            return EXIT_FAILURE;
        }
        globalcontext_insert_module(global, module);
        if (IS_NULL_PTR(main_module) && module_search_exported_function(module, START_ATOM_INDEX, 0) != 0) {
            main_module = module;
        }
    } else {
        fprintf(stderr, "%s is not an AVM or BEAM file.\n", path);
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

static void destroy_runtime(void)
{
    if (main_context) {
        context_destroy(main_context);
        main_context = NULL;
    }

    if (global) {
        globalcontext_destroy(global);
        global = NULL;
    }

    main_module = NULL;
    entry_module = NULL;
    entry_function = NULL;
    entry_arity = 0;
    entry_started = false;
    pump_scheduled = false;
    scheduled_pump_delay_ms = -1;
    pending_async_callbacks = 0;
}

static int finalize_runtime(void)
{
    if (IS_NULL_PTR(main_context)) {
        destroy_runtime();
        return EXIT_FAILURE;
    }

    term ret_value = main_context->x[0];
    fprintf(stdout, "Return value: ");
    term_display(stdout, ret_value, main_context);
    fprintf(stdout, "\n");

    int status = (ret_value == OK_ATOM || ret_value == term_from_int(0)) ? EXIT_SUCCESS : EXIT_FAILURE;
    destroy_runtime();
    return status;
}

static int prepare_main_context(void)
{
    if (IS_NULL_PTR(main_module)) {
        fprintf(stderr, "main module not loaded\n");
        return EXIT_FAILURE;
    }

    main_context = context_new(global);
    if (IS_NULL_PTR(main_context)) {
        fprintf(stderr, "Unable to allocate startup context.\n");
        return EXIT_FAILURE;
    }
    main_context->leader = 1;

    Module *init_module = globalcontext_get_module(global, INIT_ATOM_INDEX);
    if (IS_NULL_PTR(init_module)) {
        entry_module = main_module;
        entry_function = "start";
        entry_arity = 0;
        return EXIT_SUCCESS;
    }

    if (UNLIKELY(memory_ensure_free(main_context, term_binary_heap_size(2) + LIST_SIZE(2, 0)) != MEMORY_GC_OK)) {
        fprintf(stderr, "Unable to allocate arguments.\n");
        return EXIT_FAILURE;
    }
    term s_opt = term_from_literal_binary("-s", strlen("-s"), &main_context->heap, global);
    term list = term_list_prepend(module_get_name(main_module), term_nil(), &main_context->heap);
    main_context->x[0] = term_list_prepend(s_opt, list, &main_context->heap);
    entry_module = init_module;
    entry_function = "boot";
    entry_arity = 1;
    return EXIT_SUCCESS;
}

void emscripten_nosmp_schedule_pump(int timeout_ms)
{
    if (IS_NULL_PTR(global)) {
        return;
    }
    if (timeout_ms < 0) {
        return;
    }
    if (pump_scheduled && scheduled_pump_delay_ms >= 0 && scheduled_pump_delay_ms <= timeout_ms) {
        return;
    }

    pump_scheduled = true;
    scheduled_pump_delay_ms = timeout_ms;
    pump_token++;
    emscripten_async_call(atomvm_pump, (void *) (uintptr_t) pump_token, timeout_ms);
}

void emscripten_nosmp_pending_async_start(void)
{
    pending_async_callbacks++;
}

void emscripten_nosmp_pending_async_finish(void)
{
    if (pending_async_callbacks > 0) {
        pending_async_callbacks--;
    }

    if (!IS_NULL_PTR(global) && global->scheduler_stop_all) {
        emscripten_nosmp_schedule_pump(0);
    }
}

static void atomvm_pump(void *token_ptr)
{
    if (IS_NULL_PTR(global) || (uintptr_t) token_ptr != pump_token) {
        return;
    }

    pump_scheduled = false;
    scheduled_pump_delay_ms = -1;

    if (!entry_started) {
        entry_started = true;
        context_execute_loop(main_context, entry_module, entry_function, entry_arity);
    } else {
        scheduler_entry_point(global);
    }

    if (IS_NULL_PTR(global)) {
        return;
    }

    if (global->scheduler_stop_all) {
        if (pending_async_callbacks > 0) {
            emscripten_nosmp_schedule_pump(0);
            return;
        }
        emscripten_force_exit(finalize_runtime());
        return;
    }

    int wait_timeout_ms = scheduler_get_idle_wait_timeout_ms(global);
    if (wait_timeout_ms >= 0) {
        emscripten_nosmp_schedule_pump(wait_timeout_ms);
    }
}

EMSCRIPTEN_KEEPALIVE
void cast(const char *name, const char *message)
{
    sys_enqueue_emscripten_cast_message(global, name, message);
}

EMSCRIPTEN_KEEPALIVE
em_promise_t call(const char *name, const char *message)
{
    return sys_enqueue_emscripten_call_message(global, name, message);
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "No .avm or .beam module specified\n");
        return EXIT_FAILURE;
    }

    global = globalcontext_new();
    if (IS_NULL_PTR(global)) {
        fprintf(stderr, "Unable to create global context.\n");
        return EXIT_FAILURE;
    }

    for (int i = 1; i < argc; ++i) {
        if (UNLIKELY(load_module(argv[i]) != EXIT_SUCCESS)) {
            destroy_runtime();
            return EXIT_FAILURE;
        }
    }

    if (UNLIKELY(prepare_main_context() != EXIT_SUCCESS)) {
        destroy_runtime();
        return EXIT_FAILURE;
    }

    scheduler_set_yield_on_idle(global, true);
    emscripten_nosmp_schedule_pump(0);
    emscripten_exit_with_live_runtime();
    return EXIT_SUCCESS;
}
