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

#include "openc6_sys.h"
#include "openc6_abi.h"
#include "avmpack.h"
#include "defaultatoms.h"
#include "portnifloader.h"
#include "utils.h"

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "listeners.h"

extern const openc6_abi_t *openc6_abi;

// Memory APIs mapped to openC6 heap
void *sys_malloc(size_t size) {
    return openc6_abi->malloc(size);
}

void sys_free(void *ptr) {
    openc6_abi->free(ptr);
}

void sys_init_platform(GlobalContext *glb) {
    UNUSED(glb);
}

void sys_free_platform(GlobalContext *glb) {
    UNUSED(glb);
}

void platform_defaultatoms_init(GlobalContext *glb) {
    UNUSED(glb);
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path) {
    UNUSED(global);
    UNUSED(path);
    return NULL;
}

Context *sys_create_port(GlobalContext *glb, const char *port_name, term opts) {
    return port_driver_create_port(port_name, glb, opts);
}

term sys_get_info(Context *ctx, term key) {
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}

void sys_poll_events(GlobalContext *glb, int timeout_ms) {
    UNUSED(glb);
    if (timeout_ms != 0) {
        openc6_abi->delay_ms(timeout_ms > 0 ? timeout_ms : 10);
    }
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write) {
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write) {
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

static void event_listener_add_to_polling_set(struct EventListener *listener, GlobalContext *glb) {
    UNUSED(listener);
    UNUSED(glb);
}

static void listener_event_remove_from_polling_set(listener_event_t event, GlobalContext *glb) {
    UNUSED(event);
    UNUSED(glb);
}

static bool event_listener_is_event(EventListener *listener, listener_event_t event) {
    return listener->event == event;
}

void sys_register_listener(GlobalContext *global, struct EventListener *listener) {
    struct ListHead *listeners = synclist_wrlock(&global->listeners);
    list_append(listeners, &listener->listeners_list_head);
    synclist_unlock(&global->listeners);
}

void sys_unregister_listener(GlobalContext *global, struct EventListener *listener) {
    struct ListHead *dummy = synclist_wrlock(&global->listeners);
    UNUSED(dummy);
    list_remove(&listener->listeners_list_head);
    synclist_unlock(&global->listeners);
}

void sys_unregister_listener_from_event(GlobalContext *global, listener_event_t event) {
    struct ListHead *list = synclist_wrlock(&global->listeners);
    struct ListHead *item;
    LIST_FOR_EACH (item, list) {
        struct EventListener *listener = GET_LIST_ENTRY(item, struct EventListener, listeners_list_head);
        if (event_listener_is_event(listener, event)) {
            list_remove(&listener->listeners_list_head);
            break;
        }
    }
    synclist_unlock(&global->listeners);
}

void sys_time(struct timespec *t) {
    uint64_t us = openc6_abi->get_time_us();
    t->tv_sec = us / 1000000;
    t->tv_nsec = (us % 1000000) * 1000;
}

void sys_monotonic_time(struct timespec *t) {
    sys_time(t);
}

uint64_t sys_monotonic_time_u64() {
    return openc6_abi->get_time_us();
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms) {
    return ms * 1000;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t ticks) {
    return ticks / 1000;
}

#define READ_CHUNK 1024U
// Helper to read an entire file in chunks from openc6_fs
static int32_t openc6_read_entire_file(const char *name, uint8_t **out_buf, uint32_t parent_id) {
    uint32_t allocated = 2048U;
    uint8_t *buf = openc6_abi->malloc(allocated);
    if (!buf) return -1;

    uint32_t total_read = 0;
    while (1) {
        if (allocated - total_read < READ_CHUNK) {
            if (allocated > UINT32_MAX - 2048U) {
                openc6_abi->free(buf);
                return -1;
            }
            uint32_t new_allocated = allocated + 2048U;
            uint8_t *new_buf = openc6_abi->malloc(new_allocated);
            if (!new_buf) {
                openc6_abi->free(buf);
                return -1;
            }
            memcpy(new_buf, buf, total_read);
            openc6_abi->free(buf);
            buf = new_buf;
            allocated = new_allocated;
        }
        int32_t chunk_read = openc6_abi->fs_read_file(name, buf + total_read, total_read, READ_CHUNK, parent_id);
        if (chunk_read < 0) {
            openc6_abi->free(buf);
            return -1;
        }
        if (chunk_read == 0) {
            break;
        }
        if ((uint32_t)chunk_read > READ_CHUNK || total_read > UINT32_MAX - (uint32_t)chunk_read) {
            openc6_abi->free(buf);
            return -1;
        }
        total_read += (uint32_t)chunk_read;
    }
    *out_buf = buf;
    return total_read;
}

static void heap_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global) {
    UNUSED(global);
    openc6_abi->free((void *) obj->data);
    openc6_abi->free(obj);
}

static const struct AVMPackInfo heap_avm_pack_info = {
    .destructor = heap_avm_pack_destructor
};

enum OpenAVMResult sys_open_avm_from_file(GlobalContext *global, const char *path, struct AVMPackData **data) {
    UNUSED(global);
    uint8_t *file_data = NULL;
    int32_t size = openc6_read_entire_file(path, &file_data, 0);
    if (size < 0) {
        return AVM_OPEN_CANNOT_OPEN;
    }

    struct AVMPackData *avmpack_data = openc6_abi->malloc(sizeof(struct AVMPackData));
    if (!avmpack_data) {
        openc6_abi->free(file_data);
        return AVM_OPEN_FAILED_ALLOC;
    }

    avmpack_data_init(avmpack_data, &heap_avm_pack_info);
    avmpack_data->data = file_data;

    *data = avmpack_data;
    return AVM_OPEN_OK;
}
