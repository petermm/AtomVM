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

#include "nifs.h"
#include "interop.h"
#include "openc6_abi.h"
#include "defaultatoms.h"
#include "portnifloader.h"
#include "utils.h"
#include "term.h"

#include <string.h>
#include <stdlib.h>

extern const openc6_abi_t *openc6_abi;

static term nif_openc6_platform(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    
    return globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "openc6"));
}

static term nif_openc6_set_led_color(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    
    if (!term_is_uint8(argv[0]) || !term_is_uint8(argv[1]) || !term_is_uint8(argv[2])) {
        return term_invalid_term();
    }
    
    uint8_t r = term_to_uint8(argv[0]);
    uint8_t g = term_to_uint8(argv[1]);
    uint8_t b = term_to_uint8(argv[2]);
    
    openc6_abi->set_led_color(r, g, b);
    
    return OK_ATOM;
}

static term nif_openc6_wifi_connect(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    
    int ok1, ok2;
    char *ssid = interop_term_to_string(argv[0], &ok1);
    char *pass = interop_term_to_string(argv[1], &ok2);
    
    if (!ok1 || !ok2) {
        if (ssid) free(ssid);
        if (pass) free(pass);
        return term_invalid_term();
    }
    
    size_t ssid_len = strlen(ssid);
    size_t pass_len = strlen(pass);
    if (ssid_len > 32 || pass_len > 64) {
        free(ssid);
        free(pass);
        return term_invalid_term();
    }
    
    int32_t res = openc6_abi->wifi_connect(ssid, pass);
    
    free(ssid);
    free(pass);
    
    if (res == 0) {
        return OK_ATOM;
    } else {
        return ERROR_ATOM;
    }
}

static term nif_openc6_wifi_is_connected(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    
    int32_t res = openc6_abi->wifi_is_connected();
    return res ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_openc6_wifi_start_ap(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    
    int ok1, ok2;
    char *ssid = interop_term_to_string(argv[0], &ok1);
    char *pass = interop_term_to_string(argv[1], &ok2);
    
    if (!ok1 || !ok2) {
        if (ssid) free(ssid);
        if (pass) free(pass);
        return term_invalid_term();
    }
    
    size_t ssid_len = strlen(ssid);
    size_t pass_len = strlen(pass);
    if (ssid_len > 32 || pass_len > 64) {
        free(ssid);
        free(pass);
        return term_invalid_term();
    }
    
    int32_t res = openc6_abi->wifi_start_ap(ssid, pass);
    
    free(ssid);
    free(pass);
    
    if (res == 0) {
        return OK_ATOM;
    } else {
        return ERROR_ATOM;
    }
}

static term nif_openc6_sys_reset(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    
    openc6_abi->sys_reset();
    return OK_ATOM;
}

static const struct Nif openc6_platform_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_openc6_platform
};

static const struct Nif openc6_set_led_color_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_openc6_set_led_color
};

static const struct Nif openc6_wifi_connect_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_openc6_wifi_connect
};

static const struct Nif openc6_wifi_is_connected_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_openc6_wifi_is_connected
};

static const struct Nif openc6_wifi_start_ap_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_openc6_wifi_start_ap
};

static const struct Nif openc6_sys_reset_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_openc6_sys_reset
};

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        return &openc6_platform_nif;
    }
    if (strcmp("openc6:set_led_color/3", nifname) == 0) {
        return &openc6_set_led_color_nif;
    }
    if (strcmp("openc6:wifi_connect/2", nifname) == 0) {
        return &openc6_wifi_connect_nif;
    }
    if (strcmp("openc6:wifi_is_connected/0", nifname) == 0) {
        return &openc6_wifi_is_connected_nif;
    }
    if (strcmp("openc6:wifi_start_ap/2", nifname) == 0) {
        return &openc6_wifi_start_ap_nif;
    }
    if (strcmp("openc6:sys_reset/0", nifname) == 0) {
        return &openc6_sys_reset_nif;
    }
    
    return nif_collection_resolve_nif(nifname);
}
