/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. Madsen <petermm@gmail.com>
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

#ifndef _ATOMICS_H_
#define _ATOMICS_H_

#include <stddef.h>

#include "erl_nif.h"
#include "nifs.h"
#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

extern const ErlNifResourceTypeInit atomics_resource_type_init;

term nif_erts_internal_atomics_new_2(Context *ctx, int argc, term argv[]);

term nif_atomics_new_2(Context *ctx, int argc, term argv[]);
term nif_atomics_put_3(Context *ctx, int argc, term argv[]);
term nif_atomics_get_2(Context *ctx, int argc, term argv[]);
term nif_atomics_add_3(Context *ctx, int argc, term argv[]);
term nif_atomics_add_get_3(Context *ctx, int argc, term argv[]);
term nif_atomics_sub_3(Context *ctx, int argc, term argv[]);
term nif_atomics_sub_get_3(Context *ctx, int argc, term argv[]);
term nif_atomics_exchange_3(Context *ctx, int argc, term argv[]);
term nif_atomics_compare_exchange_4(Context *ctx, int argc, term argv[]);
term nif_atomics_info_1(Context *ctx, int argc, term argv[]);

#ifdef __cplusplus
}
#endif

#endif
