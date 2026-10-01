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

#ifndef _AVM_ATTRS_H_
#define _AVM_ATTRS_H_

// AVM_IRAM_ATTR places a function in instruction RAM on platforms that have one
// (currently ESP32). The build selects which parts of the VM go there with
// AVM_IRAM_GC (garbage collector hot path), AVM_IRAM_BIFS (integer arithmetic
// BIFs) and AVM_IRAM_VM (term_compare, scheduler, mailbox); on other platforms,
// or when none of them is defined, it expands to nothing.
#if !defined(AVM_IRAM_ATTR)
#if (defined(AVM_IRAM_GC) || defined(AVM_IRAM_BIFS) || defined(AVM_IRAM_VM)) && defined(ESP_PLATFORM)
#include <esp_attr.h>
#define AVM_IRAM_ATTR IRAM_ATTR
#else
#define AVM_IRAM_ATTR
#endif
#endif

// Which part of the hot set a function belongs to.
#if defined(AVM_IRAM_GC)
#define AVM_IRAM_GC_ATTR AVM_IRAM_ATTR
#else
#define AVM_IRAM_GC_ATTR
#endif

#if defined(AVM_IRAM_BIFS)
#define AVM_IRAM_BIFS_ATTR AVM_IRAM_ATTR
#else
#define AVM_IRAM_BIFS_ATTR
#endif

#if defined(AVM_IRAM_VM)
#define AVM_IRAM_VM_ATTR AVM_IRAM_ATTR
#else
#define AVM_IRAM_VM_ATTR
#endif

#endif
