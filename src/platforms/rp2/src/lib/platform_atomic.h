/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

#ifndef _PLATFORM_ATOMIC_H
#define _PLATFORM_ATOMIC_H

#include <stdbool.h>
#include <stdint.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <pico/critical_section.h>

#pragma GCC diagnostic pop

#define ATOMIC

#define ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(object, expected, desired) \
    smp_atomic_compare_exchange_weak_ptr((void **) object, (void **) expected, (void *) desired)

#define ATOMIC_COMPARE_EXCHANGE_WEAK_INT(object, expected, desired) \
    smp_atomic_compare_exchange_weak_int((void *) object, (void *) expected, (uint64_t) desired, sizeof(desired))

#define atomic_load(object) \
    smp_atomic_load((void *) object, sizeof(*(object)))

#define atomic_fetch_add(object, operand) \
    smp_atomic_fetch_add((void *) object, (uint64_t) operand, sizeof(*(object)))

static critical_section_t atomic_cas_section;

/**
 * @brief Initialize structures of atomic functions
 */
static inline void atomic_init()
{
    critical_section_init(&atomic_cas_section);
}

/**
 * @brief Free structures for atomic functions
 */
static inline void atomic_free()
{
    critical_section_deinit(&atomic_cas_section);
}

static inline bool smp_atomic_compare_exchange_weak_ptr(void **object, void **expected, void *desired)
{
    // Use critical_section_t for both SMP and non-SMP to avoid disabling
    // mission-critical interrupts (GPIO, UART, etc.)
    critical_section_enter_blocking(&atomic_cas_section);

    bool result;
    result = *object == *expected;
    if (result) {
        *object = desired;
    } else {
        *expected = *object;
    }
    critical_section_exit(&atomic_cas_section);
    return result;
}

static inline bool smp_atomic_compare_exchange_weak_int(void *object, void *expected, uint64_t desired, size_t desired_len)
{
    // Use critical_section_t for both SMP and non-SMP to avoid disabling
    // mission-critical interrupts (GPIO, UART, etc.)
    critical_section_enter_blocking(&atomic_cas_section);

    bool result;
    switch (desired_len) {
        case sizeof(uint64_t): {
            uint64_t *object_ptr = (uint64_t *) object;
            uint64_t *expected_ptr = (uint64_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        case sizeof(uint32_t): {
            uint32_t *object_ptr = (uint32_t *) object;
            uint32_t *expected_ptr = (uint32_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        case sizeof(uint16_t): {
            uint16_t *object_ptr = (uint16_t *) object;
            uint16_t *expected_ptr = (uint16_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        case sizeof(uint8_t): {
            uint8_t *object_ptr = (uint8_t *) object;
            uint8_t *expected_ptr = (uint8_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
    }

    critical_section_exit(&atomic_cas_section);
    return result;
}

static inline uint64_t smp_atomic_load(void *object, size_t object_len)
{
    // Use critical_section_t for both SMP and non-SMP to avoid disabling
    // mission-critical interrupts (GPIO, UART, etc.)
    critical_section_enter_blocking(&atomic_cas_section);

    uint64_t result;
    switch (object_len) {
        case sizeof(uint64_t): {
            uint64_t *object_ptr = (uint64_t *) object;
            result = *object_ptr;
            break;
        }
        case sizeof(uint32_t): {
            uint32_t *object_ptr = (uint32_t *) object;
            result = *object_ptr;
            break;
        }
        case sizeof(uint16_t): {
            uint16_t *object_ptr = (uint16_t *) object;
            result = *object_ptr;
            break;
        }
        case sizeof(uint8_t): {
            uint8_t *object_ptr = (uint8_t *) object;
            result = *object_ptr;
            break;
        }
        default:
            result = 0;
            break;
    }

    critical_section_exit(&atomic_cas_section);
    return result;
}

static inline uint64_t smp_atomic_fetch_add(void *object, uint64_t operand, size_t object_len)
{
    // Use critical_section_t for both SMP and non-SMP to avoid disabling
    // mission-critical interrupts (GPIO, UART, etc.)
    critical_section_enter_blocking(&atomic_cas_section);

    uint64_t result;
    switch (object_len) {
        case sizeof(uint64_t): {
            uint64_t *object_ptr = (uint64_t *) object;
            result = *object_ptr;
            *object_ptr += operand;
            break;
        }
        case sizeof(uint32_t): {
            uint32_t *object_ptr = (uint32_t *) object;
            result = *object_ptr;
            *object_ptr += operand;
            break;
        }
        case sizeof(uint16_t): {
            uint16_t *object_ptr = (uint16_t *) object;
            result = *object_ptr;
            *object_ptr += operand;
            break;
        }
        case sizeof(uint8_t): {
            uint8_t *object_ptr = (uint8_t *) object;
            result = *object_ptr;
            *object_ptr += operand;
            break;
        }
        default:
            result = 0;
            break;
    }

    critical_section_exit(&atomic_cas_section);
    return result;
}

#endif
