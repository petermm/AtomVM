/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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
#include <libgen.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "iff.h"
#include "jit.h"
#include "mapped_file.h"
#include "module.h"
#include "term.h"
#include "utils.h"

struct Test
{
    const char *test_module;
    int32_t expected_value;
    bool skip_atom;
    bool skip_beam;
};

// Favor modules that return 0
#define TEST_CASE(module)        \
    {                            \
        #module, 0, false, false \
    }
#define TEST_CASE_EXPECTED(module, expected) \
    {                                        \
        #module, expected, false, false      \
    }
#define TEST_CASE_ATOMVM_ONLY(module, expected) \
    {                                           \
        #module, expected, false, true          \
    }
#define TEST_CASE_COND(module, expected, skip) \
    {                                          \
        #module, expected, skip, false         \
    }

#ifndef AVM_NO_SMP
#define SKIP_SMP false
#else
#define SKIP_SMP true
#endif

#ifndef AVM_CREATE_STACKTRACES
#define SKIP_STACKTRACES true
#else
#define SKIP_STACKTRACES false
#endif

// Enabling this will override malloc and calloc weak symbols,
// so we can force an alternative version of malloc that returns
// NULL when size is 0.
// This is useful to find debugging or finding some kind of issues.
#ifdef FORCE_MALLOC_ZERO_RETURNS_NULL
void *malloc(size_t size)
{
    if (size == 0) {
        return NULL;
    } else {
        void *memptr = NULL;
        if (posix_memalign(&memptr, sizeof(void *), size) != 0) {
            return NULL;
        }
        return memptr;
    }
}

void *calloc(size_t nmemb, size_t size)
{
    void *ptr = malloc(nmemb * size);
    if (ptr != NULL) {
        memset(ptr, 0, nmemb * size);
    }
    return ptr;
}
#endif

struct Test tests[] = {
    TEST_CASE_EXPECTED(roundfloat, -3),
    TEST_CASE(floatmath),

    // TEST CRASHES HERE: TEST_CASE(memlimit),

    { NULL, 0, false, false }
};

static int test_atom(struct Test *test)
{
    int result = 0;
    char module_file[128];
    snprintf(module_file, sizeof(module_file), "%s.beam", test->test_module);
    MappedFile *beam_file = mapped_file_open_beam(module_file);
    assert(beam_file != NULL);

    GlobalContext *glb = globalcontext_new();
    Module *mod = module_new_from_iff_binary(glb, beam_file->mapped, beam_file->size);
    if (IS_NULL_PTR(mod)) {
        fprintf(stderr, "Cannot load startup module: %s\n", test->test_module);
        return -1;
    }
    globalcontext_insert_module(glb, mod);
    Context *ctx = context_new(glb);
    ctx->leader = 1;

    context_execute_loop(ctx, mod, "start", 0);

    if (!term_is_any_integer(ctx->x[0])) {
        fprintf(stderr, "\x1b[1;31mExpected %i but result is not an integer\x1b[0m\n", test->expected_value);
        result = -1;
    } else {
        int32_t value = (int32_t) term_maybe_unbox_int(ctx->x[0]);
        if (value != test->expected_value) {
            fprintf(stderr, "\x1b[1;31mExpected %i, got: %i\x1b[0m\n", test->expected_value, value);
            result = -1;
        }
    }

    context_destroy(ctx);
    globalcontext_destroy(glb);
    mapped_file_close(beam_file);
    return result;
}

static int test_beam(struct Test *test)
{
    char command[512];
    size_t written = snprintf(command, sizeof(command),
        "erl -pa . -eval '"
        "erlang:process_flag(trap_exit, false), \n" /* init(3) traps exits */
        "S = try %s:start() of\n"
        "    R when R =:= %i -> 0;\n"
        "    R               -> io:format(\"Expected ~B, got ~p\\n\", [%i, R]), 1\n"
        "catch\n"
        "    _C:E:ST -> io:format(\"Raised ~p, stacktrace:\\n~p\\n\", [E, ST]), 1\n"
        "end,\n"
        "erlang:halt(S).' -noshell",
        test->test_module,
        test->expected_value,
        test->expected_value);
    if (written >= sizeof(command) - 1) {
        fprintf(stderr, "Exceeded buffer size for module %s\n", test->test_module);
        return 1;
    }
    return system(command);
}

int test_module_execution(bool beam, struct Test *test)
{
    if (beam ? test->skip_beam : test->skip_atom) {
        fprintf(stderr, "%s:\x1b[34GSKIPPED\n", test->test_module);
        return 0;
    }
    fprintf(stderr, "%s:\r", test->test_module);
    fflush(NULL);
    int result = beam ? test_beam(test) : test_atom(test);
    fflush(NULL);
    if (result) {
        fprintf(stderr, "\x1b[2K\x1b[1;31m%s:\x1b[34GFAILED\x1b[0m\n", test->test_module);
        return 1;
    }
    fprintf(stderr, "\x1b[2K%s:\x1b[34GOK\n", test->test_module);
    return 0;
}

int test_modules_execution(bool beam, bool skip, int count, char **item)
{
    if (chdir("erlang_tests") != 0) {
        perror("Error: ");
        return EXIT_FAILURE;
    }
#ifndef AVM_NO_JIT
    if (!beam) {
#if JIT_ARCH_TARGET == JIT_ARCH_X86_64
        if (chdir("x86_64") != 0) {
            perror("Error: cannot find x86_64 directory");
            return EXIT_FAILURE;
        }
#elif JIT_ARCH_TARGET == JIT_ARCH_AARCH64
        if (chdir("aarch64") != 0) {
            perror("Error: cannot find aarch64 directory");
            return EXIT_FAILURE;
        }
#else
#error Unknown JIT target
#endif
    }
#endif

    int failed_tests = 0;

    if (count) {
        for (int ix = 0; ix < count; ix++) {
            struct Test *test = tests;
            do {
                if (strcmp(test->test_module, item[ix]) == 0) {
                    if (skip) {
                        test->skip_beam = true;
                        test->skip_atom = true;
                    } else {
                        failed_tests += test_module_execution(beam, test);
                    }
                    break;
                }
                test++;
            } while (test->test_module);
            if (test->test_module == NULL) {
                fprintf(stderr, "Unknown test module %s\n", item[ix]);
                return EXIT_FAILURE;
            }
        }
    }

    if (count == 0 || skip) {
        struct Test *test = tests;
        do {
            failed_tests += test_module_execution(beam, test);
            test++;
        } while (test->test_module);
    }

    if (failed_tests == 0) {
        fprintf(stderr, "Success.\n");
        return EXIT_SUCCESS;
    } else {
        fprintf(stderr, "Failed: %i tests.\n", failed_tests);
        return EXIT_FAILURE;
    }
}

static void usage(const char *name)
{
    fprintf(stdout, "%s: run AtomVM tests\n", name);
    fprintf(stdout, "%s [-h] [-s test1,test2] [-b] [test1 test2...]\n", name);
    fprintf(stdout, "  -h: display this message\n");
    fprintf(stdout, "  -s test1,test2: skip these tests\n");
    fprintf(stdout, "  -b: run tests against BEAM instead of AtomVM (erl in the PATH)\n");
    fprintf(stdout, "  test1 .. test2: specify tests to run (default to all)\n");
}

static void syntax_error(const char *name, const char *message)
{
    fprintf(stderr, "%s: syntax error\n%s\nTry %s -h for help\n", name, message, name);
}

int main(int argc, char **argv)
{
    char *name = argv[0];
    time_t seed = time(NULL);
    fprintf(stderr, "Seed is %li\n", seed);
    srand(seed);

    if (chdir(dirname(name)) != 0) {
        perror("Error: ");
        return EXIT_FAILURE;
    }

    int opt;
    bool beam = false;
    bool skip = false;
    char *skip_list = NULL;
    while ((opt = getopt(argc, argv, "hbs:")) != -1) {
        switch (opt) {
            case 'b':
                beam = true;
                break;
            case 's':
                skip_list = optarg;
                break;
            case 'h':
                usage(name);
                return 0;
            case ':':
            case '?':
                syntax_error(name, "Unknown option");
                return 1;
        }
    }
    int count = argc - optind;
    char **list = argv + optind;
    char **allocated_skip_list = NULL;
    if (count > 1 && skip_list != NULL) {
        syntax_error(name, "Option -s is incompatible with module names");
    }
    if (skip_list) {
        skip = true;
        // tokenize the list, first counting elements.
        char *skip_list_c = skip_list;
        count = 1;
        while (*skip_list_c) {
            if (*skip_list_c == ',') {
                count++;
            }
            skip_list_c++;
        }
        allocated_skip_list = malloc(count * sizeof(char *));
        int ix = 0;
        char *token = strtok(skip_list, ",");
        while (token && ix < count) {
            allocated_skip_list[ix++] = token;
            token = strtok(NULL, ",");
        }
        list = allocated_skip_list;
    }

    int result = test_modules_execution(beam, skip, count, list);
    if (allocated_skip_list) {
        free(allocated_skip_list);
    }
    return result;
}
