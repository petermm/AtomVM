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
#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <reent.h>
#include <errno.h>
#include <string.h>

extern const openc6_abi_t *openc6_abi;

// Return newlib's global pre-initialized reentrancy structure
struct _reent *__getreent(void) {
    extern struct _reent *_impure_ptr;
    return _impure_ptr;
}

// Memory APIs mapped to openC6 heap
extern void *sys_malloc(size_t size);
extern void sys_free(void *ptr);

void *malloc(size_t size) {
    if (size == 0 || size > UINT32_MAX - 16) return NULL;
    void *raw = sys_malloc(size + 16);
    if (!raw) return NULL;
    
    uintptr_t raw_addr = (uintptr_t)raw;
    uintptr_t aligned_addr = (raw_addr + 8 + 7) & ~7;
    void *aligned = (void *)aligned_addr;
    
    uint32_t *size_storage = (uint32_t *)aligned - 2;
    *size_storage = (uint32_t)size;
    
    void **orig_storage = (void **)aligned - 1;
    *orig_storage = raw;
    
    return aligned;
}

void free(void *ptr) {
    if (!ptr) return;
    void **orig_storage = (void **)ptr - 1;
    void *raw = *orig_storage;
    sys_free(raw);
}

void *realloc(void *ptr, size_t size) {
    if (!ptr) return malloc(size);
    if (size == 0) {
        free(ptr);
        return NULL;
    }
    if (size > UINT32_MAX - 16) return NULL;
    uint32_t *size_storage = (uint32_t *)ptr - 2;
    size_t old_size = *size_storage;
    if (size <= old_size) {
        return ptr;
    }
    void *new_ptr = malloc(size);
    if (!new_ptr) return NULL;
    memcpy(new_ptr, ptr, old_size);
    free(ptr);
    return new_ptr;
}

void *calloc(size_t nmemb, size_t size) {
    if (size != 0 && nmemb > SIZE_MAX / size) return NULL;
    size_t total = nmemb * size;
    void *ptr = malloc(total);
    if (ptr) {
        memset(ptr, 0, total);
    }
    return ptr;
}

// Reentrant Newlib System Call Shims
_ssize_t _write_r(struct _reent *r, int fd, const void *buf, size_t nbyte) {
    (void)r;
    (void)fd;
    if (openc6_abi && nbyte > 0) {
        const char *cptr = (const char *)buf;
        char temp[129];
        size_t written = 0;
        while (written < nbyte) {
            size_t chunk = nbyte - written;
            if (chunk > 128) chunk = 128;
            memcpy(temp, cptr + written, chunk);
            temp[chunk] = '\0';
            openc6_abi->print(temp);
            written += chunk;
        }
    }
    return (_ssize_t)nbyte;
}
_ssize_t _write(int fd, const void *buf, size_t nbyte) {
    return _write_r(_REENT, fd, buf, nbyte);
}

_ssize_t _read_r(struct _reent *r, int fd, void *buf, size_t nbyte) {
    (void)fd; (void)buf; (void)nbyte;
    r->_errno = ENOSYS;
    return -1;
}
_ssize_t _read(int fd, void *buf, size_t nbyte) {
    return _read_r(_REENT, fd, buf, nbyte);
}

_off_t _lseek_r(struct _reent *r, int fd, _off_t offset, int whence) {
    (void)fd; (void)offset; (void)whence;
    r->_errno = ENOSYS;
    return -1;
}
_off_t _lseek(int fd, _off_t offset, int whence) {
    return _lseek_r(_REENT, fd, offset, whence);
}

int _close_r(struct _reent *r, int fd) {
    (void)fd;
    r->_errno = ENOSYS;
    return -1;
}
int _close(int fd) {
    return _close_r(_REENT, fd);
}

int _fstat_r(struct _reent *r, int fd, struct stat *statbuf) {
    (void)fd; (void)statbuf;
    r->_errno = ENOSYS;
    return -1;
}
int _fstat(int fd, struct stat *statbuf) {
    return _fstat_r(_REENT, fd, statbuf);
}

int _isatty_r(struct _reent *r, int fd) {
    (void)fd;
    r->_errno = ENOSYS;
    return 0;
}
int _isatty(int fd) {
    return _isatty_r(_REENT, fd);
}

int _open_r(struct _reent *r, const char *pathname, int flags, int mode) {
    (void)pathname; (void)flags; (void)mode;
    r->_errno = ENOSYS;
    return -1;
}
int _open(const char *pathname, int flags, int mode) {
    return _open_r(_REENT, pathname, flags, mode);
}

int _kill(pid_t pid, int sig) {
    (void)pid; (void)sig;
    errno = ENOSYS;
    return -1;
}

pid_t _getpid(void) {
    return 1;
}

void *_sbrk(ptrdiff_t incr) {
    (void)incr;
    errno = ENOMEM;
    return (void *)-1;
}

void abort(void) {
    if (openc6_abi) {
        openc6_abi->print("AtomVM payload aborted! Resetting system...\n");
        openc6_abi->delay_ms(1000);
        openc6_abi->sys_reset();
    }
    while (1) { __asm__ volatile("nop"); }
}

void __assert_func(const char *file, int line, const char *func, const char *failedexpr) {
    if (openc6_abi) {
        extern int snprintf(char *str, size_t size, const char *format, ...);
        char buf[256];
        snprintf(buf, sizeof(buf), "Assertion failed: %s at %s:%d in %s\n", failedexpr, file, line, func);
        openc6_abi->print(buf);
    }
    abort();
}
