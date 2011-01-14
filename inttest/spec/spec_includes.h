/*
 * init_types.spec
 * 
 * -----------------------------------------------------------------------------
 * Copyright (c) 2008-2010 Tim Watson (watson.timothy@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 * -----------------------------------------------------------------------------
 * Notes: If you're looking at a version of this file with the extension .c then
 * you're looking at generated code. For the original test specifications, please
 * look at the file with the .spec extension instead.
 */

#ifndef _SPEC_INCL_H
#define _SPEC_INCL_H

#include "cspec.h"
#include <stdbool.h>

bool allow_allocations = true;
size_t allow_allocation_sizes = -1;
char assert_failed[2046];

// overwrite ALLOC macro

void *_spec_alloc(size_t size) {
  if (allow_allocations == false) return NULL;
  if (allow_allocation_sizes == -1 || allow_allocation_sizes == size) {
    return malloc(size);
  } else {
    return NULL;
  }
};

#define ALLOC(size) _spec_alloc(size)

#define do_with_nomem(stmt) \
  allow_allocations = false; stmt; \
  allow_allocations = true

#define with_denied_allocation(size, stmt)  \
  allow_allocation_sizes = size; stmt; \
  allow_allocation_sizes = -1;

#define create_test_data(Out, In)    \
  char *Out = ALLOC(sizeof(char) * strlen(In));  \
  strcpy(Out, In)

#define match_be_equal_to(A, E) \
  (strncmp(A, E, strlen(E)) == 0)

// override 'assert' behaviour

#define _TEST_ASSERT true

#define ASSERT(_s)  \
  (!(_s) \
    ? (strcpy(assert_failed, #_s))  \
    : ((void) 0))

void _spec_free(void *x) {
  if (x != NULL) {
    free(x);
  }
};

#define _DRV_FREE
#define DRV_FREE(x) \
  do { DBG("Free " #x " [%p]", x); \
    _spec_free(x); } while (false)

#include "erlxsl_port.h"

#endif /* _SPEC_INCL_H */
