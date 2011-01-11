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
char assert_failed[2046];

// overwrite ALLOC macro

void *_spec_alloc(size_t size) {
  return ((allow_allocations == true) ? malloc(size) : NULL);
};

#define ALLOC(size) _spec_alloc(size)

#define do_with_nomem(stmt) \
  allow_allocations = false; stmt; \
  allow_allocations = true

// override 'assert' behaviour

#define _TEST_ASSERT true

#define assert(_s)  \
  (!(_s) \
    ? (strcpy(assert_failed, #_s))  \
    : ((void) 0))

#include "erlxsl.h"
#include "erlxsl_port.h"
#include "erlxsl_internal.h"

#endif /* _SPEC_INCL_H */
