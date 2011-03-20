/*
 * erlxsl_driver.h
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
 * Notes:
 *
 * This header contains code specific to the shared object library used by the
 * linked-in driver. It depends on erl_driver and ei. This header *must* be
 * included before the erlxsl_internal header.
 *
 */

#ifndef _ERLXSL_PRT_H
#define  _ERLXSL_PRT_H

// driver_alloc wrapper
#ifndef ALLOC
#define ALLOC(size) malloc(size)
#endif

#ifndef REALLOC
#define REALLOC(ptr, size) realloc(ptr, size)
#endif

// magics for command identification
#define INIT_COMMAND (UInt32)9
#define ENGINE_COMMAND (UInt32)7

// NULL safe driver_free wrapper
#ifndef _DRV_FREE
#define _DRV_FREE
#define DRV_FREE(x) if (x != NULL) free(x)
#endif

// Cause the driver to fail (e.g., exit/unload)
#define FAIL(p, msg)  \
    ERROR(msg);          \
    exit(1);

#include "erlxsl.h"
/* we want all the internal utility functions and any additional includes now. */
#include "erlxsl_internal.h"

#endif /* _ERLXSL_PRT_H */
