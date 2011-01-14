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

#ifndef _ERLXSL_DRV_H
#define  _ERLXSL_DRV_H

#include <erl_driver.h>
#include <ei.h>

/* INTERNAL DATA & DATA STRUCTURES */

static ErlDrvTermData atom_result; 
static ErlDrvTermData atom_error;

/* LINKED-IN DRIVER SPECIFIC MACROS - MUST BE SPECIFIED BEFORE INCLUDING INTERNAL FUNCTIONS/TYPES */

// driver_alloc wrapper
#define ALLOC(size) driver_alloc(size)

#define REALLOC(ptr, size) driver_realloc(ptr, size)

// magics for command identification
#define INIT_COMMAND (UInt32)9
#define ENGINE_COMMAND (UInt32)7

// NULL safe driver_free wrapper
#define DRV_FREE(x) if (x != NULL) driver_free(x)

// Cause the driver to fail (e.g., exit/unload)
#define FAIL(p, msg) driver_failure_atom(p, msg)

/* FORWARD DEFINES */

/* makes a tagged tuple (using the driver term format) for the supplied binary payload. */
static ErlDrvTermData* make_driver_term_bin(ErlDrvPort*, ErlDrvBinary*, ErlDrvTermData*, long*);

/* makes a tagged tuple (using the driver term format) for the supplied buffer payload. */
static ErlDrvTermData* make_driver_term(ErlDrvPort*, char*, ErlDrvTermData*, long*);

/* grab the API functions... */
#include "erlxsl.h"

/* we want all the internal utility functions and any additional includes now. */
#include "erlxsl_internal.h"

/* INTERNAL UTILITY FUNCTIONS */

static ErlDrvTermData* 
make_driver_term_bin(ErlDrvPort *port, ErlDrvBinary *payload, ErlDrvTermData *tag, long *length) {
  ErlDrvTermData *term;
  ErlDrvTermData  spec[10];
  term = ALLOC(sizeof(spec));
  if (term == NULL) return NULL;
  
  spec[0] = ERL_DRV_ATOM;
  spec[1] = *tag;
  spec[2] = ERL_DRV_PORT;
  spec[3] = driver_mk_port(*port);
  spec[4] = ERL_DRV_BINARY; 
  spec[5] = (ErlDrvTermData)payload; 
  spec[6] = ERL_DRV_UINT;
  spec[7] = payload->orig_size;
  spec[8] = ERL_DRV_TUPLE; 
  spec[9] = 3;
  
  memcpy(term, &spec, sizeof(spec));
  *length = sizeof(spec) / sizeof(spec[0]);
  return term;  
};

static ErlDrvTermData* 
make_driver_term(ErlDrvPort *port, char *payload, ErlDrvTermData *tag, long *length) {
  ErlDrvTermData *term;
  ErlDrvTermData  spec[9];
  term = ALLOC(sizeof(spec));
  if (term == NULL) return NULL;
  
  spec[0] = ERL_DRV_ATOM;
  spec[1] = *tag;
  spec[2] = ERL_DRV_PORT;
  spec[3] = driver_mk_port(*port);
  /*if (result->format == Binary) {
    spec[4] = ERL_DRV_BINARY; 
    spec[5] = (ErlDrvBinary*)payload; 
    spec[6] = ERL_DRV_UINT;
    spec[7] = result->size;
  } else {*/
  spec[4] = ERL_DRV_BUF2BINARY;
  spec[5] = payload;
  spec[6] = strlen(payload);
  /*}*/
  spec[7] = ERL_DRV_TUPLE; 
  spec[8] = 3;
  
  memcpy(term, &spec, sizeof(spec));
  *length = sizeof(spec) / sizeof(spec[0]);
  return term;
};

#endif  /* _ERLXSL_DRV_H */
