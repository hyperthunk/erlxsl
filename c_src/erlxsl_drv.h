/*
 * erlxsl_drv.h
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
 */

#include <erl_driver.h>
#include <ei.h>
#include <stdarg.h>
#include "erlxsl.h"

/* INTERNAL DATA & DATA STRUCTURES */

typedef enum {
  Success,
  InitOk,
  LibraryNotFound,
  EntryPointNotFound,
  InitFailed,
  OutOfMemory,
  UnknownCommand
} DriverState;

// typedef void InitEngineFunc(xsl_engine* engine);
typedef void (*init_func)(xsl_engine*);

typedef struct {
  char* name;
  char* error_message;
  void* library;
  init_func init_f;
} loader_spec;

typedef struct {
  void*  port;
  xsl_engine* engine;
  loader_spec* loader;
} driver_data;

typedef struct {
  void* port;
  XslEngine* engine;
  loader_spec* loader;
} DriverHandle;

/*
 * Stores three headers used to identify the kind of input uris 
 * (e.g. file or buffer/memory) and the number of parameters being
 * supplied (arity).
 */
typedef struct {
  /* The position in which we're expecting the input kind value. */
  Int8 input_kind;
  /* The position in which we're expecting the xsl input kind value. */
  Int8 xsl_kind;
  /*
   * The position in which we're expecting a value
   * denoting the number of parameters being supplied.
   */
  Int16 param_grp_arity;
} input_spec_hdr;

/*
 * Headers that specify the data size for the payload (i.e. the size
 * of the input and stylesheet buffers).
 */
typedef struct  {
  /* The position in which we're expecting the input size marker value */
  Int32 input_size;
  /* The position in which we're expecting the xsl size marker value */
  Int32 xsl_size;
} payload_size_hdr;

/*
 * Contains a structural outline of a single argument within the 
 * request buffer, specifying the size of the argument name and value buffers.
 */
typedef struct request_buffer_argument_outline {
  /* size of the argument name string in the buffer. */
  Int16  name_size;
  /* size of the argument value string in the buffer. */
  Int16  value_size;
} arg_spec_hdr;

static ErlDrvTermData atom_result; 
static ErlDrvTermData atom_error;
static const char *init_entry_point = "init_engine";
static const char *unknown_command = "Unknown Command!";
static const char *heap_space_exhausted = "Out of Memory!";

/* FORWARD DEFS */

static DriverState init_provider(driver_data*, char*);
static void cleanup_task(void*);
static void apply_transform(void*);
static ErlDrvData start_driver(ErlDrvPort, char*);
static void stop_driver(ErlDrvData);
static int call(ErlDrvData, unsigned int, char*, int, char**, int, unsigned int*);
static void outputv(ErlDrvData, ErlIOVec*);
static void ready_async(ErlDrvData, ErlDrvThreadData);
static ErlDrvTermData* make_driver_term(ErlDrvPort*, char*, ErlDrvTermData*, long*);
static ErlDrvTermData* make_driver_term_bin(ErlDrvPort*, ErlDrvBinary*, ErlDrvTermData*, long*);

#define INIT_COMMAND (UInt32)9

#define DRV_FREE(x) if (x != NULL) driver_free(x)
