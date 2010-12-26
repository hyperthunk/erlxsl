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
#include "erlxsl_api.h"

/* INTERNAL DATA & DATA STRUCTURES */

typedef struct {
  void*  port;
	xsl_engine* provider;
} driver_spec;

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

/* FORWARD DEFS */

static void init_provider(driver_spec*, char *);
static DriverState default_initialize(void*);
static void default_handleTransform(void*);
static DriverState default_postHandle(void*);
static void default_shutdown(void*);
static void cleanup_task(void*);
static ErlDrvData start_driver(ErlDrvPort, char*);
static void stop_driver(ErlDrvData);
static int call(ErlDrvData, unsigned int, char*, int, char**, int, unsigned int*);
static void outputv(ErlDrvData, ErlIOVec*);
static void ready_async(ErlDrvData, ErlDrvThreadData);
static ErlDrvTermData* make_driver_term(ErlDrvPort*, char*, ErlDrvTermData*, long*);
static ErlDrvTermData* make_driver_term_bin(ErlDrvPort*, ErlDrvBinary*, ErlDrvTermData*, long*);

#define INIT_COMMAND_MAGIC 9

#define DRV_FREE(x) if (NULL != x) driver_free(x)

#define READ_Int32(s)  ((((int)(((unsigned char*) (s))[0]))  << 24) | \
                        (((int)(((unsigned char*) (s))[1]))  << 16) | \
                        (((int)(((unsigned char*) (s))[2]))  << 8)  | \
                        (((int)(((unsigned char*) (s))[3]))))

#define ERROR(str, ...)  \
    LOG(stderr, str, ##__VA_ARGS__);

#define INFO(str, ...)  \
    LOG(stdout, str, ##__VA_ARGS__);

#define LOG(stream, str, ...)  \
    fprintf(stream, str, ##__VA_ARGS__);
