/*
 * erlxsl_drv.c
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
 * This driver is intended for dynamic rather than static linking. The erlang
 * runtime should be started with the +A thread pool size option to ensure that 
 * the driver can take advantage of the async driver apis.
 *
 * The runtime should also be started with a maximum stack space argument,
 * to ensure we don't run out of stack space prematurely!
 *
 */

#include <erl_driver.h>
#include <stdarg.h>
#include "erlxsl_api.h"

/* INTERNAL DATA & DATA STRUCTURES */

typedef struct {
  ErlDrvPort  port;
	xsl_engine* provider;
} driver_spec;

static ErlDrvTermData atom_result; 
static ErlDrvTermData atom_error;

/* FORWARD DEFS */

static void init_provider(driver_spec*, char *);
static DriverState default_initialize(void*);
static void default_handleTransform(void*);
static DriverState default_postHandle(void*);
static void default_shutdown(void*);
static ErlDrvData start_driver(ErlDrvPort, char*);
static void stop_driver(ErlDrvData);
static void outputv(ErlDrvData, ErlIOVec*);
static void ready_async(ErlDrvData, ErlDrvThreadData);
static void cleanup_job(transform_job*);
static ErlDrvTermData* make_driver_term(ErlDrvPort*, char*, ErlDrvTermData*, long*);
static ErlDrvTermData* make_driver_term_bin(ErlDrvPort*, ErlDrvBinary*, ErlDrvTermData*, long*);

#define DRV_FREE(x) if (NULL != x) driver_free(x)

#define READ_Int32(s)  ((((int)(((unsigned char*) (s))[0]))  << 24) | \
                        (((int)(((unsigned char*) (s))[1]))  << 16) | \
                        (((int)(((unsigned char*) (s))[2]))  << 8)  | \
                        (((int)(((unsigned char*) (s))[3]))))

/* INTERNAL DRIVER FUNCTIONS */

//static void* try_driver_alloc(size_t size, void* pfree, ...) {
	
//}

static void init_provider(driver_spec *drv, char *buff) {
	xsl_engine *engine = (xsl_engine*)driver_alloc(sizeof(xsl_engine));
	if (engine == NULL) {
		return;
	}
	engine->initialize = default_initialize;
	engine->transform = default_handleTransform;
	engine->after_transform = default_postHandle;
	engine->shutdown = default_shutdown;
	
	// NB: this is a temporary structure for testing/discovery
	drv->provider = engine;
};

static DriverState default_initialize(void *state) {
	return Ok;
};

static void default_handleTransform(void *result) {
  transform_result *res = (transform_result*)result;
	transform_job *job = (res->context)->job;
	char *output = driver_alloc(sizeof(char) * ((strlen(job->input) + strlen(job->stylesheet)) + 1)); 
	strcpy(output, job->input);
	strcpy(output, job->stylesheet);
  res->payload.buffer = output;
  res->status = Ok;
};

static DriverState default_postHandle(void *result) {
  return Ok;
};

static void default_shutdown(void *state) {};

static void cleanup_job(transform_job* job) {
	param_info *next; 
	param_info *current;
	
  if (job != NULL) {
		DRV_FREE(job->input);
		DRV_FREE(job->stylesheet);
    
	  current = job->parameters;
		while (current != NULL) {
		  next = (param_info*)current->next;
		  DRV_FREE(current->key);
		  DRV_FREE(current->value);
		  current = next;
		}
	
		DRV_FREE(job);
  }
};

/* makes a tagged tuple (using the driver term format) for the supplied binary payload. */
static ErlDrvTermData* make_driver_term_bin(ErlDrvPort *port, ErlDrvBinary *payload, ErlDrvTermData *tag, long *length) {
	ErlDrvTermData *term;
	ErlDrvTermData  spec[10];
	
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

  term = malloc(sizeof(spec));
	if (NULL != term) {
		memcpy(term, &spec, sizeof(spec));
		*length = sizeof(spec) / sizeof(spec[0]);
	}
	return term;	
};

static ErlDrvTermData* make_driver_term(ErlDrvPort *port, char *payload, ErlDrvTermData *tag, long *length) {
	ErlDrvTermData *term;
	ErlDrvTermData  spec[9];
	
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

  term = malloc(sizeof(spec));
	if (NULL != term) {
		memcpy(term, &spec, sizeof(spec));
		*length = sizeof(spec) / sizeof(spec[0]);
	}
	return term;
};

/* DRIVER CALLBACK FUNCTIONS */

static ErlDrvData start_driver(ErlDrvPort port, char *buff) {
    
	atom_result = driver_mk_atom("result");
	atom_error  = driver_mk_atom("error");

  // avoid total madness! nice tip that one...
  if (port == NULL) {
      return ERL_DRV_ERROR_GENERAL;
  }

  driver_spec *d = (driver_spec*)driver_alloc(sizeof(driver_spec));
	if (d == NULL) {
		return ERL_DRV_ERROR_GENERAL; // TODO: use ERL_DRV_ERROR_ERRNO and provide out-of-memory info
	}
  d->port = port;    

	init_provider(d, buff);
	if (d->provider == NULL) {
		return ERL_DRV_ERROR_GENERAL; // TODO: use ERL_DRV_ERROR_ERRNO and provide out-of-memory info
	}
	
	DriverState state = (d->provider)->initialize(d);
	if (state == Ok) {
		return (ErlDrvData)d;
	} else {
		return ERL_DRV_ERROR_GENERAL; // TODO: use ERL_DRV_ERROR_ERRNO and provide more info
	}
}

static void stop_driver(ErlDrvData drv_data) {
  // give the provider a chance to clean up
  driver_spec *d = (driver_spec*)drv_data;
	ErlDrvPort port = d->port;
	xsl_engine *engine = d->provider;
	void *state = &port;
	engine->shutdown(state);

  // driver cleanup
	driver_free((char*)engine);
  driver_free((char*)drv_data);
}

static void outputv(ErlDrvData drv_data, ErlIOVec *ev) {
	char *error_msg;

	driver_spec *d = (driver_spec*)drv_data;
	ErlDrvPort port = d->port;
	ErlDrvTermData callee_pid = driver_caller(port);
	ErlDrvBinary *bin = ev->binv[1];
	xsl_engine *engine = d->provider;
	// assert(ev->binv[1] != NULL)	
	
	int bin_size = bin->orig_size;
	if (bin_size < 5) {
		error_msg = "InconsistentInputHeaders: driver protocol not recognised.";
		driver_output2(port, error_msg, strlen(error_msg), NULL, 0);
		return;
	}
	
	int offset = 1;
	int xml_kind = READ_Int32(&(bin->orig_bytes[++offset]));
	int xsl_kind = READ_Int32(&(bin->orig_bytes[++offset]));
	int xml_len = READ_Int32(&(bin->orig_bytes[++offset]));
	int xsl_len = READ_Int32(&(bin->orig_bytes[++offset]));
	int param_count = READ_Int32(&(bin->orig_bytes[++offset]));
	
	if (xml_len > (bin_size - 5)) {
		// NB: I'm being lazy an reusing bin_size as the term length parameter here
		error_msg = "BufferSizeMismatch: input length exceeds stated buffer size.";
		driver_output2(port, error_msg, strlen(error_msg), NULL, 0);
		return;
	}
	if (xsl_len > (bin_size - (5 + xml_len))) {
		error_msg = "BufferSizeMismatch: stylesheet length exceeds stated buffer size.";
		driver_output2(port, error_msg, strlen(error_msg), NULL, 0);
		return;
	}
	// TODO: add support for parameters also!
	
	char *xml = driver_alloc((sizeof(char) * xml_len) + 1);
	if (xml == NULL) {
		driver_failure_atom(port, "system_limit");
		return;
	}
	
	char *xsl = driver_alloc((sizeof(char) * xsl_len) + 1);
	if (xsl == NULL) {
		DRV_FREE(xml);
		driver_failure_atom(port, "system_limit");
		return;
	}
	
	transform_job *job = (transform_job*)driver_alloc(sizeof(transform_job));
	if (job == NULL) {
		DRV_FREE(xml);
		DRV_FREE(xsl);
		driver_failure_atom(port, "system_limit");
		return;
	}
	
	request_context *ctx = (request_context*)driver_alloc(sizeof(request_context));
	if (ctx == NULL) {
		DRV_FREE(xml);
		DRV_FREE(xsl);
		DRV_FREE(job);
		driver_failure_atom(port, "system_limit");
		return;
	}
	
	transform_result *result = (transform_result*)driver_alloc(sizeof(transform_result));
	if (result == NULL) {	
		DRV_FREE(xml);
		DRV_FREE(xsl);
		DRV_FREE(job);
		DRV_FREE(ctx);
		driver_failure_atom(port, "system_limit");
		return;
	}
	
	// this approach only works when we're running with SMP support! Check by calling driver_system_info()
	// TODO: either mutex this for non-SMP systems or do a copy operation instead
	// driver_binary_inc_refc(bin);
	
	// we save the copying until we're sure it'll take place...
	strncpy(xml, &bin->orig_bytes[offset], xml_len);
	offset += xml_len;
	strncpy(xsl, &bin->orig_bytes[offset], xsl_len);
	
	job->input_kind = xml_kind;
	job->xsl_kind = xsl_kind;
	job->input = xml;
	job->stylesheet = xsl;
	ctx->port = port;
	ctx->caller_pid = callee_pid;
	ctx->job = job;
	result->context = ctx;
	
	// TODO: consider using an explicit cleanup function instead of relying solely on ready_async and engine->after_transform
	driver_async(port, NULL, engine->transform, result, NULL); //, engine->postHandle);
};

/*
 * Processes the supplied transform_result and sends it to the appropriate
 * erlang process (e.g. the specified receiver)
 */
static void ready_async(ErlDrvData drv_data, ErlDrvThreadData async_data) {
	long 								response_len;
	ErlDrvTermData* 		term;
	ErlDrvTermData 			callee_pid, tag;
	ErlDrvPort 					port;
	driver_spec* 				driverBundle;
	xsl_engine* 				provider;
	transform_result* 	result;
	request_context* 		context;
	transform_job* 			job;				// used to repeatedly refer to the completed job 
	DriverState					state;
	
	// ErlDrvPort port, ErlDrvTermData receiver, ErlDrvTermData* term, int n
	
	driverBundle 	= (driver_spec*)drv_data;
	provider	 		= (xsl_engine*)driverBundle->provider;
  port		 	 		= driverBundle->port;
  result 		 		= (transform_result*)async_data;
	context 	 		= result->context;
	job		 	 			= context->job;
	state		 			= result->status;
	port 		 			= (ErlDrvPort)context->port;
	callee_pid 	 	= (ErlDrvTermData)context->caller_pid;
	
	switch (state) {
	case Ok:
		tag	= atom_result;
		if (result->format == Text) {
			term = make_driver_term(&port, result->payload.buffer, &tag, &response_len);
		} else {
			term = make_driver_term_bin(&port, ((ErlDrvBinary*)result->payload.data), &tag, &response_len);
		}
		break;
	default:  // TODO: it would be better if we didn't do "everthing else is an error" here
		tag = atom_error;
		term = make_driver_term(&port, result->errorMessage, &tag, &response_len);
	}
	
	if (term != NULL) {
		driver_send_term(port, callee_pid, term, response_len);
	} else {
		state = OutOfMemory;
	}

	// now the engine needs the opportunity to free up the result
  provider->after_transform(result);
    
	// cleanup time...
	cleanup_job(job);
	
	if (state == OutOfMemory) {
		driver_failure_atom(port, "system_limit");
	}
	// TODO: and the other failure conditions?
};

/* DRIVER API EXPORTS */

static ErlDrvEntry driver_entry = {
	NULL,                       /* init */
	start_driver,               /* start, called when port is opened */
	stop_driver,                /* stop, called when port is closed */
	NULL,                       /* output, called when port receives messages */
	NULL,                       /* ready_input, called when input descriptor ready to read*/
	NULL,                       /* ready_output, called when output descriptor ready to write */
	"erlxsl_drv",               /* the name of the driver */
	NULL,                       /* finish, called when unloaded */ //TODO: should we do resource disposal here!?
	NULL,                       /* handle,  */
	NULL,                       /* control */
	NULL,                       /* timeout */
	outputv,                    /* outputv */
	ready_async,                /* ready_async, called (from the emulator thread) after an asynchronous call has completed. */
	NULL,                       /* flush */
	NULL,                       /* call */
	NULL                        /* event */
};

DRIVER_INIT(erlxsl_drv) {
  return &driver_entry;
}
