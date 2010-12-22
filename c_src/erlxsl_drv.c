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
static void cleanup_job(transform_job* job);

#define DRV_FREE(x) if (NULL != x) driver_free(x)

#define READ_Int32(s)  ((((int)(((unsigned char*) (s))[0]))  << 24) | \
                        (((int)(((unsigned char*) (s))[1]))  << 16) | \
                        (((int)(((unsigned char*) (s))[2]))  << 8)  | \
                        (((int)(((unsigned char*) (s))[3]))))

/* INTERNAL DRIVER FUNCTIONS */

static void init_provider(driver_spec* drv, char* buff) {
	xsl_engine* engine = (xsl_engine*)driver_alloc(sizeof(xsl_engine));
	if (engine == NULL) {
		return;
	}
	engine->initialize = default_initialize;
	engine->transform = default_handleTransform;
	engine->afterTransform = default_postHandle;
	engine->shutdown = default_shutdown;
	
	// NB: this is a temporary structure for testing/discovery
	drv->provider = engine;
};

static DriverState default_initialize(void* state) {
	return Ok;
};

static void default_handleTransform(void* result) {
  transform_result* res = (transform_result*)result;
	char* data;
  data = "<response><name>This is very silly!</name><value>NUTS</value></response>";
  res->payload = data;
  res->status = Ok;
};

static DriverState default_postHandle(void* result) {
  return Ok;
};

static void default_shutdown(void* state) {};

static void cleanup_job(transform_job* job) {
	param_info* next; 
	param_info* current;
	
  if (NULL != job) {
		DRV_FREE(job->inputData);
		DRV_FREE(job->stylesheetData);
    
	  current = job->parameters;
		while (NULL != current) {
		  next = (param_info*)current->next;
		  DRV_FREE(current->name);
		  DRV_FREE(current->value);
		  current = next;
		}
	
		DRV_FREE(job);
  }
}

/* DRIVER CALLBACK FUNCTIONS */

static ErlDrvData start_driver(ErlDrvPort port, char *buff) {
    
	atom_result = driver_mk_atom("result");
	atom_error  = driver_mk_atom("error");

  // avoid total madness! nice tip that one...
  if (port == NULL) {
      return ERL_DRV_ERROR_GENERAL;
  }

  driver_spec* d = (driver_spec*)driver_alloc(sizeof(driver_spec));
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
  driver_spec* d = (driver_spec*)drv_data;
	ErlDrvPort port = d->port;
	xsl_engine* engine = d->provider;
	void* state = &port;
	engine->shutdown(state);

  // driver cleanup
	driver_free((char*)engine);
  driver_free((char*)drv_data);
}

static void outputv(ErlDrvData drv_data, ErlIOVec *ev) {
	driver_spec* d = (driver_spec*)drv_data;
	ErlDrvPort port = d->port;
	xsl_engine* engine = d->provider;
	
	int xmlKind, xslKind;
	int xmlLen, xslLen;
	int paramCount;
	
	xmlKind = READ_Int32(&(ev->binv[1]->orig_bytes[1]));
	xslKind = READ_Int32(&(ev->binv[1]->orig_bytes[2]));
	xmlLen  = READ_Int32(&(ev->binv[1]->orig_bytes[3]));
	xslLen  = READ_Int32(&(ev->binv[1]->orig_bytes[4]));
	
	driver_async(port, NULL, engine->transform, ev, NULL); //, engine->postHandle);
};

/*
 * Processes the supplied transform_result and sends it to the appropriate
 * erlang process (e.g. the specified receiver)
 */
static void ready_async(ErlDrvData drv_data, ErlDrvThreadData async_data) {
	int 								response_len;
	ErlDrvTermData* 		term;
	ErlDrvTermData 			calleePid, tag;
	ErlDrvPort 					port;
	driver_spec* 				driverBundle;
	xsl_engine* 				provider;
	transform_result* 	result;
	request_context* 		context;
	void* 							payload;		// used to process the payload
	transform_job* 			job;				// used to repeatedly refer to the completed job 
	DriverState					state;
	ErlDrvTermData 			spec[4];
	
	// ErlDrvPort port, ErlDrvTermData receiver, ErlDrvTermData* term, int n
	
	driverBundle 	= (driver_spec*)drv_data;
	provider	 		= (xsl_engine*)driverBundle->provider;
  port		 	 		= driverBundle->port;
  result 		 		= (transform_result*)async_data;
	context 	 		= result->context;
	job		 	 			= context->job;
	state		 			= result->status;
	port 		 			= (ErlDrvPort)context->port;
	calleePid 	 	= (ErlDrvTermData)context->callerPid;
	
	switch (state) {
	case Ok:
		payload = result->payload;
		tag	= atom_result;
		break;
	default:  // TODO: it would be better if we didn't do "everthing else is an error" here
		payload = result->errorMessage;
		tag = atom_error;
	}
	
	spec[0] = ERL_DRV_ATOM, tag;
	spec[1] = ERL_DRV_PORT, driver_mk_port(port);
  //isn't this next component an implicit ErlDrvBinary and therefore requires a call to driver_free_binary?
	if (result->format == Binary) {
		spec[2] = ERL_DRV_BINARY, (ErlDrvBinary*)payload, (ERL_DRV_UINT, result->size);
	} else {
		spec[2] = ERL_DRV_BUF2BINARY, (char*)payload, strlen((char*)payload);
	}
	spec[4] = ERL_DRV_TUPLE, 3;

  term = malloc(sizeof(spec));
	
	if (NULL != term) {
		memcpy(term, &spec, sizeof(spec));
		response_len = sizeof(spec) / sizeof(spec[0]);
		driver_send_term(port, calleePid, term, response_len);
	} else {
		state = OutOfMemory;
	}

	// now the engine needs the opportunity to free up the result
  provider->afterTransform(result);
    
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
