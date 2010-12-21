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

/* erts includes */
#include <erl_driver.h>

/* api include */
#include "erlxsl_api.h"

/* internal data structures */

typedef struct {
    ErlDrvPort  port;
	xsl_engine* provider;
} driver_spec;

/* erts driver callbacks */

static ErlDrvData start_driver(ErlDrvPort port, char *buff) {
    
    // avoid total madness! nice tip that one...
    if (port == NULL) {
        return ERL_DRV_ERROR_GENERAL;
    }

    // configure erts
    driver_spec* d = (driver_spec*)driver_alloc(sizeof(driver_spec));
	if (d == NULL) {
		return ERL_DRV_ERROR_GENERAL; // TODO: use ERL_DRV_ERROR_ERRNO and provide out-of-memory info
	}
    d->port = port;    

	init_provider(d, buff);
	if (d->engine == NULL) {
		return ERL_DRV_ERROR_GENERAL; // TODO: use ERL_DRV_ERROR_ERRNO and provide out-of-memory info
	}
	DriverState state = (d->engine)->initialize();
	if (state == DriverState.Ok) {
		return (ErlDrvData)d;
	} else {
		return ERL_DRV_ERROR_GENERAL; // TODO: use ERL_DRV_ERROR_ERRNO and provide more info
	}
}

static void stop_driver(ErlDrvData handle) {

    // give the provider a chance to clean up
    // destroy_provider();

    // cleanup for erts
    driver_free((char*)handle);
}

/* 
 
This output handler function is called from erlang with erlang:port_command(Port, Payload).
static void output(ErlDrvData handle, char *buff, int bufflen) {
    driver_spec* globalDriverSpecData;
    ErlDrvPort port;
    
    globalDriverSpecData = (driver_spec*)handle;
    port = globalDriverSpecData->port;
    
#ifdef DEBUG
    fprintf(stderr, "Driver received buffer of size %i\n", ((char *)&bufflen));
#endif
    
    ErlDrvTermData callerPid = driver_caller(port);
    
    RequestContext* context;
    context = driver_alloc(sizeof(RequestContext));
    if (NULL == context) {
        E_OUT_OF_MEMORY;
        return;
    }
    
    context->port = port;
    context->callerPid = callerPid;
    TransformRequestPtr request = unpackRequest((const char *)buff, bufflen);
    if (NULL == request) { //NB: this only happens when the heap is exhausted!
        releaseContext(context);
        E_OUT_OF_MEMORY;
        return;
    }
    
    context->request = request;    
    TransformResponse* response = malloc(sizeof(TransformResponse));
    if (NULL == response) {
        releaseContext(context);
        E_OUT_OF_MEMORY;
        return;
    }
    
    response->context = context;
    long taskHandle = driver_async(port, NULL, handle_request, response, NULL);
}

*/

static void outputv(ErlDrvData drv_data, ErlIOVec *ev) {
	driver_spec* d = (driver_spec*)drv_data;
	ErlDrvPort port = d->port;
	xsl_engine* engine = d->provider;
	long taskHandle = driver_async(port, NULL, engine->handleTransform, ev, NULL); //, engine->postHandle);
};

static void ready_async(ErlDrvData drv_data, ErlDrvThreadData async_data) {
    driver_spec* driverBundle = (driver_spec*)drv_data;
    ErlDrvPort port = driverBundle->port;
    
    TransformResponse* response = (TransformResponse*)async_data;
    sendResponse(response, NULL);    

    post_handle_request(response);
    releaseResponse(response);
};

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

static void init_provider(driver_spec* drv, char* buff) {
	xsl_engine* engine = (xsl_engine*)driver_alloc(sizeof(xsl_engine));
	if (engine == NULL) {
		return;
	}
	engine->initialize = default_initialize;
	engine->handleTransform = default_handleTransform;
	engine->postHandle = default_postHandle;
	engine->shutdown = default_shutdown;
	
	// NB: this is a temporary structure for testing/discovery
	d->provider = engine;
};

static DriverState default_initialize(void* state) {
	return DriverState.Ok;
};

static void default_handleTransform(transform_result* result) {
	result->responseFormat = ResponseFormat.Buffer;
	result->payload = "<response><name>it works!</name><value>WOW</value></response>";
	SET_STATUS(result, DriverState.Ok);
};

static DriverState default_postHandle(transform_result* result) {
	return DriverState.Ok;
};

static void default_shutdown(void* state) {
	ErlDrvPort p = (ErlDrvPort)state;  // just a sanity check! 
};

DRIVER_INIT(erlxsl_drv) {
    return &driver_entry;
}