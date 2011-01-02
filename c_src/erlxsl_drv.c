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

#include "erlxsl_drv.h"
#ifdef WIN32
  #include <Windows.h>
#else
  #include <dlfcn.h>
#endif

/* INTERNAL DRIVER FUNCTIONS */

/*
static void* try_driver_alloc(size_t size, void* pfree, ...) {
	void* val = driver_alloc(size);
	if (val == NULL) {
		va_list ap;
		void* p;
		
		va_start(ap, pfree); 
		while ((p = va_arg(ap, void*)) != NULL) {
			DRV_FREE(p);
		}
		va_end(ap);
	}
	return val;
}
*/

#ifdef WIN32
static void* 
dlsym(void* mod, const char *func) {
  HMODULE lib = (HMODULE)mod;
  void *funcp = (void*)GetProcAddress(lib, func);
  return funcp;
};

static int
dlclose(void* handle) {
  FreeLibrary((HMODULE)handle);
};
#endif

static void
load_library(loader_spec* dest) {
#ifdef WIN32
  if ((dest->library = LoadLibrary((const char*)dest->name)) == NULL) {
    DWORD error_code = GetLastError();
    sprintf(dest->error_message, "Unable to load provider library. ErrorCode: ~i", (UInt32)error_code);
  }
#else
  if ((dest->library = dlopen(dest->name, /*RTLD_LAZY*/ RTLD_NOW)) == NULL) {
    dest->error_message = dlerror();
  }
#endif

  if ((dest->init_f = dlsym(dest->library, init_entry_point)) == NULL) {
    sprintf(dest->error_message, init_error_message, dest->name);
  }
};

static DriverInit 
init_provider(driver_data *drv, char *buff) {
	xsl_engine *engine = (xsl_engine*)driver_alloc(sizeof(xsl_engine));
	loader_spec* lib = (loader_spec*)driver_alloc(sizeof(loader_spec));
    
	if (engine == NULL || lib == NULL) {
    DRV_FREE(engine);
    DRV_FREE(lib);
		return OutOfMemory;
	}
	
	drv->loader = lib;
  lib->name = buff;
  load_library(lib);

  if (lib->library == NULL) {
    // TODO: better reporting back to the port controller!?
    puts(lib->error_message);    
    return LibraryNotFound;
  }
  if (lib->init_f == NULL) {
    // TODO: better reporting back to the port controller!?
    puts(lib->error_message);
    return EntryPointNotFound; 
  }
		
  (lib->init_f)(engine);
  if (engine == NULL) {
    return InitFailed;
  }
  
  drv->engine = engine;
  return InitOk;
};

static void 
cleanup_task(void *async_state) {
  async_data* data;
	request_context *ctx;
	transform_job *job;
	param_info *next; 
	param_info *current;
  transform_result* result; 
  
  data = (async_data*)async_state;
  result = data->result;
	
	ctx = result->context;
	job = ctx->job;
	
	if (ctx != NULL) {
		if (job != NULL) {
			INFO("cleanup input\n");			
			DRV_FREE(job->input);
			INFO("cleanup stylesheet\n");
			DRV_FREE(job->stylesheet);

		  current = job->parameters;
			INFO("cleanup parameters\n");
			while (current != NULL) {
				INFO("current wasn't null!?\n");			
			  next = (param_info*)current->next;
			  DRV_FREE(current->key);
			  DRV_FREE(current->value);
			  current = next;
			}
			INFO("cleanup job\n");
			DRV_FREE(job);
	  }
		INFO("cleanup contet\n");
		DRV_FREE(ctx);
	}
	INFO("cleanup result!\n");		
	DRV_FREE(result);
  DRV_FREE(data);
};

/* makes a tagged tuple (using the driver term format) for the supplied binary payload. */
static ErlDrvTermData* 
make_driver_term_bin(ErlDrvPort *port, ErlDrvBinary *payload, ErlDrvTermData *tag, long *length) {
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

static ErlDrvTermData* 
make_driver_term(ErlDrvPort *port, char *payload, ErlDrvTermData *tag, long *length) {
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

  term = driver_alloc(sizeof(spec));
	if (NULL != term) {
		memcpy(term, &spec, sizeof(spec));
		*length = sizeof(spec) / sizeof(spec[0]);
	}
	return term;
};

static void apply_transform(void *asd) {
  async_data* data = (async_data*)asd;
  driver_data* driver = data->driver;
  xsl_engine* engine = driver->engine;
  transform_result* result = data->result;
  data->state = engine->transform(result);
};

/* DRIVER CALLBACK FUNCTIONS */

static ErlDrvData 
start_driver(ErlDrvPort port, char *buff) {
	atom_result = driver_mk_atom("result");
	atom_error  = driver_mk_atom("error");

  // avoid total madness! nice tip that one...
  if (port == NULL) {
      return ERL_DRV_ERROR_GENERAL;
  }

  driver_data *d = (driver_data*)driver_alloc(sizeof(driver_data));
	if (d == NULL) {
		return ERL_DRV_ERROR_GENERAL; // TODO: use ERL_DRV_ERROR_ERRNO and provide out-of-memory info
	}
  d->port = (void*)port;    
	return (ErlDrvData)d;
};

static void 
stop_driver(ErlDrvData drv_data) {
  // give the provider a chance to clean up
  driver_data *d = (driver_data*)drv_data;
	ErlDrvPort port = (ErlDrvPort)d->port;
	xsl_engine *engine = d->engine;
	void *state = &port;
	INFO("provider handoff: shutdown\n");			
	engine->shutdown(state);

  // driver cleanup
	driver_free((char*)engine);
  driver_free((char*)drv_data);
};

static int 
call(ErlDrvData drv_data, unsigned int command, char *buf, 
	int len, char **rbuf, int rlen, unsigned int *flags) {
	
	int i;
	int size;
  int index = 0;
	int rindex = 0;
	char *p;
	DriverInit state;    
  driver_data *d = (driver_data*)drv_data;
		
	/*if (command != INIT_COMMAND_MAGIC) {
		fprintf(stdout, "Driver received improper command code %i\n", command);
		return((int) ERL_DRV_ERROR_GENERAL);	// this will throw badarg in the emulator
	}*/
	
	ei_decode_version(buf, &index, &i);
	ei_get_type(buf, &index, &i, &size);
  p = driver_alloc(size + 1); 
  ei_decode_string(buf, &index, p);
	
	INFO("Driver received provider init command %s of %i\n", p, size);

	ei_encode_version(*rbuf, &rindex);	
	state = init_provider(d, p);
	if (state == InitOk) {
	  ei_encode_atom(*rbuf, &rindex, "configured");
	} else {
		ei_encode_tuple_header(*rbuf, &rindex, 2);
		ei_encode_atom(*rbuf, &rindex, "error");
		if (state == OutOfMemory) {
      ei_encode_string(*rbuf, &rindex, heap_space_exhausted);
		} else {
      const char *err = (d->loader)->error_message;
      ei_encode_string_len(*rbuf, &rindex, err, strlen(err));
		}
	}	
	DRV_FREE(p);
	return(rindex);
};

static void 
outputv(ErlDrvData drv_data, ErlIOVec *ev) {
	char *error_msg;
	char *xml;
  char *xsl;
  transform_job *job;
  request_context *ctx;
  transform_result *result;
  async_data *asd;
  
	driver_data *d = (driver_data*)drv_data;
	ErlDrvPort port = (ErlDrvPort)d->port;	
	ErlDrvTermData callee_pid = driver_caller(port);
	ErlDrvBinary *bin = ev->binv[1];
	// assert(ev->binv[1] != NULL)	
	
	int bin_size = bin->orig_size;
	Int32 offset = (Int32)(sizeof(input_spec_hdr) + sizeof(payload_size_hdr));
	
	if (bin_size < offset) {
		error_msg = "InconsistentInputHeaders: driver protocol not recognised.";
		driver_output2(port, error_msg, strlen(error_msg), NULL, 0);
		return;
	}
	
	const char *buffer = &(bin->orig_bytes[0]);
	const input_spec_hdr *hspec = (const input_spec_hdr* const)buffer;
	const payload_size_hdr *hsoffset = (const payload_size_hdr*)(hspec + 1);
	payload_size_hdr hsize = *hsoffset;
	
	hsoffset++;
	buffer = (const char*)hsoffset;
	
	/*
	INFO("spec xml kind = %i\n", (Int8)(hspec->input_kind));
	INFO("spec xsl kind = %i\n", (Int8)(hspec->xsl_kind));	
	INFO("spec param count = %i\n", (Int16)(hspec->param_grp_arity));
	INFO("input size = %i\n", (Int32)(hsize.input_size));
	INFO("xsl size = %i\n", (Int32)(hsize.xsl_size));
	*/
	
	if (hsize.input_size > (bin_size - offset)) {
		error_msg = "BufferSizeMismatch: input length exceeds stated buffer size.";
		driver_output2(port, error_msg, strlen(error_msg), NULL, 0);
		return;
	}
	if (hsize.xsl_size > (bin_size - (offset + hsize.input_size))) {
		error_msg = "BufferSizeMismatch: stylesheet length exceeds stated buffer size.";
		driver_output2(port, error_msg, strlen(error_msg), NULL, 0);
		return;
	}
	// TODO: add support for parameters also!
	
	xml = driver_alloc((sizeof(char) * hsize.input_size) + 1);
	xsl = driver_alloc((sizeof(char) * hsize.xsl_size) + 1);
	job = (transform_job*)driver_alloc(sizeof(transform_job));
	ctx = (request_context*)driver_alloc(sizeof(request_context));
	result = (transform_result*)driver_alloc(sizeof(transform_result));
  asd = (async_data*)driver_alloc(sizeof(async_data)); 
	
	if (xml == NULL || xsl == NULL || job == NULL || ctx == NULL || asd == NULL) { 
		DRV_FREE(xml);
		DRV_FREE(xsl);
		DRV_FREE(job);
		DRV_FREE(ctx);
    DRV_FREE(asd);
		driver_failure_atom(port, "system_limit"); 
		return;
	};
	
	// this approach only works when we're running with SMP support! Check by calling driver_system_info()
	// TODO: either mutex this for non-SMP systems or do a copy operation instead
	// driver_binary_inc_refc(bin);
	
	// we save the copying until we're sure it'll take place...
	strncpy(xml, &bin->orig_bytes[offset], hsize.input_size);
	offset += hsize.input_size;
	strncpy(xsl, &bin->orig_bytes[offset], hsize.xsl_size);
	
	fprintf(stderr, "xml = %s\n", xml);
	fprintf(stderr, "xsl = %s\n", xsl);
	
	job->input_kind = (InputType)hspec->input_kind;
	job->xsl_kind = (InputType)hspec->xsl_kind;
	job->input = xml;
	job->stylesheet = xsl;
	ctx->port = port;
	ctx->caller_pid = callee_pid;
	ctx->job = job;
	result->context = ctx;
	
  asd->driver = d;
  asd->result = result;
	
	/*
	driver_async will call engine->transform passing result, then 
	call ready_async followed by cleanup_task. The synchronous code
	works something like this:
	
	(*a->async_invoke)(a->async_data);
  if (async_ready(prt, a->async_data)) {
		if (a->async_free != NULL)
    	(*a->async_free)(a->async_data);
  }

	In SMP mode a queue is employed, but the semantics ought to remain the same 
	*/
	
	INFO("provider handoff: transform\n");			
  driver_async(port, NULL, apply_transform, asd, NULL); //cleanup_task);
};

/*
 * Processes the supplied transform_result and sends it to the appropriate
 * erlang process (e.g. the specified receiver)
 */
static void 
ready_async(ErlDrvData drv_data, ErlDrvThreadData data) {
	long 								response_len;
	ErlDrvTermData* 		term;
	ErlDrvTermData 			callee_pid, tag;
	ErlDrvPort 					port;
	driver_data* 				driverBundle;
	xsl_engine* 				provider;
	transform_result* 	result;
	request_context* 		context;
	transform_job* 			job;				// used to repeatedly refer to the completed job 
	EngineState					state;
  async_data*         async_state;
	
	// ErlDrvPort port, ErlDrvTermData receiver, ErlDrvTermData* term, int n
	
	driverBundle 	= (driver_data*)drv_data;
	provider	 		= (xsl_engine*)driverBundle->engine;
  port		 	 		= (ErlDrvPort)driverBundle->port;
  async_state   = (async_data*)data;
  result 		 		= async_state->result;
	state		 			= async_state->state;  
	context 	 		= result->context;
	job		 	 			= context->job;
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
		ERROR("Driver Out Of Memory!\n")
		state = OutOfMemoryError;
	}

	// now the engine needs the opportunity to free up any intermediate structures
	INFO("provider handoff: after_transform\n");		
  state = provider->after_transform(result);
    
	// internal cleanup time...
	cleanup_task(async_state);
	
	if (state == OutOfMemoryError) {
		ERROR("Driver Out Of Memory!\n");
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
	call,                       /* call */
	NULL                        /* event */
};

DRIVER_INIT(erlxsl_drv) {
  return &driver_entry;
}
