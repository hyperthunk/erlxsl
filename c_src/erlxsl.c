/*
 * erlxsl.c
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

// The erlxsl_driver header brings in the main erlxsl header and major pervasive includes
#include "erlxsl_driver.h"

/* INTERNAL DATA & DATA STRUCTURES */

// often used message constants
static const char* const unknown_command = "Unknown Command!";
static const char* const heap_space_exhausted = "Out of Memory!";
static const char* const transform_command = "transform";
static const char* const unsupported_response_type = "Unsupported Response Type.";

/* DRIVER CALLBACK FUNCTIONS */

static ErlDrvData 
start_driver(ErlDrvPort port, char *buff) {
  atom_result = driver_mk_atom("result");
  atom_error  = driver_mk_atom("error");

  // avoid total madness! nice tip that one...
  if (port == NULL) {
      return ERL_DRV_ERROR_GENERAL;
  }

  DriverHandle *d = (DriverHandle*)ALLOC(sizeof(DriverHandle));
  if (d == NULL) {
    return ERL_DRV_ERROR_GENERAL; // TODO: use ERL_DRV_ERROR_ERRNO and provide out-of-memory info
  }
  d->port = (void*)port;    
  return (ErlDrvData)d;
};

static void 
stop_driver(ErlDrvData drv_data) {
  // give the provider a chance to clean up
  DriverHandle *d = (DriverHandle*)drv_data;
  ErlDrvPort port = (ErlDrvPort)d->port;
  XslEngine *engine = d->engine;
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
  int type;
  int size;
  int index = 0;
  int rindex = 0;
  /*int arity;  
  char cmd[MAXATOMLEN];*/
  char *data;
  DriverState state;    
  DriverHandle *d = (DriverHandle*)drv_data;
  
  ei_decode_version(buf, &index, &i);  
  if (command == INIT_COMMAND) {
    ei_get_type(buf, &index, &type, &size);
    INFO("ei_get_type %s of size = %i\n", ((char*)&type), size);
    data = ALLOC(size + 1); 
    ei_decode_string(buf, &index, data);
    INFO("Driver received data %s\n", data);
    state = init_provider(d, data);
  } else {
    state = UnknownCommand;
  }
  
  ei_encode_version(*rbuf, &rindex);
  if (state == InitOk) {
    ei_encode_atom(*rbuf, &rindex, "configured");
  } else {
    ei_encode_tuple_header(*rbuf, &rindex, 2);
    ei_encode_atom(*rbuf, &rindex, "error");
    if (state == OutOfMemory) {
      ei_encode_string(*rbuf, &rindex, heap_space_exhausted);
    } else if (state == UnknownCommand) {
      ei_encode_string(*rbuf, &rindex, unknown_command);
    } else {
      const char *err = (d->loader)->error_message;
      ei_encode_string_len(*rbuf, &rindex, err, strlen(err));
    }
  }  
  DRV_FREE(data);
  return(rindex);
};

static void 
outputv(ErlDrvData drv_data, ErlIOVec *ev) {
  char *error_msg;
  char *xml;
  char *xsl;
    
  DriverHandle *d = (DriverHandle*)drv_data;
  ErlDrvPort port = (ErlDrvPort)d->port;
  ErlDrvBinary *bin = ev->binv[1];
  int bin_size = bin->orig_size;
  Int32 offset = (Int32)(sizeof(InputSpecHeaders) + sizeof(PayloadSizeHeaders));
  
  if (bin_size < offset) {
    error_msg = "InconsistentInputHeaders: driver protocol not recognised.";
    driver_output2(port, error_msg, strlen(error_msg), NULL, 0);
    return;
  }
  
  const char *buffer = &(bin->orig_bytes[0]);
  const InputSpecHeaders *hspec = (const InputSpecHeaders* const)buffer;
  const PayloadSizeHeaders *hsoffset = (const PayloadSizeHeaders*)(hspec + 1);
  PayloadSizeHeaders hsize = *hsoffset;
  
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
  
  XslTask *job;
  DriverContext *ctx;
  AsyncState *asd;
  DriverState state;
  ErlDrvTermData callee_pid = driver_caller(port);
  // assert(ev->binv[1] != NULL)    
  
  if ((xml = (char*)ALLOC(sizeof(char) * (hsize.input_size + 1))) == NULL) {
    FAIL(port, "system_limit");
    return;
  }
  if ((xsl = (char*)try_driver_alloc(port, 
    sizeof(char) * (hsize.xsl_size + 1), xml)) == NULL) return;
  if ((job = (XslTask*)try_driver_alloc(port, 
    sizeof(XslTask), xml, xsl)) == NULL) return;
  if ((ctx = (DriverContext*)try_driver_alloc(port, 
    sizeof(DriverContext), xml, xsl, job)) == NULL) return;
  if ((asd = (AsyncState*)try_driver_alloc(port, 
    sizeof(AsyncState), xml, xsl, job, ctx)) == NULL) return; 
  
  /*if ((asd = try_driver_alloc(sizeof(AsyncState), 
    xml, xsl, xmldoc, xsldoc, job, ctx, command) == NULL)) return;*/
    
  ctx->port = port;
  ctx->caller_pid = callee_pid;
  asd->driver = d;
  if ((asd->command = init_command(transform_command, ctx, job, NULL)) == NULL) {
    free_async_state(asd);
    FAIL(port, "system_limit");
    return;
  }
  
  // this approach only works when we're running with SMP support! Check by calling driver_system_info()
  // TODO: either mutex this for non-SMP systems or do a copy operation instead
  // driver_binary_inc_refc(bin);
  
  // we save the copying until we're sure it'll take place...
  strncpy(xml, &bin->orig_bytes[offset], hsize.input_size);
  offset += hsize.input_size;
  strncpy(xsl, &bin->orig_bytes[offset], hsize.xsl_size);
  
  fprintf(stderr, "xml = %s\n", xml);
  fprintf(stderr, "xsl = %s\n", xsl);

  state = init_task(job, &hsize, hspec, xml, xsl);
  switch (state) {
  case OutOfMemoryError:
    free_async_state(asd);
    FAIL(port, "system_limit"); 
    return;
  case Success:
    /*
    driver_async will call engine->transform passing command, then 
    call ready_async followed by cleanup_task. The synchronous code
    works something like this:
  
    (*a->async_invoke)(a->async_data);
    if (async_ready(prt, a->async_data)) {
      if (a->async_free != NULL)
        (*a->async_free)(a->async_data);
    }

    In SMP mode a queue is employed, but the semantics ought to remain the same.
    */  
    INFO("provider handoff: transform\n");      
    driver_async(port, NULL, apply_transform, asd, NULL); //cleanup_task);
    break;
  default:  // TODO: it would be better if we didn't do "everthing else is an error" here
    // TODO: error!?
    break;
  }  
};

/*
 * Processes the supplied transform_result and sends it to the appropriate
 * erlang process (e.g. the specified receiver)
 */
static void 
ready_async(ErlDrvData drv_data, ErlDrvThreadData data) {
  long response_len;
  ErlDrvTermData *term;
  
  DriverHandle *driver_handle = (DriverHandle*)drv_data;
  ErlDrvPort port = (ErlDrvPort)driver_handle->port;
  XslEngine *provider = (XslEngine*)driver_handle->engine;
  AsyncState *async_state = (AsyncState*)data;
  EngineState state = async_state->state;
  Command *command = async_state->command;
  ErlDrvTermData callee_pid = (ErlDrvTermData)(command->context)->caller_pid;  
  ErlDrvTermData tag = (state == Ok) ? atom_result : atom_error;  
  DriverIOVec* outv = command->result;
  
  if (state == OutOfMemoryError) {
    ERROR("Driver Out Of Memory!\n");
    free_async_state(async_state);
    FAIL(port, "system_limit");
    // this statement [above] will cause the driver to unload, so we may as well fail fast....
    return;
  }
  
  switch (outv->type) {
  case Text:
    term = make_driver_term(&port, outv->payload.buffer, &tag, &response_len);
    break;
  case Binary:
    term = make_driver_term_bin(&port, ((ErlDrvBinary*)outv->payload.data), &tag, &response_len);
    break;
  default:
    term = make_driver_term(&port, (char*)unsupported_response_type, &tag, &response_len);
    break;
  }
  
  if (term == NULL) {
    ERROR("Driver Out Of Memory!\n");    
    // do a quick internal cleanup ?
    // free_async_state(asd);    
    // give the XslEngine a chance to try and clean up!?
    // state = provider->after_transform(command);

    FAIL(port, "system_limit");
    // this statement [above] will cause the driver to unload, so we may as well fail fast....
    return;
  }
  
  driver_send_term(port, callee_pid, term, response_len);

  // now the engine needs the opportunity to free up any intermediate structures
  INFO("provider handoff: after_transform\n");    
  state = provider->after_transform(command);
    
  // internal cleanup time...
  free_async_state(async_state);
};

/* DRIVER API EXPORTS */

static ErlDrvEntry driver_entry = {
  NULL,                       /* init */
  start_driver,               /* start, called when port is opened */
  stop_driver,                /* stop, called when port is closed */
  NULL,                       /* output, called when port receives messages */
  NULL,                       /* ready_input, called when input descriptor ready to read*/
  NULL,                       /* ready_output, called when output descriptor ready to write */
  "erlxsl",                   /* the name of the driver */
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
