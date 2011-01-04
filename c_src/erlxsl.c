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

#ifdef WIN32
  #include <Windows.h>
#else
  #include <dlfcn.h>
#endif
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
typedef void (*init_func)(XslEngine*);

typedef struct {
  char* name;
  char* error_message;
  void* library;
  init_func init_f;
} LoaderSpec;

typedef struct {
  void* port;
  XslEngine* engine;
  LoaderSpec* loader;
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
} InputSpecHeaders;

/*
 * Headers that specify the data size for the payload (i.e. the size
 * of the input and stylesheet buffers).
 */
typedef struct  {
  /* The position in which we're expecting the input size marker value */
  Int32 input_size;
  /* The position in which we're expecting the xsl size marker value */
  Int32 xsl_size;
} PayloadSizeHeaders;

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

/* Used as a handle during async processing */ 
typedef struct {
  /* Holds the state of the XslEngine post processing. */
  EngineState state;
  /* Holds the DriverHandle. */
  DriverHandle* driver;
  /* Holds the command being processed. */
  Command* command; 
} AsyncState;

static ErlDrvTermData atom_result; 
static ErlDrvTermData atom_error;
static const char *init_entry_point = "init_engine";
static const char *unknown_command = "Unknown Command!";
static const char *heap_space_exhausted = "Out of Memory!";
static const char *transform_command = "transform";
static const char *unsupported_response_type = "Unsupported Response Type.";

/* FORWARD DEFS */

static DriverState init_provider(DriverHandle*, char*);
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

/* INTERNAL DRIVER FUNCTIONS */

static void* try_driver_alloc(ErlDrvPort port, size_t size, void* pfree, ...) {
  void* val = driver_alloc(size);
  if (val == NULL) {
    va_list ap;
    void* p;
    
    va_start(ap, pfree); 
    while ((p = va_arg(ap, void*)) != NULL) {
		  DRV_FREE(p);
    }
    va_end(ap);
    
    driver_failure_atom(port, "system_limit"); 
  }
  return val;
}

#ifdef WIN32
static void* 
dlsym(void *mod, const char *func) {
  HMODULE lib = (HMODULE)mod;
  void *funcp = (void*)GetProcAddress(lib, func);
  return funcp;
};

static int
dlclose(void *handle) {
  FreeLibrary((HMODULE)handle);
};
#endif

static void
set_dlerror(LoaderSpec *dest, char *message) {
#ifdef WIN32
  DWORD error_code = GetLastError();
  strcat(message, " ErrorCode: ~i");
  sprintf(dest->error_message, message, (UInt32)error_code);
#else
  dest->error_message = dlerror();
  if (dest->error_message == NULL) {
    sprintf(dest->error_message, message, dest->name);
  } 
#endif
};

#ifdef WIN32
static void *_dlopen(const char *name) {
  return LoadLibrary(name);
};
#else
static void *_dlopen(const char *file) {
  return dlopen(file, /*RTLD_LAZY*/ RTLD_NOW);
};
#endif

static void
load_library(LoaderSpec *dest) {
  char *libload_failure = "Unable to load xslt provider library.";
  char *entrypoint_failure = "Unable to locate entry point 'init_engine'.";
  
  if ((dest->library = _dlopen((const char*)dest->name)) == NULL) {
    INFO("dlopen failed with %s'\n", dlerror());
    set_dlerror(dest, libload_failure);
  }
  
  printf("library %s = %p\n", dest->name, dest->library);

  if ((dest->init_f = dlsym(dest->library, init_entry_point)) == NULL) {
    set_dlerror(dest, entrypoint_failure);
  }
};

static DriverState 
init_provider(DriverHandle *drv, char *buff) {
  XslEngine *engine = (XslEngine*)driver_alloc(sizeof(XslEngine));
  LoaderSpec* lib = (LoaderSpec*)driver_alloc(sizeof(LoaderSpec));
    
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

/* makes a tagged tuple (using the driver term format) for the supplied binary payload. */
static ErlDrvTermData* 
make_driver_term_bin(ErlDrvPort *port, ErlDrvBinary *payload, ErlDrvTermData *tag, long *length) {
  ErlDrvTermData *term;
  ErlDrvTermData  spec[10];
  term = driver_alloc(sizeof(spec));
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
  term = driver_alloc(sizeof(spec));
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

static void apply_transform(void *asd) {
  AsyncState* data = (AsyncState*)asd;
  DriverHandle* driver = data->driver;
  XslEngine* engine = driver->engine;
  Command* command = data->command;
  data->state = engine->transform(command);
};

static void
free_iov(DriverIOVec *iov) {
  if (iov != NULL) {
    if (iov->type == Text) {
      char *buffer = iov->payload.buffer;
      DRV_FREE(buffer);
    } else {
      void *data = iov->payload.data;
      DRV_FREE(data);
    }
    driver_free(iov);
  }
};

static void
free_parameters(ParameterListNode *current) {
  ParameterListNode *next;
  INFO("cleanup parameters\n");
  while (current != NULL) {
    INFO("current wasn't null!? - %p\n", current);
    next = (ParameterListNode*)current->next;
    INFO("next - %p\n", next);
    DRV_FREE(current->key);
    DRV_FREE(current->value);
    DRV_FREE(current);
    current = next;
  }
};

static void
free_document(InputDocument *doc) {
  if (doc != NULL) {
    free_iov(doc->iov);
    driver_free(doc);
  }
};

static void 
free_task(XslTask *task) {
  if (task != NULL) {
    free_parameters(task->parameters);
    free_document(task->input_doc);
    free_document(task->xslt_doc);
  }
};

static void
free_command(Command *cmd) {
  if (cmd != NULL) {
    if (strcmp("transform", cmd->command_string)) {
      free_task(cmd->command_data.xsl_task);
    } else {
      free_iov(cmd->command_data.iov);
    }
    DRV_FREE(cmd->context);
    free_iov(cmd->result);  
    DRV_FREE((char*)cmd->command_string);
    driver_free(cmd);
  }
};

static void 
free_async_state(AsyncState *state) {
  if (state != NULL) {
    free_command(state->command);
    DRV_FREE(state);
  }
};

static DriverIOVec*
init_iov(DataFormat type, 
         Int32 size, 
         void *payload) {
  
  DriverIOVec *iov = (DriverIOVec*)driver_alloc(sizeof(DriverIOVec));
  if (iov == NULL) return NULL;
  
  iov->type = type;
  iov->size = size;
  if (type == Text) {
    iov->payload.buffer = (char*)payload;
  } else {
    iov->payload.data = payload;
  }
  return iov;
};

static InputDocument*
init_doc(InputType type, 
         Int32 size, 
         char *data) {
  
  InputDocument *doc = (InputDocument*)driver_alloc(sizeof(InputDocument));
  if (doc == NULL) return NULL;
  
  doc->type = type;
  if ((doc->iov = init_iov(Text, size, (void*)data)) == NULL) {
    DRV_FREE(doc);
    return NULL;
  }
  return doc;
};

static DriverState 
init_task(XslTask *task, 
          const PayloadSizeHeaders* const hsize, 
          const InputSpecHeaders* const hspec, 
          char* xml, 
          char* xsl) {
  
  InputDocument *xmldoc;
  InputDocument *xsldoc;
  
  if ((xmldoc = init_doc((InputType)hspec->input_kind, 
      hsize->input_size, xml)) == NULL) {
    DRV_FREE(xml);
    DRV_FREE(xsl);  
    return OutOfMemoryError;
  }
      
  if ((xsldoc = init_doc((InputType)hspec->xsl_kind, 
      hsize->xsl_size, xml)) == NULL) {  
    free_document(xmldoc);
    DRV_FREE(xml);
    DRV_FREE(xsl);
    return OutOfMemoryError;  
  }
  
  task->input_doc = xmldoc;
  task->xslt_doc = xsldoc;
  task->parameters = NULL;
  return Success;
};

static Command*
init_command(const char *command, DriverContext *context, XslTask* xsl_task, DriverIOVec* iov) {
  Command *cmd; 
  if ((cmd = (Command*)driver_alloc(sizeof(Command))) == NULL) return NULL;
  if (xsl_task != NULL) {
    cmd->command_data.xsl_task = xsl_task;
  } else {
    cmd->command_data.iov = iov;
  }
  cmd->command_string = command;
  cmd->context = context;
  return cmd;
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

  DriverHandle *d = (DriverHandle*)driver_alloc(sizeof(DriverHandle));
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
    data = driver_alloc(size + 1); 
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
  
  if ((xml = (char*)driver_alloc(sizeof(char) * (hsize.input_size + 1))) == NULL) {
    driver_failure_atom(port, "system_limit");
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
    driver_failure_atom(port, "system_limit");
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
    driver_failure_atom(port, "system_limit"); 
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
    driver_failure_atom(port, "system_limit");
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

    driver_failure_atom(port, "system_limit");
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
