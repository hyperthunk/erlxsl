/*
 * erlxsl_internal.h
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
 * This header contains internal utility functions used by the port program and
 * linked in driver executables. This header *must* be included *after* the 
 * main erlxsl header, and *after* the erlxsl_driver header when the linked-in 
 * driver is being compiled. 
 * 
 */

#ifndef _ERLXSL_INT_H
#define _ERLXSL_INT_H

#include <stdarg.h>

#ifdef WIN32
  #include <Windows.h>
#else
  #include <dlfcn.h>
#endif

/* INTERNAL DATA & DATA STRUCTURES */

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
} ParameterSpecHeaders;

/* Used as a handle during async processing */ 
typedef struct {
  /* Holds the state of the XslEngine post processing. */
  EngineState state;
  /* Holds the DriverHandle. */
  DriverHandle* driver;
  /* Holds the command being processed. */
  Command* command; 
} AsyncState;

// entry point in the provider engine shared object library
static const char const *init_entry_point = "init_engine";

/* FORWARD DEFS */

#ifdef WIN32
static void* dlsym(void*, const char*);
static int dlclose(void*);
#endif

static void set_dlerror(LoaderSpec*, char*);

#ifdef WIN32
static void *_dlopen(const char*);
#else
static void *_dlopen(const char*);
#endif

/* Attempt to load the shared object library specified by the supplied LoaderSpec */
static void load_library(LoaderSpec*);
/* Attempts to initialize the XSLT engine provider */
static DriverState init_provider(DriverHandle*, char*);
/* Async callback wrapper that takes an AsyncState struct, applies the engine function and stores the result */
static void apply_transform(void*);
/* Free all memory associated with the supplied DriverIOVec (including all referenced data). */
static void free_iov(DriverIOVec*);
/* Free all memory associated with the supplied ParameterListNode (including all referenced data). */
static void free_parameters(ParameterListNode*);
/* Free all memory associated with the supplied InputDocument (including all referenced data). */
static void free_document(InputDocument*);
/* Free all memory associated with the supplied XslTask (including all referenced data). */
static void free_task(XslTask*);
/* Free all memory associated with the supplied Command (including all referenced data). */
static void free_command(Command *cmd);
/* Free all memory associated with the supplied AsyncState (including all referenced data). */
static void free_async_state(AsyncState*);
/* Allocate and initialize a DriverIOVec structure with the supplied 
   arguments (presets all fields appropriately). Returns NULL on failure. */
static DriverIOVec* init_iov(DataFormat, Int32, void*);
/* Allocate and initialize an InputDocument structure with the supplied 
   arguments (presets all fields appropriately). Returns NULL on failure. */
static InputDocument* init_doc(InputType, Int32, char*);
/* Initialize the supplied XslTask structure with the supplied arguments 
   (presets all fields appropriately). Returns the appropriate DriverState to indicate the result. */
static DriverState 
init_task(XslTask*, 
          const PayloadSizeHeaders* const, 
          const InputSpecHeaders* const, 
          char*, char*);
/* Allocate and initialize a Command structure with the supplied arguments 
   (presets all fields appropriately). Returns NULL on failure. */
static Command* init_command(const char*, DriverContext*, XslTask*, DriverIOVec*);
static char *libload_failure = "Unable to load xslt provider library.";
char *entrypoint_failure = "Unable to locate entry point 'init_engine'.";

/* MACROS */

#ifndef FAIL
#define FAIL(p, msg) abort(msg)
#endif

/* INTERNAL DRIVER FUNCTIONS */

/* Tries to allocate heap space of 'size'. If this fails, the pointers 
   in the varags array are freed one by one and the program fails. */ 
static void* try_driver_alloc(void *port, size_t size, void* pfree, ...) {
  void* val = ALLOC(size);
  if (val == NULL) {
    va_list ap;
    void* p;

    va_start(ap, pfree); 
    while ((p = va_arg(ap, void*)) != NULL) {
		  DRV_FREE(p);
    }
    va_end(ap);

    FAIL(port, "system_limit"); 
  }
  return val;
};

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
  strcat(message, " ErrorCode: %i");
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
  if (dest == NULL) return;
  
  if ((dest->library = _dlopen((const char*)dest->name)) == NULL) {
    set_dlerror(dest, libload_failure);
    DBG("dlopen failed with %s\n", dest->error_message);
    return;
  }
  
  DBG("library %s = %p\n", dest->name, dest->library);

  if ((dest->init_f = dlsym(dest->library, init_entry_point)) == NULL) {
    set_dlerror(dest, entrypoint_failure);
  }
};

static DriverState 
init_provider(DriverHandle *drv, char *buff) {
  XslEngine *engine = ALLOC(sizeof(XslEngine));
  LoaderSpec* lib = ALLOC(sizeof(LoaderSpec));
    
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

static void apply_transform(void *asd) {
  AsyncState* data = (AsyncState*)asd;
  DriverHandle* driver = data->driver;
  XslEngine* engine = driver->engine;
  Command* command = data->command;
  data->state = engine->transform(command);
  DBG("output buffer: %s\n", command->result->payload.buffer);
};

static void
free_iov(DriverIOVec *iov) {
  if (iov != NULL) {
    if (iov->dirty == 1) {
      if (iov->type == Text) {
        DBG("Freeing iov buffer %s\n", iov->payload.buffer);
        DRV_FREE(iov->payload.buffer);
      } else {
        DBG("Freeing iov data %p\n", iov->payload.data);
        DRV_FREE(iov->payload.data);
      }
    }
    DRV_FREE(iov);
  }
};

static void
free_parameters(ParameterListNode *current) {
  ParameterListNode *next;
  DBG("cleanup parameters\n");
  while (current != NULL) {
    DBG("current wasn't null!? - %p\n", current);
    next = (ParameterListNode*)current->next;
    DBG("next - %p\n", next);
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
    DRV_FREE(doc);
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
    if (strcmp("transform", cmd->command_string) == 0) {
      free_task(cmd->command_data.xsl_task);
    } else {    
      free_iov(cmd->command_data.iov);
    }
    free_iov(cmd->result);
    DRV_FREE(cmd);
  }
};

static void 
free_async_state(AsyncState *state) {
  ASSERT(state != NULL);
  if (state != NULL) {
    free_command(state->command);
    DRV_FREE(state);
  }
};

static DriverIOVec*
init_iov(DataFormat type, 
         Int32 size, 
         void *payload) {
  
  DriverIOVec *iov = ALLOC(sizeof(DriverIOVec));
  if (iov == NULL) return NULL;
  
  iov->dirty = (payload == NULL) ? 0 : 1;
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
  
  InputDocument *doc = ALLOC(sizeof(InputDocument));
  if (doc == NULL) return NULL;
  
  doc->type = type;
  if ((doc->iov = init_iov(Text, size, (void*)data)) == NULL) {
    DRV_FREE(doc);
    return NULL;
  }
  return doc;
};

static void clear_task_fields(XslTask* t) {
  t->input_doc = t->xslt_doc = NULL;
  t->parameters = NULL;
};

static DriverState 
init_task(XslTask *task, 
          const PayloadSizeHeaders* const hsize, 
          const InputSpecHeaders* const hspec, 
          char* xml, 
          char* xsl) {
  
  if (task == NULL) {
    return BadArgumentError;
  }
  clear_task_fields(task);
  
  if (hsize == NULL || hspec == NULL || xml == NULL || xsl == NULL) {
    return BadArgumentError;
  }
  
  if (hsize->input_size == 0 || hsize->xsl_size == 0) {
    return EmptyBufferError;
  }
  
  ASSERT(hsize->input_size == strlen(xml));
  ASSERT(hsize->xsl_size == strlen(xsl));
  
  InputDocument *xmldoc;
  InputDocument *xsldoc;
  
  if ((xmldoc = init_doc((InputType)hspec->input_kind, 
      hsize->input_size, xml)) == NULL) {
    DRV_FREE(xml);
    DRV_FREE(xsl);  
    return OutOfMemoryError;
  }
      
  if ((xsldoc = init_doc((InputType)hspec->xsl_kind, 
      hsize->xsl_size, xsl)) == NULL) {  
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

static inline void* 
internal_alloc(size_t size) {
  return ALLOC(size);
};

static inline 
void internal_free(void *p) {
  DRV_FREE(p);
};

static Command*
init_command(const char *command, DriverContext *context, XslTask *xsl_task, DriverIOVec *iov) {
  Command *cmd; 
  if ((cmd = ALLOC(sizeof(Command))) == NULL) return NULL;  
  if ((cmd->result = init_iov(Text, -1, NULL)) == NULL) {
    return NULL;
  }
  if (xsl_task != NULL) {
    cmd->command_data.xsl_task = xsl_task;
  } else {
    cmd->command_data.iov = iov;
  }
  cmd->command_string = command;
  cmd->context = context;
  cmd->alloc = internal_alloc;
  cmd->release = internal_free;
  return cmd;
};

#endif /* _ERLXSL_INT_H */
