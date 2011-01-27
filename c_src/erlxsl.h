/*
 * erlxsl.h
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
 *
 * This header file contains the data types and functions required to create an
 * XSLT engine provider shared object library.
 *
 */

#ifndef _ERLXSL_H
#define _ERLXSL_H

/* pervasive includes */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#ifndef DEBUG
#define NDEBUG    // prevent assert from happening!
#endif

// we also allow test cases to override the *normal* assert behaviour
#ifndef _TEST_ASSERT
#include <assert.h>
#define ASSERT(stmt) assert(stmt)
#endif

#ifdef  __cplusplus
extern "C" {
#endif

/* macro definitions and other hash-defines */

#ifdef LOG_DEBUG
// writes to stdout if DEBUG is defined
#define DBG(str, ...) INFO(str, ##__VA_ARGS__);
#else
#define DBG(str, ...)   /* disabled */
#endif

// writes to stderr 
#define ERROR(str, ...)  \
    LOG(stderr, str, ##__VA_ARGS__);

// writes to stdout
#define INFO(str, ...)  \
    LOG(stdout, str, ##__VA_ARGS__);

// wrapper for fprintf (uses __VA_ARGS__)
#define LOG(stream, str, ...)  \
    fprintf(stream, str, ##__VA_ARGS__);

/* If the Command is not a null pointer and its command_string is equal to
   the value "transform", evaluates to the XslTask associated with 
   its command_data, otherwise to NULL. */
#define get_task(cmd) \
    ((cmd == NULL)                                        \
      ? NULL                                              \
      : (strcmp("transform", cmd->command_string) == 0)   \
        ? cmd->command_data.xsl_task                      \
        : NULL)

/* If niether the supplied InputDocument nor its DriverIOVec is a null pointer
   and the type of the DriverIOVec is set to Text, evaluates the size of 
   the DriverIOVec, otherwise NULL. */
#define get_doc_buffer(idoc)  \
    ((idoc == NULL) ? NULL : get_buffer(idoc->iov))

/* If niether the supplied InputDocument nor its DriverIOVec is a null pointer, 
   evaluates the size of the DriverIOVec, otherwise NULL. */
#define get_doc_size(idoc)    \
    ((idoc == NULL || idoc->iov == NULL) ? -1 : idoc->iov->size)

/* If the supplied DriverIOVec is not a null pointer and it's type is set as Text, 
   evaluates to the buffer payload, otherwise to NULL. */
#define get_buffer(iov)             \
    ((iov == NULL)                  \
      ? NULL                        \
      : ((iov->type == Text)        \
        ? iov->payload.buffer       \
        : NULL))

/*#define append_data(item, cmd)  \
  ((cmd == NULL || item == NULL) \
    ? (void*)NULL \
    : (cmd->command_data.iov->size += 1, \
       cmd->command_data.iov->payload.data = item))
*/

#define get_item(item_type, item_index) \
  (((item_type*)cmd->command_data.iov->payload.data) + item_index)

#define write_cmd_data(item_T, item, cmd) \
 ((cmd == NULL || item == NULL) \
   ? (void*)NULL \
   : (cmd->command_data.iov->size > 0) \
      ? ((cmd->command_data.iov->payload.data = \
         /* resize uses realloc, which will move the original payload for us! */ \
         cmd->resize(cmd->command_data.iov->payload.data, (sizeof(item_T) * (cmd->command_data.iov->size += 1)))), \
         memcpy((((item_T*)cmd->command_data.iov->payload.data) + (cmd->command_data.iov->size - 1)), item, sizeof(item_T))) \
      : (cmd->command_data.iov->size += 1, cmd->command_data.iov->payload.data = item))

#define write_cmd_data2(item_T, item, cmd) \
  do { \
    if (cmd != NULL && item != NULL) {  \
      Int32 size = cmd->command_data.iov->size; \
      if (size > 0) { \
        Int32 idx = size -1;  \
        item_T *p = (item_T*)cmd->command_data.iov->payload.data;  \
        item_T *chunk = (item_T*)cmd->resize(cmd->command_data.iov->payload.data, \
            (sizeof(item_T) * cmd->command_data.iov->size));  \
        cmd->command_data.iov->payload.data = chunk;  \
        for (Int32 i = 0; i < idx; i++) { \
          chunk[i] = p[i];  \
        } \
        chunk[idx] = item;  \
      } else {  \
        cmd->command_data.iov->size += 1;   \
        cmd->command_data.iov->payload.data = item; \
      } \
    } \
  } while (false)

/* Resizes the result buffer of 'cmd' using its 'resize' function
 * pointer to the 'newsize'. The DriverIOVec size is updated and 
 * the expression evaluates to the (potentially re-positioned) reassigned buffer.
 */
#define resize_result_buffer(newsize, cmd) \
  (cmd->result->payload.buffer = \
    cmd->resize(cmd->result->payload.buffer, cmd->result->size = (Int32)newsize))

#define cmd_buff(cmd) cmd->result->payload.buffer

/* Ensures the result DriverIOVec for cmd is sufficient to contain buff without destroying any existing data. 
   Evaluates to the total contents of the current result buffer, or NULL if 
   cmd or cmd->result is a null pointer. */
#define ensure_buffer(buff, cmd)  \
  ((cmd == NULL || buff == NULL) \
    ? NULL /* we cannot proceed */ \
    : (cmd->result->dirty == 0) \
      ? (cmd->result->size < strlen(buff)) \
        ? resize_result_buffer(strlen(buff), cmd) \
          /* we've enough room to work here */ \
        : (cmd->result->payload.buffer == NULL) \
          ? make_result_buffer(strlen(buff), cmd) \
          : cmd->result->payload.buffer \
      : (cmd->result->size < (strlen(cmd->result->payload.buffer) + strlen(buff))) \
        ? resize_result_buffer(strlen(buff) + strlen(cmd->result->payload.buffer), cmd) \
        : cmd->result->payload.buffer)

/* Clears the results buffer of the supplied Command, unless cmd or cmd->result 
   are null pointers. Evaluates to NULL in any case. */
#define clear_result_buffer(cmd)                \
   do {                                         \
     if (cmd != NULL && cmd->result != NULL) {  \
       cmd->result->dirty = 0;                  \
       DRV_FREE(cmd->result->payload.buffer);   \
       cmd->result->payload.buffer = NULL;      \
     }                                          \
   } while (false)

/* allocates a result buffer of the requested size, setting the 
 * type and size fields of the DriverIOVec structure accordingly */
#define make_result_buffer(buffersize, cmd) \
  ((cmd == NULL || buffersize <= 0) ? NULL \
    : (cmd->result == NULL) ? NULL \
      : (cmd->result->type = Text, \
         cmd->result->size = buffersize, \
         cmd->result->payload.buffer = cmd->alloc(sizeof(char) * buffersize)))

/*  Writes the given buffer to the results buffer of the supplied Command
   
   - Ensures that the buffer is of sufficient size
   - If the buffer has already been written to, then buff is concatenated onto the end
   - If the buffer has not been written to, buff is copied into place
   
   Evaluates to the total contents of the current result buffer, or NULL if 
   cmd or cmd->result is a null pointer. */
#define write_result_buffer(buff, cmd) \
  ((ensure_buffer(buff, cmd)) \
    ? (cmd->result->dirty == 0) \
      ? (cmd->result->dirty=1, strcpy(cmd->result->payload.buffer, buff))  \
      : strcat(cmd->result->payload.buffer, buff)  \
    : NULL)

/* Data Types & Aliasing */

typedef uint8_t   UInt8;
typedef uint16_t  UInt16;
typedef uint32_t  UInt32;
typedef int8_t    Int8;
typedef int16_t   Int16;
typedef int32_t   Int32;
typedef int64_t   Int64;

/* Indicates the transient state of a driver. */
typedef enum {
  Success,
  InitOk,
  LibraryNotFound,
  EntryPointNotFound,
  InitFailed,
  OutOfMemory,
  UnknownCommand,
  UnsupportedOperationError,
  BadArgumentError,
  EmptyBufferError,
  DecodeError
} DriverState;

/* Indicates the transient state of an XslEngine - used primarily for error reporting. */
typedef enum {
  Ok,
  Error,
  XmlParseError,
  XslCompileError,
  XslTransformError,
  OutOfMemoryError
} EngineState;

/* Used to identify the semantic meaning of the data in an InputDocument,
   as being a file uri (File), char buffer (Buffer) or input stream (Stream). */
typedef enum {
  File = 1,
  Buffer = 2,
  Stream = 3
} InputType;

/* Indicator of the format for data stored in a DriverIOVec. */
typedef enum { 
  /* Binary data (i.e., ErlDrvBinary). */
  Binary,
  /* ErlDrvTermData */
  Term, 
  /* An ErlXSL API Object. */
  Object, 
  /* A char buffer of raw data. */
  Text,
  /* An opaque handle - set by an XslEngine when caching XML documents. */
  Opaque
} DataFormat;

/* 
 * Used to pass data between the driver and an xsl_engine.
 * Stores data in either a character buffer or some other
 * format undefined at compile time - must be used with a value
 * from the DataFormat enum to indicate which 'payload' member is in use. 
 */  
typedef struct { 
  /* FOR INTERNAL USE ONLY */
  unsigned int dirty:1;
  /* Indicates the type of data in the 'payload' field. */
  DataFormat type;
  /* Indicates the size of the buffer (or data) where this is necessary. */
  Int32 size; // FIXME: this is a big assumption about the MAX response size!
  union {
    /* Maintaining our response data in a buffer. */
    char* buffer;
    /* Maintaining our response data in some other format. */
    void* data;
  } payload;
} DriverIOVec;

/* Stores the data an input type specifier for an input document. */
typedef struct {
  /* 
   * Indicates whether the buffer contains the document
   * content, a file uri, a stream (IO vector) or a binary object 
   */
  InputType type;
  /* Stores the actual payload (i.e., the document content, file uri, stream or binary object). */
  DriverIOVec* iov;
} InputDocument;

/* Stores the call context for a command, containing the port and caller pid. 
   This is probably only of interest to XslEngine providers 
   that wish to use ei/erlang APIs directly. */
typedef struct {
  /* Stores the port associated with the current driver instance. */
  void* port;
  /* Stores the calling process' Pid. */
  unsigned long caller_pid;  
} DriverContext;

/*
 * Packages information about a parameter (its name and value)
 * and the next parameter in line - i.e. this is a linked list.
 */
typedef struct {
  /* The name of the parameter (as passed to xslt). */
  char* key;
  /* The value assigned to the parameter (as passed to xslt). */
  char* value;
  /* Stores a pointer to the next parameter, or NULL if this element is the tail. */
  void* next;
} ParameterListNode;  

/* A specialised command pertaining to an XSLT transformation that has been tasked. */
typedef struct {
  /* The input (XML) document. */
  InputDocument* input_doc;
  /* The xslt (XML) document. */
  InputDocument* xslt_doc;
  /* The head of a linked list of parameters, or NULL if none are passed. */    
  ParameterListNode* parameters;
} XslTask;  

/* Allocation function type. */  
typedef void* alloc_f(size_t size);

/* Reallocation function type */
typedef void * realloc_f(void *ptr, size_t size);

/* Release/Free function type. */
typedef void release_f(void* p);

/* A generic command. */
typedef struct {
  const char *command_string;
  /* Stores either an IO vector containing the command data or an XslTask. 
     When command_string == "transform" then command_data contains the XslTask. */        
  union {
    // Data is held in a DriverIOVec
    DriverIOVec* iov;
    // Data is held as an XslTask
    XslTask* xsl_task; 
  } command_data;
  /* An IO vector containing the results. */        
  DriverIOVec* result;
  /* The internal (driver) call context (i.e., port and calling process id). */    
  DriverContext* context;
  /* Custom allocator (wraps drivers allocation strategy) */
  alloc_f* alloc;
  /* Custom realloc (wraps drivers allocation strategy) */
  realloc_f* resize;
  /* Custom 'free' (wraps drivers allocation strategy) */
  release_f* release;
} Command;

/*
 * This function is the request handler hook used by plugins (such as 
 * implementations for sablotron and/or libxslt).
 * 
 * The response structure passed contains the context required to get to
 * underlying data, along with fields which the plugin can fill in to 
 * get data sent back to erlang.
 *
 */
typedef EngineState transform_function(Command* cmd); 

/*
 * This function gives the implementing plugin a chance to cleanup 
 * after handle_request has returned. 
 *
 * Returns a value from the 'DriverState' enumeration to indicate
 * current system state. A return value of DriverState::ProviderError
 * will cause the driver to exit, which the erlang port handler will
 * interpret as and error and unload the driver library.
 */
typedef EngineState after_transform_function(Command* cmd);

/* Generic command processing function. */
typedef EngineState command_function(Command* cmd);

/*
 * Gives the implementation provider a change to cleanup. Because this function
 * is only called when the driver is being stopped, the provider cannot supply
 * a meaningful return value, therefore the function returns void.
 *
 * After this function returns, the memory allocated for the xsl_engine will be
 * freed, which means that the engine must fully release all references/pointers 
 * held before returning.
 */
typedef void shutdown_function(void* state);
 
/* Represents an XSLT engine. */
typedef struct {
  /* The following function pointers will need be set by the provider on startup */
  command_function*         command;
  transform_function*       transform;
  after_transform_function* after_transform;
  shutdown_function*        shutdown;
  /* Generic storage location, so providers can stash whatever they need. */
  void*                     providerData;
} XslEngine;

#ifdef  __cplusplus
}
#endif

#endif  /* _ERLXSL_H */
