/*
 * erlxsl_api.h
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
 *  This header file contains internal and external api functions. 
 */

/* pervasive includes */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#ifndef _ERLXSL_H
#define  _ERLXSL_H

#ifdef  __cplusplus
extern "C" {
#endif

    /* macro definitions and other hash-defines */
#ifdef DEBUG
#define DebugOut(/* (char*) */message, args...)  \
    fprintf (stderr, message , ## args);
#else
#define DebugOut(message, args...) /* DEBUG OUTPUT DISABLED */
#endif

#define ERROR(str, ...)  \
    LOG(stderr, str, ##__VA_ARGS__);

#define INFO(str, ...)  \
    LOG(stdout, str, ##__VA_ARGS__);

#define LOG(stream, str, ...)  \
    fprintf(stream, str, ##__VA_ARGS__);

  /* Data Types & Aliasing */
  
  typedef uint8_t   UInt8;
  typedef uint16_t  UInt16;
  typedef uint32_t  UInt32;
  typedef int8_t    Int8;
  typedef int16_t   Int16;
  typedef int32_t   Int32;
  typedef int64_t   Int64;
  
  typedef enum {
    Ok,
    Error,
    XmlParseError,
    XslCompileError,
    XslTransformError,
    OutOfMemoryError
  } EngineState;

  typedef enum input_type {
    File = 1,
    Buffer = 2,
    Stream = 3
  } InputType;

  /* Indicator of the format for data stored in a DriverIOVec. */
  typedef enum { 
    /* Binary data (i.e., ErlDrvBinary). */
    Binary, 
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
  
  /* A generic command. */
  typedef struct {
    const char *command_string;
    /* Stores either an IO vector containing the command data or an XslTask. 
       When command_string == "transform" then command_data contains the XslTask. */        
    union {
      DriverIOVec* iov;
      XslTask* xsl_task; 
    } command_data;
    /* An IO vector containing the results. */        
    DriverIOVec* result;
    /* The internal (driver) call context (i.e., port and calling process id). */    
    DriverContext* context;
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
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
  } param_info;
      
  /*
   * transform_request packages the data used during
   * transformation, which is (usually) gathered from the
   * input buffer supplied by the Erlang runtime during
   * a call to one of the control/input callback functions,
   * with some useful flags.
   */
  typedef struct {
    // Denotes the kind of input (e.g. buffer, stream or file based) for the data.
    InputType input_kind;
    // Denotes the kind of xsl input (e.g. buffer, stream or file based) for the stylesheet.
    InputType xsl_kind;
    // A pointer to the head element in a linked list of param_info structures.
    param_info* parameters;
    // A pointer to the input data.
    char* input;
    // A pointer to the stylesheet data.
    char* stylesheet;
  } transform_job;
  
  /* comms related data structures (e.g. for interacting with the emulator) */    
  
  /*
   * Stores the current request context, which consists of the port,
   * target process id (note that this doesn't have to be the Pid of 
   * the *connected process* if you want to send data to another Pid) and
   * a pointer to the current request.
   */
  typedef struct {
      /* Stores the port associated with the current driver instance. */
      void* port;
      /* Stores the calling process' Pid. */
      unsigned long caller_pid;
      /* Stores a pointer to the current request data. */
      transform_job* job;
  } request_context;
    
  /*
   * Stores response data operated on when calling back into the emulator.
   */
  typedef struct {
    /* Stores the current request context. */
    request_context* context;
    /*
     * If anything went wrong with request processing, this field will
     * (hopefully) contain a helpful error message.
     */
    char* errorMessage;
    /* Stores the response size (either the buffer length or the 
     * number of terms in the supplied composite ErlDrvTermData array).
     * NB: This field is optional, as the protocol implementation performs
     * the same bounds checking regardless.
     */
    int size;
    /* The format in which the resulting data is stored */
    DataFormat format;
    /* Stores the response data itself. */
    union { 
      /* Maintaining our response data in a buffer. */
      char* buffer;
      /* Maintaining our response data in the *driver term format*; this
       is a pointer to the start of an array in the appropriate format.*/
      void* data;
    } payload;
  } transform_result;
  
/************************  THE EXTERNAL API ***********************************/

  /*
   * This function is the request handler hook used by plugins (such as 
   * implementations for sablotron and/or libxslt).
   * 
   * The response structure passed contains the context required to get to
   * underlying data, along with fields which the plugin can fill in to 
   * get data sent back to erlang.
   *
   */
  typedef EngineState TransformFunc(transform_result* result); 
  
  /*
   * This function gives the implementing plugin a chance to cleanup 
   * after handle_request has returned. 
   *
   * Returns a value from the 'DriverState' enumeration to indicate
   * current system state. A return value of DriverState::ProviderError
   * will cause the driver to exit, which the erlang port handler will
   * interpret as and error and unload the driver library.
   */
  typedef EngineState PostTransformFunc(transform_result* result);
  
  typedef EngineState CommandFunc(char* cmd, void* state);
    
  /*
   * Gives the implementation provider a change to cleanup. Because this function
   * is only called when the driver is being stopped, the provider cannot supply
   * a meaningful return value, therefore the function returns void.
   *
   * After this function returns, the memory allocated for the xsl_engine will be
   * freed, which means that the engine must fully release all references/pointers 
   * held before returning.
   */
   typedef void ShutdownFunc(void* state);
   
  /* Represents an XSLT engine. */
  typedef struct {
    /* The following function pointers will need 
       be set by the provider on startup */
    TransformFunc*      transform;
    PostTransformFunc*  after_transform;
    ShutdownFunc*       shutdown;
    /* Generic storage location, so providers can stash whatever they need. */
    void*               providerData;
  } xsl_engine;

/******************************************************************************/    
    

#ifdef  __cplusplus
}
#endif

#endif  /* _ERLXSL_H */
