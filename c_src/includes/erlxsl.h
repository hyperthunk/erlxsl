/* ``The contents of this file are copyright (C) ncorp llc,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * "License" along with this software. If not, it can be
 * retrieved via the world wide web at http://www.....
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 *
 *     $Id$
 */

/*
 *  This header file contains the external api functions erlxsl exposes
 *  to implementors who need/want to write a plug-in that supplies xslt
 *  functionality to the driver at runtime. 
 */

/* pervasive includes */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#ifndef _ERLXSL_H
#define	_ERLXSL_H

#ifdef	__cplusplus
extern "C" {
#endif

    /* macro definitions and other hash-defines */
#ifdef DEBUG
#define DebugOut(/* (char*) */message, args...)  \
      // fprintf (stderr, message , ## args);
#else
#define DebugOut(message, args...) /* DEBUG OUTPUT DISABLED */
#endif

    /* Data Types & Aliasing */
    
    typedef unsigned char           UInt8;
    typedef unsigned short  int     UInt16;
    typedef unsigned short  int     UInt32;
    typedef signed   char           Int8;
    typedef signed   short  int     Int16;
    //this (following) word size assumption is true in the erlang source, so we should be ok!
    typedef signed   long   int     Int32;
    typedef signed   long   long    Int64;

    typedef char * TextDataOffsetPtr;
    
    /*
     * Packages information about a parameter (its name and value)
     * and the next parameter in line - i.e. this is a linked list.
     */
    typedef struct param_info {
        /*
         * The name of the parameter (as passed to xslt).
         */
        TextDataOffsetPtr name;
        /*
         * The value assigned to the parameter (as passed to xslt).
         */
        TextDataOffsetPtr value;
        /*
         * Stores a pointer to the next parameter, or NULL if this
         * element is the tail.
         */
        void* next;
    } ParameterInfo;
    
    //TODO: supply explicit values here and (perhaps) use a bitmask to allow
    //      setting values such as ProviderError | FatalError
    //      [or perhaps have a separate ErrorState enum!?]
    typedef enum {
        /* Set when everything *looks* fine - this is no guarantee of
         * consistent data however!
         */
        Ok,
        /*
         * Set when the input headers (a prelude containing parameter size
         * and input type flags, set by the caller) is in an inconsistent format
         * and/or contains inconsistent data (e.g. negative flags/size).
         */
        InconsistentInputHeaders,
        /*
         * Set when the expected buffer outline size exceeds the supplied
         * buffer length. NB: Not sure if this will ever happen!
         */
        BufferSizeMismatch,
        /*
         * Set by providers, to let the driver know they've run into a problem.
         */
        ProviderError,
        /*
         * Set generally whenever a fatal runtime error occurs. 
         */
        FatalError
    } DriverState;
    
    /*
     * TransformRequest packages the data used during
     * transformation, which is (usually) gathered from the
     * input buffer supplied by the Erlang runtime during
     * a call to one of the control/input callback functions,
     * with some useful flags.
     */
    typedef struct transform_request {
        /*
         * Holds the status of the current request. This is also a hint as
         * to whether or not there is any useful information in the errorMessage field.
         */
        DriverState status;
        /*
         * If anything went wrong with request processing, this field will
         * (hopefully) contain a helpful error message.
         */
        TextDataOffsetPtr errorMessage;
        /*
         * Denotes the kind of input (e.g. buffer, stream or file based).
         *
         * 0 denotes buffer based input and is the default (if the unpacked
         * value is unrecognised).
         *
         * 1 denotes stream based input and is not implemented currently.
         *
         * 2 denotes file based input. In this mode, the inputSpecification
         * is treated as a uri for the input data itself.
         *    NB: this mode is not currently implemented either!
         */
        UInt8 inputKind;
        /*
         * Denotes the kind of xsl input (e.g. buffer, stream or file based).
         *
         * 0 denotes buffer based input and is the default (if the unpacked
         * value is unrecognised).
         *
         * 1 denotes stream based input and is not implemented currently.
         *
         * 2 denotes file based input. In this mode, the inputSpecification
         * is treated as a uri for the input data itself.
         *    NB: this mode is not currently implemented either!
         */
        UInt8 xslInputKind;
        /*
         * A pointer to the head element in a linked list of ParameterInfo structures.
         */
        ParameterInfo* parameters;
        /*
         * A pointer to the input data.
         */
        TextDataOffsetPtr inputData;
        /*
         * A pointer to the stylesheet data.
         */
        TextDataOffsetPtr stylesheetData;
    } TransformRequest;
    
    typedef TransformRequest * TransformRequestPtr;
    
    /* comms related data structures (e.g. for interacting with the emulator) */    
    
    /*
     * Describes the mode in which an xslt library adapter can 
     * pass response data to be returned to the emulator.
     */
    typedef enum {
        /* Indicates that data is stored in a buffer (i.e. char*). */
        Buffer = 1,
        /* Indicates that data is stores in the driver term format. */
        DriverTerm = 2
        //TODO: consider others, such as the external term format!?
    } ResponseFormat;
    
    /*
     * Stores the current request context, which consists of the port,
     * target process id (note that this doesn't have to be the Pid of 
     * the *connected process* if you want to send data to another Pid) and
     * a pointer to the current request.
     */
    typedef struct request_context {
        /* Stores the port associated with the current driver instance. */
        void* port;
        /* Stores the calling process' Pid. */
        unsigned long callerPid;
        /* Stores a pointer to the current request data. */
        TransformRequestPtr request;
    } RequestContext;
    
    /*
     * Stores response data operated on when calling back into the emulator.
     */
    typedef struct transform_response {
        /* Stores the current request context. */
        RequestContext* context;
        /* Specifies the response format. 
         Used to decode which field in #response we're 
         meant to be looking at.*/
        ResponseFormat responseFormat;
        /* Stores the response size (either the buffer length or the 
         * number of terms in the supplied composite ErlDrvTermData array).
         * NB: This field is optional, as the protocol implementation performs
         * the same bounds checking regardless.
         */
        int responseSize;
        /* Stores the response data itself. */
        union { 
            /* Maintaining our response data in a buffer. */
            char* buffer;
            /* Maintaining our response data in the *driver term format*; this
             is a pointer to the start of an array in the appropriate format.*/
            void* data;
        } payload;
        /* Generic storage location, so plugins can stash whatever they need. */
        void* externalData;
    } TransformResponse;

/************************  THE EXTERNAL API ***********************************/
    
    /*
     * Gives the implementation provider a chance to initialize.
     * Returns a value from the 'DriverState' enumeration to indicate
     * current system state. A return value of DriverState::ProviderError
     * will cause the driver to exit, which the erlang port handler will
     * interpret as and error and unload the driver library.
     */
    extern DriverState init_provider();

    /*
     * This function is the request handler hook used by plugins (such as 
     * implementations for sablotron and/or libxslt).
     * 
     * The response structure passed contains the context required to get to
     * underlying data, along with fields which the plugin can fill in to 
     * get data sent back to erlang.
     *
     */
    extern void handle_request(void* response); 
    
    /*
     * This function gives the implementing plugin a chance to cleanup 
     * after handle_request has returned. The callback doesn't take threads
     * into account at all, therefore any synchronization the client requires 
     * should be dealt with externally.
     *
     * Returns a value from the 'DriverState' enumeration to indicate
     * current system state. A return value of DriverState::ProviderError
     * will cause the driver to exit, which the erlang port handler will
     * interpret as and error and unload the driver library.
     */
    extern DriverState post_handle_request(void* response);
    
    /*
     * Gives the implementation provider a change to cleanup. Because this function
     * is only called when the driver is being stopped, the provider cannot supply
     * a meaningful return value, therefore the function returns void.
     */
    extern void destroy_provider();
   
/******************************************************************************/    
    

#ifdef	__cplusplus
}
#endif

#endif	/* _ERLXSL_H */
