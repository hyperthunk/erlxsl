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
#include <stdbool.h>

#ifndef _ERLXSL_H
#define	_ERLXSL_H

#ifdef	__cplusplus
extern "C" {
#endif

    /* macro definitions and other hash-defines */
#ifdef DEBUG
#define DebugOut(/* (char*) */message, args...)  \
    fprintf (stderr, message , ## args);
#else
#define DebugOut(message, args...) /* DEBUG OUTPUT DISABLED */
#endif

#define SET_STATUS(res, stat) \
	((transform_job*)((request_context*)(res->context))->job)->status = stat;

	// TODO: investigate using the Apache Portable Runtime
	
    /* Data Types & Aliasing */
    
    typedef unsigned char           UInt8;
    typedef unsigned short  int     UInt16;
    typedef unsigned short  int     UInt32;
    typedef signed   char           Int8;
    typedef signed   short  int     Int16;
    //this (following) word size assumption is true in the erlang source, so we should be ok!
    typedef signed   long   int     Int32;
    typedef signed   long   long    Int64;
    //this is platform dependant but consistent with erts
    typedef unsigned long           ULong;
    
    /*
     * Packages information about a parameter (its name and value)
     * and the next parameter in line - i.e. this is a linked list.
     */
    typedef struct {
        /*
         * The name of the parameter (as passed to xslt).
         */
        char * name;
        /*
         * The value assigned to the parameter (as passed to xslt).
         */
        char * value;
        /*
         * Stores a pointer to the next parameter, or NULL if this
         * element is the tail.
         */
        void* next;
    } param_info;
    
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

	typedef enum {
		File = 1,
		Buffer = 2,
		Stream = 3
	} InputType;
    
    /*
     * transform_request packages the data used during
     * transformation, which is (usually) gathered from the
     * input buffer supplied by the Erlang runtime during
     * a call to one of the control/input callback functions,
     * with some useful flags.
     */
    typedef struct {
        /*
         * Holds the status of the current request. This is also a hint as
         * to whether or not there is any useful information in the errorMessage field.
         */
        DriverState status;
        /*
         * If anything went wrong with request processing, this field will
         * (hopefully) contain a helpful error message.
         */
        char * errorMessage;
        /*
         * Denotes the kind of input (e.g. buffer, stream or file based) for the data.
         */
        InputType inputKind;
        /*
         * Denotes the kind of xsl input (e.g. buffer, stream or file based) for the stylesheet.
         */
        InputType xslInputKind;
        /*
         * A pointer to the head element in a linked list of param_info structures.
         */
        param_info* parameters;
        /*
         * A pointer to the input data.
         */
        char * inputData;
        /*
         * A pointer to the stylesheet data.
         */
        char * stylesheetData;
    } transform_job;
    
    /* comms related data structures (e.g. for interacting with the emulator) */    
    
    /*
     * Describes the mode in which an xslt library adapter can 
     * pass response data to be returned to the emulator.
     */
    typedef enum {
        /* Indicates that data is passed in a buffer (i.e. char*). */
        Buffer = 1,
        /* Indicates that data is passed using the driver term format. */
        DriverTerm = 2,
		/* Indicates that data is passed using the external term format. */
		ExternalTermFormat = 3
    } ResponseFormat;
    
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
        unsigned long callerPid;
        /* Stores a pointer to the current request data. */
        transform_job* job;
    } request_context;
    
    /*
     * Stores response data operated on when calling back into the emulator.
     */
    typedef struct {
        /* Stores the current request context. */
        request_context* context;
        /* Specifies the response format. 
         Used to decode which field in #response we're 
         meant to be looking at. */
        ResponseFormat responseFormat;
        /* Stores the response size (either the buffer length or the 
         * number of terms in the supplied composite ErlDrvTermData array).
         * NB: This field is optional, as the protocol implementation performs
         * the same bounds checking regardless.
         */
        int responseSize;
        /* Stores the response data itself. - TODO: this feels rather "unportable" as an approach */
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
     * Gives the implementation provider a chance to initialize.
     * Returns a value from the 'DriverState' enumeration to indicate
     * current system state. A return value of DriverState::ProviderError
     * will cause the driver to exit, which the erlang port handler will
     * interpret as and error and unload the driver library.
     */
    typedef DriverState (*InitFunc)();

    /*
     * This function is the request handler hook used by plugins (such as 
     * implementations for sablotron and/or libxslt).
     * 
     * The response structure passed contains the context required to get to
     * underlying data, along with fields which the plugin can fill in to 
     * get data sent back to erlang.
     *
     */
    typedef void (*TransformFunc)(void* result); 
    
    /*
     * This function gives the implementing plugin a chance to cleanup 
     * after handle_request has returned. 
     *
     * Returns a value from the 'DriverState' enumeration to indicate
     * current system state. A return value of DriverState::ProviderError
     * will cause the driver to exit, which the erlang port handler will
     * interpret as and error and unload the driver library.
     */
    typedef DriverState (*PostTransformFunc)(void* result);
    
    /*
     * Gives the implementation provider a change to cleanup. Because this function
     * is only called when the driver is being stopped, the provider cannot supply
     * a meaningful return value, therefore the function returns void.
     */
    typedef void (*ShutdownFunc)();
   
	typedef struct {
		/* The following function pointers will need be set by the provider on startup */
		InitFunc 			initialize;
		TransformFunc 		handleTransform;
		PostTransformFunc	postHandle;
		ShutdownFunc		shutdown;
		/* Generic storage location, so providers can stash whatever they need. */
        void* providerData;
	} xsl_engine;

/******************************************************************************/    
    

#ifdef	__cplusplus
}
#endif

#endif	/* _ERLXSL_H */
