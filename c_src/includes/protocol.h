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
 * Describes a proprietary protocol between erlang and our xsl driver.
 */

#include "erlxsl.h"

/* erts includes */
#include <erl_driver.h>

#ifndef _PROTOCOL_H
#define	_PROTOCOL_H

#ifdef	__cplusplus
extern "C" {
#endif
    
    /*
 * TODO: this (below) is a very cheeky macro - we're being super-lazy, 
 * which is bad -> we should probably force the parameter in (if we must
 * use macros at all).
 */

/* NB: this macro assumes the presence of a port within 
 the scope in which it is expanded! */
#define E_OUT_OF_MEMORY (driver_failure_atom(port, "system_limit"));

    /* internal data structures */
    
    /*
     * Stores three headers used to identify the kind of input uris 
     * (e.g. file or buffer/memory) and the number of parameters being
     * supplied (arity).
     */
    typedef struct input_spec_headers {
        /* The position in which we're expecting the input kind value. */
        Int16 inputKindMarker;
        /* The position in which we're expecting the xsl input kind value. */
        Int16 xslInputKindMarker;
        /*
         * The position in which we're expecting a value
         * denoting the number of parameters being supplied.
         */
        Int16 parameterGroupArityMarker;
    } InputSpecHeaders;
    
    /*
     * Headers that specify the data size for the payload (i.e. the size
     * of the input and stylesheet buffers).
     */
    typedef struct datasize_headers {
        /* The position in which we're expecting the input size marker value */
        Int32 inputDataSizeMarker;
        /* The position in which we're expecting the xsl size marker value */
        Int32 xslDataSizeMarker;
    } PayloadSizeHeaders;
    
    /*
     * Contains a structural outline of a single argument within the 
     * request buffer, specifying the size of the argument name and value buffers.
     */
    typedef struct request_buffer_argument_outline {
        /* size of the argument name string in the buffer. */
        Int16  argumentNameDataSize;
        /* size of the argument value string in the buffer. */
        Int16  argumentDataSize;
    } RequestBufferArgumentOutline;
    
    /*
     * Cleans up all the resources which the supplied 
     * response is referencing and response itself. 
     */
    void releaseResponse(TransformResponse* response);
    
    /*
     * Cleans up all the resources, which *only* the supplied 
     * context is referencing and context itself. 
     */
    void releaseContext(RequestContext* context);
    
    /*
     * Cleans up all the resources which the supplied 
     * request is referencing and request itself.
     */
    void releaseRequest(TransformRequestPtr request); 
    
    /*
     * Unpacks *buffer* and populates a TransformRequest structure, used by the
     * driver to perform the actual processing.
     */
    TransformRequestPtr unpackRequest(const char* const buffer, const Int32 bufflen);
    
    /*
     * Processes the supplied response and sends it to the appropriate 
     * erlang process (e.g. the specified receiver).
     *
     * If no receiver is supplied, the value of the callerPid field in the 
     * response is used. 
     */
    void sendResponse(/*ErlDrvPort port, */ TransformResponse* response, ErlDrvTermData* receiver);
    
#ifdef	__cplusplus
}
#endif

#endif	/* _PROTOCOL_H */
