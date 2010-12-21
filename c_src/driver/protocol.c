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

#include "protocol.h"

/*
 * Checks the validity of buffer size constraints against the supplied inputs.
 * Specifically, this checks that the expectedBufferSize does not exceed the
 * supplied bufflen. Returns true if the buffer sizes mismatch, otherwise true.
 *
 * If the check fails, this function also sets request->status to failureStatus
 * and request->errorMessage to errorMessage.
 */
static bool isBufferSizeMismatch(const Int32 expectedBufferSize, const Int32 bufflen,
        DriverState failureStatus, TransformRequestPtr request, char* errorMessage) {
    
    if (expectedBufferSize > bufflen) {
        DebugOut("Encounter expected buffer size of %i when expecting %i.\n",
                expectedBufferSize, bufflen);
        request->status = failureStatus;
        request->errorMessage = errorMessage;
        return true;
    } else {
        return false;
    }
}

/*
 * Sets the tail of the linked list pointed to by parameterListTail
 */
static void setNextParameter(ParameterInfo* parameterListHead,
        ParameterInfo* parameterListTail) {
    ParameterInfo* nextPointer;
    void * next = parameterListHead->next;
    if (NULL != next) {
        nextPointer = (ParameterInfo*)next;
        setNextParameter(nextPointer, parameterListTail);
    } else {
        parameterListHead->next = parameterListTail;
    }
}

/*
 * Recursively releases all the resources referenced by paramInfo and paramInfo itself.
 */
static void releaseParameterInfo(ParameterInfo* paramInfo) {
    if (paramInfo == NULL) {
        DebugOut("Attempted to release NULL ParameterInfo pointer.");
    }
    if (NULL != paramInfo->next) {
        releaseParameterInfo((ParameterInfo*)paramInfo->next);
    }
    TextDataOffsetPtr paramNameStorage = paramInfo->name;
    TextDataOffsetPtr paramValueStorage = paramInfo->value;
    free(paramNameStorage);
    free(paramValueStorage);
}

void releaseRequest(TransformRequestPtr request) {
    //TODO: get rid of all these debug statements
    if (NULL == request) {
        DebugOut("Attempt to release a NULL pointer to a request failed.\n");
        //TODO: consider logging instead of all these debug statements...
        return;
    }
    if (NULL != request->errorMessage) {
        DebugOut("Releasing error message.\n");
        free( request->errorMessage );
    }
    if (NULL != request->inputData) {
        DebugOut("Releasing input data.\n");
        free( request->inputData );
    }
    if (NULL != request->stylesheetData) {
        DebugOut("Releasing stylesheet data.\n");
        free( request->stylesheetData );
    }
    if (NULL != request->parameters) {
        DebugOut("Releasing parameter(s) data.\n");
        releaseParameterInfo(request->parameters);
    }
    free( request );
}

void releaseResponse(TransformResponse* response) {
    if (NULL != response) {
        releaseContext(response->context);
    }
    free(response);
}

void releaseContext(RequestContext* context) {
    /* This is very esoteric to my implementation:
     I think reference counting would be much cleaner,
     so perhaps going to C++/Boost and using smart_pointers
     would be a good idea.*/
    if (NULL != context) {
        releaseRequest(context->request);
    }
    free(context);
}

TransformRequestPtr unpackRequest(const char* const buffer, const Int32 bufflen) {
    Int32 expectedBufferSize = 0;
    TransformRequestPtr request;
    
    /* assign heap space for the request object, as we're returning it.
     * NB: this is the last time we'll use malloc directly, delegating
     * instead to our checked alloc (which also performs logging).*/
    request = malloc(sizeof(TransformRequest));
    if (request == NULL) {
        return(NULL);
    }
    
    /* set parameters up as a null pointer -
     * it will be reset to poitn to heap allocated space if
     * we encounter any values, otherwise it'll die along with the stack
     */
    ParameterInfo* parameters;
    parameters = NULL;
    
    expectedBufferSize += (sizeof(InputSpecHeaders) + sizeof(PayloadSizeHeaders));
    
    /*
     * Reinterpret the buffer pointer in the context of our expected structures.
     * We then increment the inputHeaders and then get our (new)
     * offset(s) to mark the data segment size(s)
     * NOTE: the recast pointers are stack allocated, so cleanup is automatic
     */
    const InputSpecHeaders* inputHeaders = (const InputSpecHeaders* const)buffer;
    const PayloadSizeHeaders* payloadSizeHeaders = (const PayloadSizeHeaders* )(inputHeaders + 1);
    const PayloadSizeHeaders dataSegmentSizes = *payloadSizeHeaders;
    payloadSizeHeaders++;
    
    const char* offset = (const char*)payloadSizeHeaders;
    //put parens around the increment and just use the pointer later on (e.g. (const char*)(PayloadSizeHeaders + 1);...)
    
    const Int16 argumentLength = inputHeaders->parameterGroupArityMarker;
    DebugOut("Argument length set to %i.\n", argumentLength);
    
    if (argumentLength > 0) {
        
        /* quick sanity check */
        expectedBufferSize += (argumentLength * (sizeof(RequestBufferArgumentOutline)));
        if (isBufferSizeMismatch(expectedBufferSize, bufflen,
                InconsistentInputHeaders, request, NULL)) {
            return(request);
        }
        
        /* look at the supplied argument headers a build up an array of 'em */
        Int16 param;
        RequestBufferArgumentOutline argHeaders[argumentLength];
        RequestBufferArgumentOutline* argumentOffset = (RequestBufferArgumentOutline*)offset;
        
        /* unpack the name and data-value sizes from the buffer */
        for (param = 0; param < argumentLength; param++) {
            /*
             * Note that we need a postfix increment here, to make sure the
             * offset is incremented after the final iteration, so it's usable
             * later on when we need to re-cast it for use in memcpy/strncpy
             */
            RequestBufferArgumentOutline argHeader = *argumentOffset;
            argHeaders[param] = argHeader;
            argumentOffset++;
            expectedBufferSize += (sizeof(char) *
                    (argHeader.argumentDataSize + argHeader.argumentNameDataSize));
        }
        
        /* sanity check for potential overflow */
        if (isBufferSizeMismatch(expectedBufferSize, bufflen,
                BufferSizeMismatch, request, NULL)) {
            return(request);
        }
        
        /* reinterpret the argumentOffset pointer into our char* offset */
        offset = (const char*)argumentOffset;
        
        /* process the arguments in turn */
        UInt16 nameSize, valueSize;
        for (param = 0; param < argumentLength; param++) {
            
            nameSize = argHeaders[param].argumentNameDataSize;
            valueSize = argHeaders[param].argumentDataSize;
            
            /* assign some space for the name and value */
            char* name = malloc((sizeof(char) * nameSize) + 1);
            if (name == NULL) {
                releaseRequest(request);
                return(NULL);
            }
            
            char* value = malloc((sizeof(char) * valueSize) + 1);
            if (value == NULL) {
                free(name);
                releaseRequest(request);
                return(NULL);
            }
            
            /* null terminate the buffers just in case the supplied
             * ones aren't properly terminated...*/
            memset(name, '\0', nameSize + 1);
            memset(value, '\0', valueSize + 1);
            
            /* and move.... */
            strncpy(name, offset, nameSize);
            offset += nameSize;
            strncpy(value, offset, valueSize);
            offset += valueSize;
            
            /* construct a new node and add it to the linked list! */
            ParameterInfo* nextParam = malloc(sizeof(ParameterInfo));
            if (nextParam == NULL) {
                free(name);
                free(value);
                releaseRequest(request);
                return(NULL);
            }
            
            nextParam->name = name, nextParam->value = value,
                    nextParam->next = NULL;
            
            if (NULL == parameters) {
                parameters = nextParam;
            } else {
                setNextParameter(parameters, nextParam);
            }
        }
    }
    
    /* one final sanity check re: the available buffer length */
    expectedBufferSize += (
            dataSegmentSizes.inputDataSizeMarker +
            dataSegmentSizes.xslDataSizeMarker
            );
    if ( isBufferSizeMismatch(expectedBufferSize, bufflen,
            BufferSizeMismatch, request, NULL)) {
        return(request);
    }
    
    /* on to the actual data/stylesheet  */
    
    /* TODO: here we assume that offset is in the correct position to start copying data -> don't we!? */
    char* inputData = malloc(
        (sizeof(char) * dataSegmentSizes.inputDataSizeMarker) + 1);
    if (NULL == inputData) {
        releaseRequest(request);
        return(NULL);
    }
    
    char* xslData = malloc(
        (sizeof(char) * dataSegmentSizes.xslDataSizeMarker) + 1);
    if (NULL == xslData) {
        releaseRequest(request);
        return(NULL);
    }
    
    memset(inputData, '\0', dataSegmentSizes.inputDataSizeMarker + 1);
    memset(xslData, '\0', dataSegmentSizes.xslDataSizeMarker + 1);
    
    inputData = strncpy(inputData, offset, dataSegmentSizes.inputDataSizeMarker);
    offset += dataSegmentSizes.inputDataSizeMarker;
    xslData = strncpy(xslData, offset, dataSegmentSizes.xslDataSizeMarker);
    
    request->inputKind = inputHeaders->inputKindMarker;
    request->inputData = inputData;
    request->xslInputKind = inputHeaders->xslInputKindMarker;
    request->stylesheetData = xslData;
    request->parameters = parameters;
    request->status = Ok;
    return(request);
}

/*
 * Processes the supplied response and sends it to the appropriate
 * erlang process (e.g. the specified receiver).
 *
 * If no receiver is supplied, the value of the callerPid field in the
 * response is used.
 */
void sendResponse(/*ErlDrvPort thePort,*/ TransformResponse* response, ErlDrvTermData* receiver) {
    RequestContext* context = response->context;
    ErlDrvPort port = (ErlDrvPort)context->port;
    ErlDrvTermData calleePid;
    if (NULL == receiver) {
        calleePid = (ErlDrvTermData)context->callerPid;
    } else {
        calleePid = *receiver;
    }
    ErlDrvTermData* term = NULL;
    if (response->responseFormat == Buffer) {
        char* payload = (char*)response->payload.buffer;
        ErlDrvTermData spec[] = {
            ERL_DRV_ATOM, driver_mk_atom("response"),
            ERL_DRV_PORT, driver_mk_port(port),
            ERL_DRV_BUF2BINARY, payload, strlen(payload),
            ERL_DRV_TUPLE, 3
        };
        term = malloc(sizeof(spec));
        if (NULL == term) {
            E_OUT_OF_MEMORY;
        }
        driver_send_term(port, calleePid,
                memcpy(term, &spec, sizeof(spec)), sizeof(spec) / sizeof(spec[0]));
        //response->responseSize = sizeof(spec) / sizeof(spec[0]);
    } else if (response->responseFormat == DriverTerm) {
        //TODO: check to make sure there is some valid data, otherwise 
        //      return an error tuple!
        if (NULL == term) {
            term = (ErlDrvTermData*)response->payload.data;
            if (response->responseSize <= 0) {
                response->responseSize = sizeof(term) / sizeof(term[0]);
            }
            driver_send_term(port, calleePid, term, response->responseSize);
        }
    } else {
        DebugOut("WOW - what exactly is going on here!?");
        //TODO: error condition(s)!?
    }
}
