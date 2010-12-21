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
 * Implements an erlxsl test plugin.
 */

#include <erlxsl.h>


/*
 * Gives the implementation provider a chance to initialize.
 */
void init_provider() {
    DebugOut("Test provider initializing...");
}

void handle_request(void* response) {
    TransformResponse* result = (TransformResponse*)response;
    RequestContext* context = result->context;
    TransformRequestPtr request = context->request;  
    
    DebugOut("Input Data: %s\n......................\n", request->inputData);
    DebugOut("Xsl Data: %s\n......................\n", request->stylesheetData);
    if (NULL != request->parameters) {
        ParameterInfo* currentParam = request->parameters;
        while (NULL != currentParam) {
            DebugOut("Parameter Name: %s, Value: %s\n", currentParam->name, currentParam->value);
            currentParam = currentParam->next;
        }
    }
    
    char* data;
    data = "<response><name>it works!</name><value>WOW</value></response>";
    
/*
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, driver_mk_atom("response"),
        ERL_DRV_PORT, driver_mk_port((ErlDrvPort)context->port),
        ERL_DRV_BUF2BINARY, data, strlen(data),
        ERL_DRV_TUPLE, 3,
    };
    ErlDrvTermData* term = malloc(sizeof(spec));
*/
    
    //ErlDrvTermData* term = spec;
    result->payload.data = data; // memcpy(term, &spec, sizeof(spec)); // sizeof(spec) / sizeof(spec[0]) ????
    result->responseFormat = Buffer;
    result->responseSize = sizeof(data); // / sizeof(spec[0]);    
}

void post_handle_request(void* response) {
    TransformResponse* result = (TransformResponse*)response;
    free( result->payload.data );
}

/*
 * Gives the implementation provider a change to cleanup.
 */
void destroy_provider() {}
