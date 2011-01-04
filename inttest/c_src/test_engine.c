/*
 * default_provider.c
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
 *
 */

#include "erlxsl_api.h"

#ifdef  __cplusplus
extern "C" {
#endif

/* INTERNAL DRIVER FUNCTIONS */
void init_engine(xsl_engine*);
static EngineState default_handleTransform(transform_result*);
static EngineState default_postHandle(transform_result*);
static void default_shutdown(void*);
 
void init_engine(xsl_engine *spec) {
  spec->transform = default_handleTransform;
  spec->after_transform = default_postHandle;
  spec->shutdown = default_shutdown;
};

static EngineState 
default_handleTransform(transform_result *result) {
  INFO("default_handleTransform\n");  
  transform_result *res = (transform_result*)result;
  transform_job *job = (res->context)->job;
  char *output = malloc(sizeof(char) * ((strlen(job->input) + strlen(job->stylesheet)) + 1)); 
  if (output == NULL) {
    return OutOfMemoryError;
  }
  strcpy(output, job->input);
  strcat(output, job->stylesheet);
  res->format = Text;
  res->payload.buffer = output;
  return Ok;
};

static EngineState 
default_postHandle(transform_result *result) {
  INFO("default_postHandle\n");
  return Ok;
};

static void 
default_shutdown(void *state) {
  INFO("default_shutdown\n");
};

#ifdef  __cplusplus
}
#endif
