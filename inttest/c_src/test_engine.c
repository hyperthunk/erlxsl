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

#include "erlxsl.h"

#ifdef  __cplusplus
extern "C" {
#endif

/* INTERNAL DRIVER FUNCTIONS */
void init_engine(XslEngine*);
static EngineState default_handleTransform(Command*);
static EngineState default_postHandle(Command*);
static void default_shutdown(void*);
 
void init_engine(XslEngine *spec) {
  spec->transform = default_handleTransform;
  spec->after_transform = default_postHandle;
  spec->shutdown = default_shutdown;
};

static EngineState 
default_handleTransform(Command *command) {
  INFO("default_handleTransform\n");  
  
  ASSERT(command != NULL);
  ASSERT(command->result != NULL);
  
  XslTask* task = get_task(command);

  ASSERT(task != NULL);
  ASSERT(task->input_doc != NULL);  
  ASSERT(task->xslt_doc != NULL);

  DBG("Checking buffer size\n");  
  Int32 buffersize = (get_doc_size(task->input_doc) + get_doc_size(task->xslt_doc) + 1); 
  DBG("Assigning result buffer of %i\n", buffersize);  
  if (!assign_result_buffer(buffersize, command)) {
    return OutOfMemoryError;
  }
  
  DBG("concatenating documents\n");
  
  char *input = get_doc_buffer(task->input_doc);
  ASSERT(input != NULL);
  
  char *stylesheet = get_doc_buffer(task->xslt_doc);
  ASSERT(stylesheet != NULL);
  
  if (!write_result_buffer(input, command)) return OutOfMemoryError;
  if (!write_result_buffer(stylesheet, command)) return OutOfMemoryError;
  return Ok;
};

static EngineState 
default_postHandle(Command *result) {
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
