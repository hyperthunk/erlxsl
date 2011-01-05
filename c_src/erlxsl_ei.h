/*
 * erlxsl_ei.h
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
 * This header contains functions and macros for working with ei (erlang interface).
 * This header *should* be included LAST of all, to ensure that dependant functions
 * and macros are already available.
 */

#ifndef _ERLXSL_EI_H
#define _ERLXSL_EI_H

/* FORWARD DEFINES */

/* ERLANG INTERFACE FUNCTIONS */

/* Allocates all neccessary heap space for the next serialised term
   in the supplied buffer. If a mapping to an internal structure is known
   (i.e., registered in ei_type_mappings) then this type will be used, otherwise
   the following rules are applied:
   
   - default (ei) mappings are used for "primitive" types
   - lists are converted to a skip list and terms allocated by recursively applying the rule(s)
   - tuples are not supported and cause the funtion to return UnsupportedOperationError
*/
/*
static DriverState alloc_ei(void **target, char *buf, int *index, int *size) {
  int type;
  
  
  return UnsupportedOperationError;
};
*/

#endif /* _ERLXSL_EI_H */
