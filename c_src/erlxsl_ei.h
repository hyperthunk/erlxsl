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

#ifndef _EI_TEST
#include <ei.h>
#else
#include "ei_test.h"
#endif

/* FORWARD DEFINES */

#define _PID_NUM_SIZE     15

typedef unsigned int Uint;

// FIXME: type check against this definition:
//
// #if SIZEOF_VOID_P == SIZEOF_LONG
// typedef unsigned long Eterm;
// typedef unsigned long Uint;
// typedef long          Sint;
// #define ERTS_SIZEOF_ETERM SIZEOF_LONG
// #elif SIZEOF_VOID_P == SIZEOF_INT
// typedef unsigned int Eterm;
// typedef unsigned int Uint;
// typedef int          Sint;
// #define ERTS_SIZEOF_ETERM SIZEOF_INT
// #else
// #error Found no appropriate type to use for 'Eterm', 'Uint' and 'Sint'
// #endif

#define make_pid_data(Ser, Num) \
    ((Uint) ((Ser) << _PID_NUM_SIZE | (Num)))

#define DECODE_OK(decode) (decode == 0)

/* ERLANG INTERFACE FUNCTIONS */

static DriverState decode_ei_cmd(Command*, char*, int*);

/* Allocates all neccessary heap space for the next serialised term
   in the supplied buffer. If a mapping to an internal structure is known
   (i.e., registered in ei_type_mappings) then this type will be used, otherwise
   the following rules are applied:
   
   - default (ei) mappings are used for "primitive" types
   - lists are converted to a skip list and terms allocated by recursively applying the rule(s)
   - tuples are not supported and cause the funtion to return UnsupportedOperationError
*/
static DriverState decode_ei_cmd(Command *command, char *buf, int *index) {
  int type;
  int size = 0;
  int arity;
  DriverState state;

  if (!DECODE_OK(ei_get_type(buf, index, &type, &size))) {
    return DecodeError;
  }
  
  PropListItem *item; 
  // FIXME: rewrite this to generate and store PropListItem instead of writing directy to Command

  switch (type) {
    case ERL_SMALL_TUPLE_EXT:
      // the ONLY kind of tuple that we play with is a {tag, Value} pair. 
      // this is encoded as a PropListItem
      if (DECODE_OK(ei_decode_tuple_header(buf, index, &arity))) {
        if (arity != 2) {
          return UnsupportedOperationError;
        }
        if ((item = ALLOC(sizeof(PropListItem))) == NULL) {
          return OutOfMemory;
        }
        
        // safely allocates or does resize + append onto command->command_data.iov->payload.data
        write_cmd_data(item, command);
        
        while(arity > 0) {
          if ((state = decode_ei_cmd(command, buf, index)) != Success) {
            return state;
          }
          arity--;
        }
        state = Success;
      }
      break;
    case ERL_ATOM_EXT:
      INFO("processing atom at index %i\n", (*index));
      char *pcmd = ALLOC(sizeof(char) * MAXATOMLEN);
      if (DECODE_OK(ei_decode_atom(buf, index, pcmd))) {
        // atoms are always command strings
        item = (PropListItem*)command->command_data.iov->payload.data;
        item->tag = ALLOC(strlen(pcmd));
        // char *cmd = (char *)item->tag;
        strcpy((char *)item->tag, pcmd);
        INFO("assigned unpacked atom buffer %s\n", item->tag);
        state = Success;
      } else {
        state = DecodeError;
      }
      break;
    case ERL_STRING_EXT:
      INFO("processing string at index %i\n", (*index));
      char *data;
      if ((data = ALLOC(size + 1)) == NULL) {
        state = OutOfMemory;
      } else {
        if (DECODE_OK(ei_decode_string(buf, index, data))) {
          item = (PropListItem*)command->command_data.iov->payload.data;        
          if ((item->payload.buffer = strlen(data)) == NULL) {
            DRV_FREE(data); // we don't get another chance to free this buffer
            state = OutOfMemory;
          } else {
            INFO("assigning unpacked buffer %s\n", data);
            item->payload.buffer = strcpy(item->payload.buffer, data);
            state = Success;
          }
        }
      }
      break;
    case ERL_PID_EXT:
      INFO("processing pid at index %i\n", (*index));
      erlang_pid *pid; 
      if ((pid = ALLOC(sizeof(erlang_pid))) == NULL) {
        state = OutOfMemory;
      } else {
        if (DECODE_OK(ei_decode_pid(buf, index, pid))) {
          if ((command->command_data.iov = ALLOC(sizeof(DriverIOVec))) == NULL) {
            DRV_FREE(pid);
            state = OutOfMemory;
          } else {
            INFO("transforming pid number %i\n", (Int32)pid->num);
            ErlDrvTermData *data = ALLOC(sizeof(ErlDrvTermData));
            *data = make_pid_data(pid->serial, pid->num);
            command->command_data.iov->dirty = 1;
            command->command_data.iov->type = Term;
            command->command_data.iov->size = sizeof(erlang_pid);
            command->command_data.iov->payload.data = (void*)data;
            state = Success;
          }
        } else {
          state = DecodeError;
        }
      }
      break;
    default:  
      state = DecodeError;
  }
  return state;
};

#endif /* _ERLXSL_EI_H */

