/*
 * ei_test.h
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
 * EI Test Functions/Utilities
 *
 */

#ifndef _SPEC_EI_H
#define _SPEC_EI_H

#define MAXATOMLEN 255

#define ERL_SMALL_INTEGER_EXT 'a'
#define ERL_INTEGER_EXT       'b'
#define ERL_FLOAT_EXT         'c'
#define ERL_ATOM_EXT          'd'
#define ERL_REFERENCE_EXT     'e'
#define ERL_NEW_REFERENCE_EXT 'r'
#define ERL_PORT_EXT          'f'
#define ERL_PID_EXT           'g'
#define ERL_SMALL_TUPLE_EXT   'h'
#define ERL_LARGE_TUPLE_EXT   'i'
#define ERL_NIL_EXT           'j'
#define ERL_STRING_EXT        'k'
#define ERL_LIST_EXT          'l'
#define ERL_BINARY_EXT        'm'
#define ERL_SMALL_BIG_EXT     'n'
#define ERL_LARGE_BIG_EXT     'o'
#define ERL_NEW_FUN_EXT	      'p'
#define ERL_FUN_EXT	          'u'

#define with_atom(s, expr) \
  do { \
    fixture->test_type = ERL_ATOM_EXT; \
    fixture->test_buff = s; \
    do { \
      expr; \
    } while (false) \
  } while(false)

#define with_string(s, expr) \
  do { \
    fixture->test_type = ERL_STRING_EXT; \
    fixture->test_buff = s; \
    do { \
      expr; \
    } while (false)\
  } while(false)

#define with_tuple(size, expr) \
  do { \
    fixture->test_type = ERL_SMALL_TUPLE_EXT; \
    fixture->test_arity = size; \
    expr; \
  } while(false)

#define with_type(type, expr) \
  do { \
    fixture->test_type = type; \
    expr; \
  } while (false)

#define with_ei_fail(expr) \
  do { \
    fixture->test_fail = true; \
    expr; \
  } while (false)

#define setup_fixture \
  do { \
    fixture = ALLOC(sizeof(TestState)); \
    fixture->test_buff = NULL;  \
    fixture->test_type = -1;  \
    fixture->test_fail = false; \
    fixture->test_arity = -1; \
    fixture->next = current_fixture; \
    current_fixture = fixture; \
  } while (false)

static DriverState decode_ei_cmd(Command*, char*, int*);
static DriverState decode_ei_cmd_impl(Command*, char*, int*);
static int ei_get_type(const char *buf, const int *index, int *type, int *size);
static int ei_decode_string(const char *buf, int *index, char *p);
static int ei_decode_atom(const char *buf, int *index, char *p);
static int ei_decode_tuple_header(const char *buf, int *index, int *arity);

typedef struct test_state {
  char *test_buff;
  int test_type;
  bool test_fail;
  int test_arity;
  struct test_state* next;
} TestState;

static TestState* fixture = NULL;
static TestState* current_fixture = NULL;

static DriverState decode_ei_cmd(Command *command, char *buf, int *index) {
  DriverState state;
  while (current_fixture != NULL) {
    state = decode_ei_cmd_impl(command, buf, index);
    current_fixture = current_fixture->next;
  }
  // FIXME: release all the fixtures...
  return state;
};

static int ei_get_type(const char *buf, const int *index, int *type, int *size) {
  // buf is ignored...
  if (current_fixture->test_fail == true) {
    return 1;
  }
  *type = current_fixture->test_type;
  return 0;
};

static int ei_decode_tuple_header(const char *buf, int *index, int *arity) {
  // buf is ignored...
  if (current_fixture->test_fail) return 1;
  *arity= current_fixture->test_arity;
  (*index)++;
  return 0;
};

static int ei_decode_atom(const char *buf, int *index, char *p) {
  return ei_decode_string(buf, index, p);
}

static int ei_decode_string(const char *buf, int *index, char *p) {
  // buf is ignored...
  if (current_fixture->test_fail) return 1;
  int i = *index;
  *index = (i += strlen(current_fixture->test_buff));
  strcpy(p, current_fixture->test_buff);
  return 0;
};

#endif /* _SPEC_EI_H */
