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
    *test_type = ERL_ATOM_EXT; \
    do { \
      expr; \
    } while (false)\
  } while(false)

#define with_string(s, expr) \
  do { \
    *test_type = ERL_STRING_EXT; \
    do { \
      expr; \
    } while (false)\
  } while(false)

#define with_tuple(size, expr) \
  do { \
    *test_type = ERL_SMALL_TUPLE_EXT; \
    *test_arity = size; \
    do { \
      expr; \
    } while (false)\
  } while(false)

#define with_ei_fail(expr) \
  do { \
    test_fail = true; \
    expr; \
    test_fail = false; \
  } while (false)

static int ei_get_type(const char *buf, const int *index, int *type, int *size);
static int ei_decode_string(const char *buf, int *index, char *p);
static int ei_decode_atom(const char *buf, int *index, char *p);
static int ei_decode_tuple_header(const char *buf, int *index, int *arity);

static char *test_buff = NULL;
static int *test_type;
static bool test_fail = false;
static int *test_arity;

static int ei_get_type(const char *buf, const int *index, int *type, int *size) {
  // buf is ignored...
  INFO("checking %i\n", test_fail);
  if (test_fail == true) {
    puts("failing");
    return 1;
  }
  *type = *test_type;
  return 0;
};

static int ei_decode_tuple_header(const char *buf, int *index, int *arity) {
  // buf is ignored...
  if (test_fail) return 1;
  *arity= *test_arity;
  (*index)++;
  return 0;
};

static int ei_decode_atom(const char *buf, int *index, char *p) {
  return ei_decode_string(buf, index, p);
}

static int ei_decode_string(const char *buf, int *index, char *p) {
  // buf is ignored...
  if (test_fail) return 1;
  int i = *index;
  *index = (i += strlen(test_buff));
  strcpy(p, test_buff);
  return 0;
};

#endif /* _SPEC_EI_H */
