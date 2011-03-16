/*
 * so_loading.spec
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
 * Notes: If you're looking at a version of this file with the extension .c then
 * you're looking at generated code. For the original test specifications,
 * please look at the file with the .spec extension instead.
 */

#include "cspec.h"
#include "spec_includes.h"
#include "ei_test.h"
#include "erlxsl_ei.c"

describe "Decoding Buffers using EI"

  it "should return DecodeError when the initial type check fails"
    char *buf = "12345";
    int index = 0;
    Command *cmd = ALLOC(sizeof(Command));
    DriverState state;

    with_ei_fail(state = decode_ei_cmd(cmd, buf, &index));
    state should be DecodeError;
    free_command(cmd);
  end

  it "should return DecodeError when the initial type check returns an unexpected value"
    char *buf = "12345";
    int index = 0;
    Command *cmd = ALLOC(sizeof(Command));
    DriverState state;

    with_type(ERL_FUN_EXT, state = decode_ei_cmd(cmd, buf, &index));
    state should be DecodeError;
    free_command(cmd);
  end

  it "should return OutOfMemory when allocation fails"
    char *buf = "12345";
    int index = 0;
    int tuple_size = 2;
    Command *cmd = ALLOC(sizeof(Command));
    DriverState state;

    with_tuple(tuple_size,
      do_with_nomem(state = decode_ei_cmd(cmd, buf, &index))
    );
    state should be OutOfMemory;
    free_command(cmd);
  end

end

