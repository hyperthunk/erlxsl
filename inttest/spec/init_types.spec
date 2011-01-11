/*
 * init_types.spec
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
 * you're looking at generated code. For the original test specifications, please
 * look at the file with the .spec extension instead.
 */

#include "cspec.h"
#include "erlxsl.h"
#include "erlxsl_port.h"
#include "erlxsl_internal.h"

static char *payload = "some data....";

#define payload_size strlen(payload)

describe "Initializing DriverIOVec Structures"
  it "should set the 'dirty bit' to zero when there is no payload"
    DriverIOVec *iov = init_iov(Text, 0, NULL);
    
    iov->dirty should be 0;    
    iov->size should be 0;
    
    DRV_FREE(iov);
  end
  
  it "should set the 'dirty bit' to one when there is a payload"
    DriverIOVec *iov = init_iov(Text, strlen(payload), payload);
    char *buffer = iov->payload.buffer;

    iov->dirty should be 1;    
    iov->size should equal payload_size;
    iov->type should equal Text;
    buffer should point_to payload;

    DRV_FREE(iov);
  end  
end
