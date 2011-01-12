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
 * you're looking at generated code. For the original test specifications, please
 * look at the file with the .spec extension instead.
 */

#include "cspec.h"
#include "spec_includes.h"

static char *stub_entry_point = "initialize_me";
static char *bad_image_name = "no_such_library.so";
static char *good_image_name = "bin/lib_stub.so";

// FIXME: need a cross platform build that copes with different path schemes

static char *ok_image_name_bad_format = "priv/test/bin/test_harness";

#define match_have_error(A, E) \
  match_be_equal_to(A, E)

#define BAD_IMAGE_ERROR       \
    "dlopen(priv/test/bin/test_harness, 2): no suitable image found." \
    "  Did find:\n\t../priv/test/bin/test_harness: can't map"

describe "Loading XslEngine from a Shared Library (so)"
  it "should guard against null loader specs"
    load_library(NULL);
  end
  
  it "should yield error data for invalid library names"
    LoaderSpec *loader = ALLOC(sizeof(LoaderSpec));
    
    loader->name = bad_image_name;
    load_library(loader);
    
    loader->library should be NULL;
    loader->error_message should have_error "dlopen(no_such_library.so, 2): image not found";
    
    free(loader);
  end
  
  it "should yield error data for images of invalid type/format"
    LoaderSpec *loader = ALLOC(sizeof(LoaderSpec));
    
    loader->name = ok_image_name_bad_format;
    load_library(loader);
    
    loader->library should be NULL;
    loader->error_message should have_error BAD_IMAGE_ERROR;
    
    free(loader);
  end

  it "should yield error data for images missing the correct entry point"
    LoaderSpec *loader = ALLOC(sizeof(LoaderSpec));

    loader->name = good_image_name;
    load_library(loader);

    loader->library should not be NULL;
    loader->error_message should include "init_engine): symbol not found";

    free(loader);
  end

  it "should load the correct entry point (when present)"
    LoaderSpec *loader = ALLOC(sizeof(LoaderSpec));

    loader->name = good_image_name;
    load_library(loader);

    loader->library should not be NULL;
    loader->init_f = dlsym(loader->library, stub_entry_point);    
    loader->init_f should not be NULL;

    free(loader);
  end
end
