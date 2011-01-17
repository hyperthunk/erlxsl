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

static char *output_data = "format %s\n";
static char *command_string_transform = "transform";
static char *command_foo = "command_foo";
static char *assigned_data = "data";
static char *combined_data = "datadata";

describe "ALLOC macro expansion"

  it "should use the supplied (underlying) function!"
    char *foo = ALLOC(sizeof(char) * 10);
    foo should not equal NULL
    DRV_FREE(foo);
  end

end

describe "Extracting an XslTask from a Command using get_task"  
  
  it "should return NULL for a NULL Command pointer"
    Command *c = NULL;
    get_task(c) should be NULL;
  end
  
  it "should return NULL for a command string other than 'transform'"
    Command c = { .command_string="foobar" };
    Command *cmd = &c;
    get_task(cmd) should be NULL;
  end
  
  it "should return its task, when the command string is 'transform'"
    create_test_data(test_command, command_string_transform);
    XslTask *task = ALLOC(sizeof(XslTask));
    clear_task_fields(task);
    Command *cmd = init_command(test_command, NULL, task, NULL);
    get_task(cmd) should point_to task;
    
    free_command(cmd);
  end
  
end

describe "Obtaining a char buffer from a DriverIOVec using the get_buffer macro"
  
  it "should return NULL for a NULL DriverIOVec"
    DriverIOVec *iov = NULL;
    get_buffer(iov) should be NULL;
  end
  
  it "should return NULL when the format is not Text"
    create_test_data(test_data, command_string_transform);
    DriverIOVec *iov = init_iov(Opaque, strlen(test_data), test_data);
    get_buffer(iov) should be NULL;
    free_iov(iov);
  end

  it "should return the buffer value for a DriverIOVec when the format is Text"
    create_test_data(test_data, command_string_transform);
    DriverIOVec *iov = init_iov(Text, strlen(test_data), test_data);
    get_buffer(iov) should point_to test_data;
    free_iov(iov);    
  end
  
end

describe "Assigning a result buffer to a Command using the supplied macro"

  it "should evaluate to NULL when the DriverIOVec argument to make_result_buffer is NULL"
    Command *cmd = NULL;
    make_result_buffer(255, cmd) should be NULL;
  end
  
  it "should evaluate to NULL when the Command argument to ensure_buffer is NULL"
    Command *cmd = NULL;
    ensure_buffer(command_string_transform, cmd) should be NULL;
  end
  
  it "should evaluate to NULL when the buffer argument to ensure_buffer is NULL"
    Command *cmd = init_command(command_foo, NULL, NULL, NULL);
    ensure_buffer(NULL, cmd) should be NULL;
    free_command(cmd);
  end
  
  it "should allocate and size the DriverIOVec only when making an initial result buffer"
    Command *cmd = init_command(command_foo, NULL, NULL, NULL);
    make_result_buffer(255, cmd);
    
    // sanity checking...
    DriverIOVec *iov = cmd->result;
    iov->dirty should be 0;
    iov->size should be 255;
    strlen(cmd->result->payload.buffer) should equal 0;
    free_command(cmd);
  end 

  it "should succeed when the Command's result already contains space for the supplied buffer"
    Command *cmd = init_command(command_foo, NULL, NULL, NULL);
    make_result_buffer(255, cmd);
    
    create_test_data(test_command, command_string_transform);
    ensure_buffer(test_command, cmd) should point_to cmd->result->payload.buffer;
    
    strlen(cmd->result->payload.buffer) should equal 0;
    free_command(cmd);
  end 

  it "should evaluate to NULL when the Command result DriverIOVec has not been properly assigned"
    create_test_data(test_command, command_string_transform);
    Command *cmd = init_command(test_command, NULL, NULL, NULL);
    // this is an edge case.....
    free_iov(cmd->result);
    cmd->result = NULL;

    make_result_buffer(0, cmd) should be NULL;
    free_command(cmd);
  end
  
  it "should assign a buffer of the requested size when the result is clean"
    create_test_data(test_command, command_string_transform);
    create_test_data(test_data, command_string_transform);
    Command *cmd = init_command(test_command, NULL, NULL, NULL);
    int len = strlen(test_data);
    
    make_result_buffer(len, cmd) should not be NULL;
    DriverIOVec *iov = cmd->result;
    
    iov should not be NULL;
    iov->dirty should equal 0;  
    iov->type should equal Text;
    iov->size should equal strlen(test_data);
    
    free_command(cmd);
  end

  it "should ensure that a buffer is allocated when it is initially NULL"
    create_test_data(test_command, command_string_transform);
    create_test_data(test_data, command_string_transform);
    Command *cmd = init_command(test_command, NULL, NULL, NULL);
    int len = strlen(test_data);
    
    ensure_buffer(test_data, cmd) should not be NULL;

    DriverIOVec *iov = cmd->result;
    iov->dirty should equal 0;  
    iov->type should equal Text;
    iov->size should equal strlen(test_data);
    
    free_command(cmd);
  end

  it "should resize the underlying buffer when necessary"
    create_test_data(test_command, command_string_transform);
    create_test_data(test_data, assigned_data);
    Command *cmd = init_command(test_command, NULL, NULL, NULL);
    int len = strlen(test_data);

    make_result_buffer(len, cmd) should not be NULL;
    DriverIOVec *iov = cmd->result;
    iov->size should equal len;

    ensure_buffer(combined_data, cmd);
    iov->size should equal strlen(combined_data);

    free_command(cmd);
  end
 
  it "should append space to an existing buffer when required"
    create_test_data(test_command, command_string_transform);
    create_test_data(test_data, assigned_data);
    Command *cmd = init_command(test_command, NULL, NULL, NULL);
    
    write_result_buffer(test_data, cmd) should not be NULL;

    DriverIOVec *iov = cmd->result;
    iov->size should equal strlen(test_data);

    ensure_buffer(test_data, cmd) should not be NULL;

    iov->size should equal strlen(combined_data);

    // quick sanity check that should show we have not written any additional data
    char *buff = iov->payload.buffer;
    buff should be_equal_to test_data;
    
    free_command(cmd);
  end

  it "should not append space when sufficient buffer space is already allocated"
    create_test_data(test_command, command_string_transform);
    create_test_data(test_data, assigned_data);
    Command *cmd = init_command(test_command, NULL, NULL, NULL);
    
    make_result_buffer(255, cmd) should not be NULL;
    write_result_buffer(test_data, cmd) should not be NULL;

    DriverIOVec *iov = cmd->result;
    iov->size should equal 255;

    write_result_buffer(test_data, cmd) should not be NULL;

    iov->size should equal 255;
    free_command(cmd);
  end
  
  it "should append data to an existing buffer when it is already dirty"
    create_test_data(test_command, command_string_transform);
    create_test_data(test_data, assigned_data);
    Command *cmd = init_command(test_command, NULL, NULL, NULL);
    
    write_result_buffer(test_data, cmd) should not be NULL;

    DriverIOVec *iov = cmd->result;
    iov should not be NULL;
    iov->size should equal strlen(test_data);
    iov->dirty should be 1;

    write_result_buffer(test_data, cmd) should not be NULL;
    size_t len = (size_t)iov->size;
    len should equal strlen(combined_data);
    // quick sanity check that should show we have not written any additional data
    char *buff = iov->payload.buffer;

    buff should be_equal_to combined_data;
  end
  
end
  
