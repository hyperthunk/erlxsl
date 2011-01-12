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

describe "Assigning a result buffer to a Command using the assign_result_buffer macro"
  
  it "should evaluate to NULL when the Command reference is NULL"
    Command *cmd = NULL;
    assign_result_buffer(0, cmd) should be NULL;
  end
  
  it "should evaluate to NULL when the Command result DriverIOVec has not been properly assigned"
    create_test_data(test_command, command_string_transform);
    Command *cmd = init_command(test_command, NULL, NULL, NULL);
    cmd->result should not be NULL;
    // this is an edge case.....
    free_iov(cmd->result);
    cmd->result = NULL;

    assign_result_buffer(0, cmd) should be NULL;
    free_command(cmd);
  end
  
  it "should assign a buffer of the requested size"
    create_test_data(test_command, command_string_transform);
    create_test_data(test_data, command_string_transform);
    Command *cmd = init_command(test_command, NULL, NULL, NULL);
    
    assign_result_buffer(strlen(test_data), cmd);
    DriverIOVec *iov = cmd->result;
    
    iov should not be NULL;
    iov->type should equal Text;
    iov->size should equal strlen(test_data);
    
    free_command(cmd);
  end
  
end

describe "Writing output to a Command result buffer (i.e., DriverIOVec) using the write_result_buffer macro"

  it "should assign the requisite space and copy the supplied buffer when the payload.buffer is unassigned"
    create_test_data(test_command, command_foo);
    Command *cmd = init_command(test_command,
                                NULL, NULL,
                                init_iov(Text, strlen(test_command), test_command));

    DriverIOVec *result = cmd->result;
    
    // should set the actual payload.buffer to NULL
    clear_result_buffer(cmd);

    // a bit of a sanity check is needed here
    result->dirty should equal 0;
    char *result_buff = result->payload.buffer;
    result_buff should be NULL;
    
    write_result_buffer(test_command, cmd);
    
    // the DriverIOVec result is still properly assigned
    result->size should equal strlen(test_command);
    result->dirty should equal 1;
    
    // both allocation and copying should have succeeded - we find out now
    result_buff = result->payload.buffer;
    
    result_buff should be_equal_to test_command;
    
    free_command(cmd);
  end

end
