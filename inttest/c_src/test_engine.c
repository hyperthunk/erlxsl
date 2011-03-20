/*
 * test_engine.c
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
 * Notes: This *test* driver is used to validate the interactions between the
 * port controller and the linked in driver and port program code.
 */

#include "erlxsl.h"

#ifdef    __cplusplus
extern "C" {
#endif

/* INTERNAL DRIVER FUNCTIONS */
void init_engine(XslEngine*);
static EngineState default_handleTransform(Command*);
static EngineState default_postHandle(Command*);
static void default_shutdown(void*);
static int lookup_command(char*);
static void setup_stub(int, char*);
static EngineState default_command(Command*);

/* STORAGE */

#define TRANSFORM_CMD 1
#define AFTER_TRANSFORM_CMD 2
#define SHUTDOWN_CMD 3
#define UNKNOWN_CMD -1

static struct target_codes {
    char *command;
    int code;
} codes[] = {
    {"transform", TRANSFORM_CMD},
    {"after_transform", AFTER_TRANSFORM_CMD},
    {"shutdown", SHUTDOWN_CMD}
};

// easily abused, knowledge of this array (e.g. it's size) is just assumed
static char *target_data[] = { NULL, NULL, NULL, NULL };

void init_engine(XslEngine *spec) {
    spec->transform = default_handleTransform;
    spec->after_transform = default_postHandle;
    spec->shutdown = default_shutdown;
    spec->command = default_command;
};

static int lookup_command(char *cmd) {
    int i;
    for(i = 0; i < (sizeof(codes) / sizeof(codes[0])); i++) {
        if(strcmp(codes[i].command, cmd) == 0) {
            return codes[i].code;
        }
    }
    return UNKNOWN_CMD;
};

static void setup_stub(int cmd, char *data) {
    if (target_data[cmd] != NULL) {
        free(target_data[cmd]);
    }
    target_data[cmd] = data;
};

static void process_cmd(CmdData *cd) {
    setup_stub(TRANSFORM_CMD, cd->payload.buffer);
};

static EngineState
default_command(Command* cmd) {
    CmdData *cd = get_cmd_data(cmd);
    switch (lookup_command(cd->tag)) {
    case TRANSFORM_CMD:
        // for now let's assume we've been handed something
        // which should should be 'returned' from transform
        process_cmd(cd);
    }
    return Ok;
};

static EngineState
default_handleTransform(Command *command) {
    INFO("default_handleTransform\n");

    assert(command != NULL);
    assert(command->result != NULL);

    XslTask* task = get_task(command);

    assert(task != NULL);
    assert(task->input_doc != NULL);
    assert(task->xslt_doc != NULL);

    Int32 buffersize = (get_doc_size(task->input_doc) + get_doc_size(task->xslt_doc) + 1);
    DBG("Assigning result buffer of %i\n", buffersize);
    if (!make_result_buffer(buffersize, command)) {
        return OutOfMemoryError;
    }

    char *input = get_doc_buffer(task->input_doc);
    assert(input != NULL);
    char *stylesheet = get_doc_buffer(task->xslt_doc);
    assert(stylesheet != NULL);

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

#ifdef __cplusplus
}
#endif
