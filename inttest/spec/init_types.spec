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
#include "spec_includes.h"

#define transform         \
 "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>" \
 "\n<xsl:template match='/'><xsl:copy-of select='.' /></xsl:template>\n" \
 "</xsl:stylesheet>"

static char *payload = "some data....";
static char *input_doc = "<doc><input @name='foobar'/></doc>";
static char *xsl_doc = transform;
static char *command_string = "transform";
static char *internal_command_string = "command123";

#define payload_size strlen(payload)

#define match_have_failed_due_to(A, E) \
    (strncmp(A, E, strlen(E)) == 0)

#define setup_size_headers(Hsz, Xml, Xsl)    \
    Hsz.input_size = strlen(Xml); \
    Hsz.xsl_size = strlen(Xsl)

#define setup_spec_headers(Spec, In, Xsl, Pg) \
    Spec.input_kind = (Int8)In; \
    Spec.xsl_kind = (Int8)Xsl;    \
    Spec.param_grp_arity = (Int16)Pg

describe "Initializing DriverIOVec Structures"

    it "should return NULL when allocation fails"
        DriverIOVec *iov;
        do_with_nomem(iov = init_iov(Text, 0, NULL));
        iov should be NULL;
    end

    it "should set the 'dirty bit' to zero when there is no payload"
        DriverIOVec *iov = init_iov(Text, 0, NULL);

        iov->dirty should be 0;
        iov->size should be 0;

        free_iov(iov);
    end

    it "should set the 'dirty bit' to one when there is a payload"
        create_test_data(test_data, payload);
        DriverIOVec *iov = init_iov(Text, strlen(payload), test_data);
        char *buffer = iov->payload.buffer;

        iov->dirty should be 1;
        iov->size should equal payload_size;
        iov->type should equal Text;
        buffer should point_to test_data;

        free_iov(iov);
    end

    it "should set the 'data' union member rather than the 'buffer', when payload is marked as Opaque"
        create_test_data(test_data, payload);
        DriverIOVec *iov = init_iov(Opaque, strlen(test_data), test_data);
        char *buffer = iov->payload.buffer;
        void *data = iov->payload.data;

        iov->dirty should be 1;
        iov->size should equal payload_size;
        iov->type should equal Opaque;

        data should point_to test_data;

        DRV_FREE(iov);
    end

    it "should set the 'data' union member rather than the 'buffer', when payload is marked as Binary"
        create_test_data(test_data, payload);
        DriverIOVec *iov = init_iov(Binary, strlen(test_data), test_data);
        char *buffer = iov->payload.buffer;
        void *data = iov->payload.data;

        iov->dirty should be 1;
        iov->size should equal payload_size;
        iov->type should equal Binary;

        data should point_to test_data;

        free_iov(iov);
    end

    it "should set the 'data' union member rather than the 'buffer', when payload is marked as Object"
        create_test_data(test_data, payload);
        DriverIOVec *iov = init_iov(Object, strlen(test_data), test_data);
        void *data = iov->payload.data;

        iov->dirty should be 1;
        iov->size should equal payload_size;
        iov->type should equal Object;

        data should point_to test_data;

        free_iov(iov);
    end

end

describe "Initializing InputDocument Structures"
    it "should return NULL when allocation fails"
        InputDocument *doc;
        do_with_nomem(doc = init_doc(Stream, 0, NULL));
        doc should be NULL;
    end

    it "should set the input type to text and size the DriverIOVec appropriately"
        create_test_data(test_data, payload);
        InputDocument *doc = init_doc(Stream, strlen(test_data), test_data);
        DriverIOVec *iov = doc->iov;
        char *buffer = doc->iov->payload.buffer;

        doc->type should be Stream;
        iov->type should be Text;
        iov->size should equal payload_size;
        buffer should point_to test_data;

        free_document(doc);
    end
end

describe "Initializing XslTask Structures"
    it "should fail for NULL task pointers"
        DriverState state;

        state = init_task(NULL, NULL, NULL, NULL, NULL);
        state should be BadArgumentError;
    end

    it "should fail for NULL PayloadSize"
        DriverState state;
        XslTask *task = ALLOC(sizeof(XslTask));
        state = init_task(task, NULL, NULL, NULL, NULL);
        state should be BadArgumentError;
        free_task(task);
    end

    it "should fail for NULL CmdData"
        DriverState state;
        PayloadSize hsize;
        XslTask *task = ALLOC(sizeof(XslTask));
        state = init_task(task,
                        (const PayloadSize* const)&hsize,
                        NULL, NULL, NULL);
        state should be BadArgumentError;
        free_task(task);
    end

    it "should fail for NULL xml buffers"
        DriverState state;
        PayloadSize hsize;
        InputSpec hspec;
        XslTask *task = ALLOC(sizeof(XslTask));
        state = init_task(task,
                        (const PayloadSize* const)&hsize,
                        (const InputSpec* const)&hspec,
                        NULL, NULL);
        state should be BadArgumentError;
        free_task(task);
    end

    it "should fail for NULL xsl buffers"
        DriverState state;
        PayloadSize hsize;
        InputSpec hspec;
        XslTask *task = ALLOC(sizeof(XslTask));
        create_test_data(test_xml, input_doc);

        state = init_task(task,
                        (const PayloadSize* const)&hsize,
                        (const InputSpec* const)&hspec,
                        test_xml, NULL);
        state should be BadArgumentError;
        free_task(task);
    end

    it "should fail for empty input sizes"
        DriverState state;
        PayloadSize hsize;
        InputSpec hspec;
        XslTask *task = ALLOC(sizeof(XslTask));
        create_test_data(test_xml, input_doc);
        create_test_data(test_xsl, xsl_doc);

        hsize.input_size = 0;

        state = init_task(task,
                        (const PayloadSize* const)&hsize,
                        (const InputSpec* const)&hspec,
                        test_xml, test_xsl);
        state should be EmptyBufferError;
        free_task(task);
    end

    it "should fail for empty input sizes"
        DriverState state;
        PayloadSize hsize;
        InputSpec hspec;
        XslTask *task = ALLOC(sizeof(XslTask));
        create_test_data(test_xml, input_doc);
        create_test_data(test_xsl, xsl_doc);

        hsize.input_size = 0;

        state = init_task(task,
                        (const PayloadSize* const)&hsize,
                        (const InputSpec* const)&hspec,
                        test_xml, test_xsl);
        state should be EmptyBufferError;
        free_task(task);
    end

    it "should fail for empty xslt sizes"
        DriverState state;
        PayloadSize hsize;
        InputSpec hspec;
        XslTask *task = ALLOC(sizeof(XslTask));
        create_test_data(test_xml, input_doc);
        create_test_data(test_xsl, xsl_doc);

        hsize.input_size = strlen(test_xml);
        hsize.xsl_size = 0;

        state = init_task(task,
                        (const PayloadSize* const)&hsize,
                        (const InputSpec* const)&hspec,
                        test_xml, test_xsl);
        state should be EmptyBufferError;
        free_task(task);
    end

    it "should (fail) assertion for non-matching [input] buffer vs. header size"
        DriverState state;
        PayloadSize hsize;
        InputSpec hspec;
        XslTask *task = ALLOC(sizeof(XslTask));
        create_test_data(test_xml, input_doc);
        create_test_data(test_xsl, xsl_doc);

        hsize.input_size = 2;
        hsize.xsl_size = strlen(test_xsl);

        state = init_task(task,
                        (const PayloadSize* const)&hsize,
                        (const InputSpec* const)&hspec,
                        test_xml, test_xsl);

        state should be Success;
        char *s = assert_failed;

	s should have_failed_due_to "hsize->input_size == strlen(xml)";

        free_task(task);
    end

    it "should (fail) assertion for non-matching [xsl] buffer vs. header size"
        DriverState state;
        PayloadSize hsize;
        InputSpec hspec;
        XslTask *task = ALLOC(sizeof(XslTask));
        create_test_data(test_xml, input_doc);
        create_test_data(test_xsl, xsl_doc);

        hsize.input_size = strlen(test_xml);
        hsize.xsl_size = 3;

        state = init_task(task,
                        (const PayloadSize* const)&hsize,
                        (const InputSpec* const)&hspec,
                        test_xml, test_xsl);

        state should be Success;

        assert_failed should have_failed_due_to "hsize->xsl_size == strlen(xsl)";

        free_task(task);
    end

    it "should fail when no memory is available to allocate InputDocument(s)"
        PayloadSize hsize;
        InputSpec hspec;

        create_test_data(test_xml, input_doc);
        create_test_data(test_xsl, xsl_doc);
        setup_size_headers(hsize, test_xml, test_xsl);
        setup_spec_headers(hspec, Buffer, Buffer, 0);

        XslTask *task = ALLOC(sizeof(XslTask));
        DriverState state;
        do_with_nomem(
            state = init_task(task,
                (const PayloadSize* const)&hsize,
                (const InputSpec* const)&hspec,
                test_xml, test_xsl));

        state should be OutOfMemory;
        free_task(task);
    end

    it "should set the input type to text and size the DriverIOVec appropriately"
        PayloadSize hsize;
        InputSpec hspec;

        create_test_data(test_xml, input_doc);
        create_test_data(test_xsl, xsl_doc);
        setup_size_headers(hsize, test_xml, test_xsl);
        setup_spec_headers(hspec, Buffer, Buffer, 0);

        XslTask *task = ALLOC(sizeof(XslTask));
        DriverState state = init_task(task,
            (const PayloadSize* const)&hsize,
            (const InputSpec* const)&hspec,
            test_xml, test_xsl);

        task should not be NULL;
        task->parameters should be NULL;

        InputDocument *input_doc = task->input_doc;
        InputDocument *xslt_doc = task->xslt_doc;
        char *buff_xml = get_doc_buffer(input_doc);
        char *buff_xsl = get_doc_buffer(xslt_doc);

        state should be Success;
        input_doc->type should be Buffer;
        xslt_doc->type should be Buffer;
        buff_xml should point_to test_xml;
        buff_xsl should point_to test_xsl;

        free_task(task);
    end
end

describe "Initializing Command Structures"

    it "should return NULL when Command allocation fails"
        Command *cmd;
        do_with_nomem(
            cmd = init_command(command_string, NULL, NULL, NULL));
        cmd should be NULL;
    end

    it "should return NULL when DriverIOVec allocation fails"
        Command *cmd;
        with_denied_allocation(sizeof(DriverIOVec),
            cmd = init_command(command_string, NULL, NULL, NULL));
        cmd should be NULL;
    end

    it "should assign the XslTask if provided"
        create_test_data(test_data, command_string);
        DriverContext *ctx = ALLOC(sizeof(DriverContext));
        ctx->port = NULL;
        XslTask *task = ALLOC(sizeof(XslTask));
        clear_task_fields(task);
        Command *cmd;
        cmd = init_command(test_data, ctx, task, NULL);

        XslTask *ptask = cmd->command_data.xsl_task;

        ptask should point_to task;
        cmd->command_string should equal test_data;
        cmd->context should point_to ctx;

        free_command(cmd);
        DRV_FREE(ctx);
    end

    it "should assign the DriverIOVec if provided"
        create_test_data(test_data, internal_command_string);
        DriverContext *ctx = ALLOC(sizeof(DriverContext));
        ctx->port = NULL;
        DriverIOVec *iov = init_iov(Text, 0, NULL);
        Command *cmd;

        cmd = init_command(test_data, ctx, NULL, iov);

        DriverIOVec *piov = cmd->command_data.iov;

        cmd->command_string should equal test_data;
        cmd->context should point_to ctx;
        piov should point_to iov;

        free_command(cmd);
        DRV_FREE(ctx);
end

end
