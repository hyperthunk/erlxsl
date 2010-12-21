%%
%% Copyright (c) Tim Watson, 2008
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without modification,
%% are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright notice,
%%       this list of conditions and the following disclaimer.
%%
%%     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions
%% 	     and the following disclaimer in the documentation and/or other materials provided with the distribution.
%%
%%     * Neither the name of the author nor the names of any contributors may be used to endorse or
%% 	     promote products derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% @author Tim Watson [http://tim-watson.blogspot.com]
%% @copyright (c) Tim Watson, 2008
%% @since: 29 Feb 2008
%% @version 0.2.0
%% @hidden
%% @doc  erlxsl driver protocol unit tests
%%

-module(erlxsl.protocol.marshall_tests).

%% compiler flags
-compile(export_all).

%% lib includes
-include_lib("eunit/include/eunit.hrl").

%% local includes
-include("include/eunit_ext.hrl").

building_request_creates_nested_iolist_test_() ->
	Xml = <<"<fragment><empty /></fragment>">>,
	Xsl = <<"<?xml version='1.0'?>">>,
	?_assertMatch(
		[
			[_XmlTypeHdr, _XslTypeHdr, _ParamSizeHdr],
			[_XmlSizeHdr, _XslSizeHdr],
			[_XmlDataBinary, _XslDataBinary]
		],
		erlxsl.protocol.marshall:build_request(Xml, Xsl)
	).

building_parameterized_request_creates_nested_iotlist_test_() ->
	Xml = <<"<fragment><empty /></fragment>">>,
	Xsl = <<"<?xml version='1.0'?>">>,
	Parameters = [ {"p1", "value1"}, {"p2", "value2"} ],
	?_assertMatch(
		[
			[_XmlTypeHdr, _XslTypeHdr, _ParamSizeHdr],
			[_XmlSizeHdr, _XslSizeHdr],
			[
				[
					_P1NameSizeHdr,
					_P1ValueSizeHdr,
					_P2NameSizeHdr,
					_P2ValueSizeHdr
				],
				[
					<<"p1">>,
					<<"value1">>,
					<<"p2">>,
					<<"value2">>
				]
			],
			%% _BinaryConvertedParameterList=[ H | T ],
			[_XmlDataBinary, _XslDataBinary]
		],
		erlxsl.protocol.marshall:build_request(Xml, Xsl, Parameters)
	).

building_buffer_request_sets_type_1_test_() ->
	Xml = <<"<fragment><empty /></fragment>">>,
	Xsl = <<"<?xml version='1.0'?><xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\"/>">>,
	ExpectedHeaderValue = <<1,0>>,
	[
		[XmlTypeHdr, XslTypeHdr, _],
		[_,_],
		[_,_]
	] = erlxsl.protocol.marshall:build_request(Xml, Xsl),
	_Tests = [
		?_assertEqual(ExpectedHeaderValue, XmlTypeHdr),
		%%TODO: for some reason, this next line generates a warning; But why!?
		?_assertEqual(ExpectedHeaderValue, XslTypeHdr)
	].

destroying_something_test_() ->
    _X = 2 + 2, ?fail().

%%driver_response_match() ->
%%	io:format("Running driver test(s) in ~p.~n", [file:get_cwd()]),
%%    Driver = "erlxsl",
%%    ok = erl_ddll:load_driver("./priv/bin", Driver),
%%    Port = open_port({spawn, Driver}, []),
%%    {ok, Input} = file:read_file("test/resources/test.xml"),
%%    {ok, Xsl} = file:read_file("test/resources/test.xsl"),
%%    %% Parameters = [{"p1", 1234}, {"p2", "joe.armstrong@ericcson.com"}, {"p3", 26.49875}],
%%    Parameters = [{"title", "This is my title"}],
%%    Request = erlxsl.protocol.marshall:build_request(Input, Xsl, Parameters),
%%    true = port_command(Port, Request),
%%    receive
%%		%%{response, Port, ResponseData} ->
%%		%%	io:format("Response size: ~p~n", [size(ResponseData)]);
%%		Response ->
%%			?debugMsg("Received response.~n"),
%%			?debugFmt("Response:~n~p~n", [Response]),
%%			?_assertMatch({response, _Port, _ResponseData}, Response)
%%    after 8000
%%        -> ?_assertFail("timeout (8000)...")
%%    end.
