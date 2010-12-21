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
%% @author Tim Watson [http://hyperthunk.wordpress.com]
%% @copyright (c) Tim Watson, 2008
%% @since: 29 Feb 2008
%% @version 0.3.0
%% @hidden
%% @doc  erlxsl driver protocol unit tests
%%

-module(protocol_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-import(erlxsl).

all() -> 
	[
	building_request_should_create_nested_iolist,
	building_parameterized_request_should_create_nested_iolist,
	building_buffer_request_should_set_typehdr_1
	].
	

building_request_should_create_nested_iolist(_) ->
	Xml = <<"<fragment><empty /></fragment>">>,
	Xsl = <<"<?xml version='1.0'?>">>,
	[
		[_XmlTypeHdr, _XslTypeHdr, _ParamSizeHdr],
		[_XmlSizeHdr, _XslSizeHdr],
		[_XmlDataBinary, _XslDataBinary]
	] = erlxsl.protocol:build_request(Xml, Xsl).

building_parameterized_request_should_create_nested_iolist(_) ->
	Xml = <<"<fragment><empty /></fragment>">>,
	Xsl = <<"<?xml version='1.0'?>">>,
	Parameters = [ {"p1", "value1"}, {"p2", "value2"} ],
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
	] = erlxsl.protocol:build_request(Xml, Xsl, Parameters).

building_buffer_request_should_set_typehdr_1(_) ->
	Xml = <<"<fragment><empty /></fragment>">>,
	Xsl = <<"<?xml version='1.0'?><xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\"/>">>,
	ExpectedHeaderValue = <<1,0>>,
	[
		[XmlTypeHdr, XslTypeHdr, _],
		[_,_],
		[_,_]
	] = erlxsl.protocol:build_request(Xml, Xsl),
	ExpectedHeaderValue = XmlTypeHdr,
	ExpectedHeaderValue = XslTypeHdr.

