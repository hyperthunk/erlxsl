%% -----------------------------------------------------------------------------
%%
%% ErlXSL: Marshall
%%
%% Copyright (c) 2008-2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%%
%% Handles packing/unpacking the driver protocol.
%%
%% -----------------------------------------------------------------------------

%% module annotations
-module(erlxsl_marshall).

-define(FILE_INPUT, 1).
-define(BUFFER_INPUT, 2).
-define(STREAM_INPUT, 3).

%% Public API Exports
-export([pack/2]).

pack(Input, Stylesheet) when is_binary(Input) andalso is_binary(Stylesheet) ->
	XmlKind = <<?BUFFER_INPUT:32>>,
	XslKind = <<?BUFFER_INPUT:32>>,
	InputLen = size(Input),
	XslLen = size(Stylesheet),
	[XmlKind, XslKind, <<InputLen:32>>, <<XslLen:32>>, <<0:32>>, Input, Stylesheet].
