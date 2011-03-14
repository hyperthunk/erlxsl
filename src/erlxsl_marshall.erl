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
%% @doc Internal Module: handles packing/unpacking the driver protocol.
%%
%% -----------------------------------------------------------------------------

-module(erlxsl_marshall).

-include("erlxsl.hrl").

%% Public API Exports
-export([pack/4, pack/5]).

%% FIXME: tighten up spec for /headers to specify the allowed range of atoms

%% @doc Packs the input and xsl type metadata and instance
%% data into an iolist, for submission to a linked-in port driver.
-spec(pack(InputType::atom(), XslType::atom(), Input::binary(), Xsl::binary()) -> iolist()).
pack(InputType, XslType, Input, Xsl)
when is_binary(Input) andalso is_binary(Xsl) ->
  pack(InputType, XslType, Input, Xsl, []).

%% @doc Packs the input and xsl type metadata, instance data and the
%% supplied proplist of parameters into an iolist, for submission
%% to a linked-in port driver.
-spec(pack(InputType::atom(), XslType::atom(),
           Input::binary(), Xsl::binary(), [{binary(), binary()}]) -> iolist()).
pack(InputType, XslType, Input, Xsl, []) ->
  [[pack(InputType), pack(XslType)], [Input, Xsl]];
pack(InputType, XslType, Input, Xsl, Parameters) ->
  [[pack(InputType), pack(XslType)],
   [Input, Xsl],
   [[Name, Value] || {Name, Value} <- Parameters]].

pack(?BUFFER_INPUT) -> 0;
pack(?FILE_INPUT) -> 1.
