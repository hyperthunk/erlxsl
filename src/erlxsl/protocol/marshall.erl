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
%% @private
%% @see erlxsl
%% @doc  erlxsl driver protocol
%% <p>
%% This module exposes a set of functions that can create a data structure representing
%% the xml input data (or file uri), xslt stylesheet data (or file) and (optionally) any
%% parameters being passed, in a format understood by the linked-in driver.
%% </p>
%%

-module(erlxsl.protocol.marshall).

%% compiler flags
-compile(export_all).

%%
%% @doc Builds a request packet for communicating with the underlying driver.
%%
build_request(InputDataBinary, XslDataBinary) ->
	build_request(InputDataBinary, XslDataBinary, []).

%%
%% @doc Builds a request packet for communicating with the underlying driver.
%%
build_request(InputDataBinary, XslDataBinary, ParameterList) ->
    build_request_internal(_InputType=1, InputDataBinary, _XslType=1, XslDataBinary, ParameterList).

%%
%% populates the header fields and pattern matches on the params...
%% @private
%%
build_request_internal(
	InputType,
	Input,
	XslType,
	Xsl,
	Parameters
) when is_binary(Input) andalso is_binary(Xsl) ->
	Headers = [ InputType, XslType, length(Parameters) ],
    DataSizeMarkers = [ size(Input), size(Xsl) ],
	build_request_internal([Headers|DataSizeMarkers], Input, Xsl, Parameters).

%%
%% @doc Builds a request packet for communicating with the underlying driver.
%%
build_request_internal(
	[Headers|DataSizeMarkers],
	Input,
	Xsl,
	[]
) when is_binary(Input) andalso is_binary(Xsl) ->
	_Request = [
		[ <<Header:16/native-integer>> || Header <- Headers ],
		[ <<Marker:32/native-integer>> || Marker <- DataSizeMarkers ],
		[ Input, Xsl ]
	];

%%
%% @doc Builds a request packet for communicating with the underlying driver.
%%
build_request_internal(
	[Headers|DataSizeMarkers],
	Input,
	Xsl,
	ParameterList=[_H|_T]
) when is_binary(Input) andalso is_binary(Xsl) ->
	PackagedParameters = package_parameters(ParameterList),
	_Request = [
		[ <<Header:16/native-integer>> || Header <- Headers ],
		[ <<Marker:32/native-integer>> || Marker <- DataSizeMarkers ],
		PackagedParameters,
		[ Input, Xsl ]
	].

%%
%% Constructs a nested structure (list) of binaries,
%% containing the appropriate offset markers (for parameter names
%% and values) followed by the parameter data itself (names followed
%% by values). This structure is added to the other arguments, to form
%% a package [ HEADERS | ( [ ParamHeaders | ParamData ] ), Input, Xsl ]
%%
%% TODO: @spec the argument types out (or use Yariv's parse transform!)
%%
package_parameters([])
    -> [];
package_parameters(ParameterList) ->
    %% YUK -> recursive fun syntax is v. off; is there a better way!?
    Conversion =
    fun ({Element, _Convertor}, Acc)
            when is_list(Element) ->
                Bin = list_to_binary(Element), {
                    %% returns a 2tuple, with each element
                    %% ultimately heading for it's own output list
                    <<(size(Bin)):16/native-integer>>, [Bin|Acc]
                };
        ({Element, Convertor}, Acc)
            when is_integer(Element) ->
                Convertor({integer_to_list(Element), Convertor}, Acc);
        ({Element, Convertor}, Acc)
            when is_float(Element) ->
				Convertor({float_to_list(Element), Convertor}, Acc);
        (_, _Acc)
            -> throw({ebadarg, "Parameter List Structure Mismatch!"})
                %%TODO: wth is the right error code/atom to use for this!?
    end,
    {ParamHeaders, ParamData} = lists:mapfoldl(
        Conversion,
        _Accumulator = [],
        [ {Item, Conversion} || Item <- lists:foldl(
                fun({ParamName, ParamValue}, Acc) ->
                    [ParamValue, ParamName | Acc]
                end,
                _Acc=[],
                ParameterList
            )
        ]
    ),
    [lists:reverse(ParamHeaders), ParamData].
