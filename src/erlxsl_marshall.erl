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

%% @private
pack(InputType, XslType, Input, Xsl) 
when is_binary(Input) andalso is_binary(Xsl) ->
	pack(InputType, XslType, Input, Xsl, []).

%% @private
pack(InputType, XslType, Input, Xsl, []) 
when is_binary(InputType) andalso is_binary(XslType) ->
	DataSizeMarkers = [ size(Input), size(Xsl) ],
	[
		[ InputType, XslType, <<0:16/native-integer>> ],
		[ <<Marker:32/native-integer>> || Marker <- DataSizeMarkers ],
		[ Input, Xsl ]
	];
pack(InputType, XslType, Input, Xsl, Parameters) 
when is_binary(InputType) andalso is_binary(XslType) ->
	DataSizeMarkers = [ size(Input), size(Xsl) ],
	ParamLen = length(Parameters),
	[
		[ InputType, XslType, <<ParamLen:16/native-integer>> ],
		pack(Parameters),
		[ <<Marker:32/native-integer>> || Marker <- DataSizeMarkers ],
		[ Input, Xsl ]
	].
	

%%
%% Constructs a nested structure (list) of binaries,
%% containing the appropriate offset markers (for parameter names
%% and values) followed by the parameter data itself (names followed
%% by values). This structure is added to the other arguments, to form
%% a package [ HEADERS | ( [ ParamHeaders | ParamData ] ), Input, Xsl ]
%%
pack([])
    -> [];
pack(ParameterList) ->
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
