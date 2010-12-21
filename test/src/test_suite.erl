%%%
%%% Copyright (c) Tim Watson, 2008
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%%
%%%     * Redistributions of source code must retain the above copyright notice,
%%%       this list of conditions and the following disclaimer.
%%%
%%%     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions
%%% 	     and the following disclaimer in the documentation and/or other materials provided with the distribution.
%%%
%%%     * Neither the name of the author nor the names of any contributors may be used to endorse or
%%% 	     promote products derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%%% IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%
%%% @file test_suite.erl
%%% @copyright Tim Watson 2008
%%% @author Tim Watson [http://tim-watson.blogspot.com]
%%% @version 0.2.0
%%% @doc  erlxsl test suite runner
%%% @end
%%%

-module(test_suite).
%% -compile(export_all).
-export([test/0]).

%% local includes
-include("include/eunit_ext.hrl").

test() ->
	{ok, Pwd} = file:get_cwd(),
	io:format("Running test suite in ~p.~n", [Pwd]),
	Results = lists:map(fun testModule/1, loadTestModules()),
    io:format("Results: [~p]~n", [Results]),
    {Passed, Failed} = lists:partition(
        fun(Element) ->
            case (Element) of
                {ok, _Module} -> true;
                error -> false
            end
        end,
        Results
    ),
    io:format("Done: ~p module passed, ~p module failed.~n", [length(Passed), length(Failed)]).

testModule({module, ModuleName}) ->
    Mod = list_to_atom("erlxsl.protocol." ++ ModuleName),
    io:format("Processing module: ~p~n", [Mod]),
    Res = Mod:test(),
    %%io:format("Got result: ~p~n", [Res]),
    case (Res) of
        error -> Result = Res;
        _     -> Result = {ok, ModuleName}
    end,
	receive
	after 1000
        -> Result
	end;

testModule(Unknown) ->
       io:format("Test suite failed to load module(s) correctly:~p~n" , [Unknown]),
       error.

loadTestModules() ->
    Modules = filelib:fold_files(
        "test/ebin/",
        "(.*)(_tests.beam)",
        true,
        fun(F, _Acc) ->
            ModuleName = filename:basename(F, ".beam"),
            {module, ModuleName}
        end,
        []
    ),
    case (is_list(Modules)) of
        true -> Modules;
        false -> [Modules]
    end.
