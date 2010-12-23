%%
%% Copyright (c) Tim Watson, 2008 - 2010
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
%% @doc  erlxsl port server tests
%%

-module(port_server_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/test.hrl").

-define(SLAVE, esmt_test_slave).
-define(PORT_HINT, 10100).

%% public api exports

%% automatically registers all exported functions as test cases
all() ->
    ?EXPORT_TESTS(?MODULE).

driver_startup(_) ->
	ok.




