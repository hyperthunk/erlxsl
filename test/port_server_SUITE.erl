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
%% @doc  erlxsl port server unit and integration tests
%%

%% module annotations
-module(port_server_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').

%% compilation directives
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-import(ct).
-import(filename, [basename/1, join/2]).
-import(erlxsl.port_server).

-define(UNPACK_SVR_CONF, [
        {load_path, join(code:priv_dir(erlxsl), "bin")},
        {driver, "xsldrv"}
    ]
).

-define(FAIL(Msg, Args), ct:pal(Msg, Args), ct:fail()).

all() -> [
    initializing_native_driver,
    server_init_returns_port,
    server_init_stores_options,
    bad_server_init_returns_stop
    ].

initializing_native_driver(_) ->
    ServerConf = ?UNPACK_SVR_CONF,
    ?assertMatch({ok,_}, erlxsl.port_server:init(ServerConf)).

server_init_stores_options(_) ->
    InitialConfig = ?UNPACK_SVR_CONF,
    ServerConf = InitialConfig,
    {ok, State} = erlxsl.port_server:init(ServerConf),
    lists:map(
        fun({K,V}) ->
            case proplists:get_value(K,InitialConfig) of
                undefined -> ok ;
                V -> ok ;
                _ -> ?FAIL("expected server to retain initial config "
                    ++ "value of '~p' as '~p'", [K, V])
            end
        end,
        State
    ).

server_init_returns_port(_) ->
    ServerConf = ?UNPACK_SVR_CONF,
    {ok, State} = erlxsl.port_server:init(ServerConf),
    ?assert(is_port(proplists:get_value(port, State))).

bad_server_init_returns_stop(Config) ->
    ServerConf = [
        {load_path, ?config(data_dir, Config)},
        {driver, "xsldrv"}
    ],
    ct:pal("causing deliberate error in working process: don't be alarmed by any noise on stdout....."),
    {stop, {einval, _}} = erlxsl.port_server:init(ServerConf).

%%Port = open_port({spawn, Driver}, [])

%% TODO: write a provider implementation that causes open_port to fail and test it
