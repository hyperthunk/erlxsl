%% -----------------------------------------------------------------------------
%%
%% ErlXSL: Port Server
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
%% Port Driver Control Process
%%
%% -----------------------------------------------------------------------------

%% module annotations
-module(erlxsl_port_server).
-author('Tim Watson <watson.timothy@gmail.com>').

%% public api exports
-export([init/1]).

-import(ct).
-import(ets).
-import(proplists).

%% public api
init(Config) ->
    BinPath = proplists:get_value(load_path, Config, "./priv/bin"),
    Driver = proplists:get_value(driver, Config, "xsldrv"),
    Flag = process_flag(trap_exit, true),
    Data =
    case port_init(.erl_ddll:load_driver(BinPath, Driver), Driver) of
        {ok, Port} -> {ok, [Port|Config]};
        Other -> Other
    end,
    process_flag(trap_exit, Flag),
    Data.
    
%% private api
port_init({error, Error}, _) ->
    {stop, {Error, .erl_ddll:format_error(Error)}};
port_init(ok, Driver) ->
    Self = self(),
    Pid = spawn_link(
        fun() ->
            Port = open_port({spawn, Driver}, []),
            Self ! {self(), {port, Port}}
        end
    ),
    receive
        {Pid, {port,_}=PSpec} -> {ok, PSpec};
        {'EXIT', Pid, Reason} -> {stop, Reason}
    end.

%%port_init(ok, Driver) ->
%%    {ok, [{port, open_port({spawn, Driver}, [])}]}.

%%-import(code).
%%-import(ct).
%%-import(filename).
%%-import(proplists).
%%
%%%% public api
%%init(Config) ->
%%    BinPath = proplists:get_value(load_path, Config, filename:join(code:priv_dir(erlxsl), "bin")),
%%    Driver = proplists:get_value(driver, Config, "xsldrv"),
%%    Flag = process_flag(trap_exit, true),
%%    ct:pal("current trap_exit is set to ~p", [Flag]),
%%    InitStartData = port_init(.erl_ddll:load_driver(BinPath, Driver), Driver),
%%    process_flag(trap_exit, Flag).
%%
%%%% private api
%%port_init({error, Error}, _) ->
%%    {stop, {Error, .erl_ddll:format_error(Error)}}
%%;
%%%port_init(ok, Driver) ->
%%%    Self = self(),
%%%    Pid = start_link(
%%%        fun() ->
%%%            Port = open_port({spawn, Driver}, []),
%%%            Self ! {self(), {port, Port}}
%%%        end
%%%    ),
%%%    receive
%%%        {Pid, {port,_}=PSpec} -> {ok, [PSpec]}
%%%        ;
%%%        {'EXIT', Pid, Reason} -> {stop, Reason}
%%%    end.
