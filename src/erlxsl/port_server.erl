%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(erlxsl.port_server).
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
        {ok, Port} -> {ok, [Port|Config]}
        ;
        Other      -> Other
    end,
    process_flag(trap_exit, Flag),
    Data.
    
%% private api
port_init({error, Error}, _) ->
    {stop, {Error, .erl_ddll:format_error(Error)}}
;
port_init(ok, Driver) ->
    Self = self(),
    Pid = spawn_link(
        fun() ->
            Port = open_port({spawn, Driver}, []),
            Self ! {self(), {port, Port}}
        end
    ),
    receive
        {Pid, {port,_}=PSpec} -> {ok, PSpec}
        ;
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
