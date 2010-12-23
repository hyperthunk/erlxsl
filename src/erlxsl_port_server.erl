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

-behaviour(gen_server).

%% OTP Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API Exports
-export([start/0, start_link/0]).

-define(SERVER, ?MODULE).
-define(PORT_INIT, 9).		%% magic number indicating that the port should initialize itself 

%% public api
start() ->
	gen_server:start(?SERVER, [], []).

start_link() ->
	gen_server:start_link(?SERVER, [], []).

%% gen_server api
init([]) ->
	erlxsl_fast_log:info("initializing port_server...~n"),
	Config = application:get_all_env(erlxsl),
	%% io:format("APPCONFIG [~p]~n", [Config]),
  Options = proplists:get_value(driver_options, Config, [{driver, "default_provider"}]),
	case proplists:get_value(driver, Options) of 
		undefined -> {stop, {config_error, "No XSLT Engine Specified."}};
		Provider ->
			process_flag(trap_exit, true),
			init_driver(Config, Provider)
	end.

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({transform, Input, Stylesheet}, State) ->
	handle_transform(Input, Stylesheet, State),
	{noreply, State};
handle_cast(stop, State) ->
	{stop, shutdown, State};
handle_cast(_, State) ->
	{noreply, State}.

handle_info(Unknown, State) ->
	erlxsl_fast_log:info("Port server received unknown message ~p~n", [Unknown]),
	{noreply, State}.

terminate(Reason, State) ->
	erlxsl_fast_log:info("Terminating due to [~p]~n", [Reason]),
	Driver = proplists:get_value(driver, State, "erlxsl_drv"),
	catch erl_ddll:unload_driver(Driver),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% private api
    
handle_transform(Input, Stylesheet, State) ->
	Controller = self(),
	Port = proplists:get_value(port, State),
	spawn_link(
		fun() -> 
			port_command(Port, erlxsl_marshall:pack(Input, Stylesheet)),
			receive 
				Data -> Controller ! {port_message, Data}
			end
		end
	).

init_driver(Config, Provider) ->
	erl_ddll:start(),
	% load driver
	BaseDir = filename:rootname(filename:dirname(filename:absname(code:which(erlxsl_app))), "ebin"),
	PrivDir = filename:join(BaseDir, "priv"),
	BinPath = proplists:get_value(load_path, Config, PrivDir),
	Driver = proplists:get_value(driver, Config, "erlxsl_drv"),
	init_lib({erl_ddll:load_driver(BinPath, Driver), Driver}, Config, Provider).

init_lib({{error, Error}, _}, _, _) ->
  {stop, {Error, erl_ddll:format_error(Error)}};
init_lib({ok, Driver}, Config, Provider) ->
	Port = open_port({spawn, Driver}, [binary]),
	init_port([{port, Port}|Config], Provider).

init_port(Config, Provider) ->
	Port = proplists:get_value(port, Config),
	try (erlang:port_call(Port, 9, term_to_binary(Provider))) of
		configured -> {ok, Config};
		Other -> {stop, {unexpected_driver_state, Other}}
	catch 
		_:Badness -> 
			terminate(Badness, Config),
			{stop, Badness}
	end.
