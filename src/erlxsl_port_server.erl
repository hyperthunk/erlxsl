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
-export([start/1, start_link/1]).

-define(SERVER, ?MODULE).
-define(PORT_INIT, 9).		%% magic number indicating that the port should initialize itself 

%% public api
start(Config) ->
	gen_server:start(?SERVER, [Config], []).

start_link(Config) ->
	gen_server:start_link(?SERVER, [Config], []).

%% gen_server api
init(Config) ->
  Options = proplists:get_value(driver_options, Config, []),
	case proplists:get_value(driver, Options, missing) of 
		missing -> {stop, {config_error, "No XSLT Engine Specified."}};
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
	catch erl_ddll:unload_driver(Driver).

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
	PrivDir = code:priv_dir(erlxsl),
	BinPath = proplists:get_value(load_path, Config, PrivDir),
	Driver = proplists:get_value(driver, Config, "erlxsl_drv"),
	init_lib({erl_ddll:load_driver(BinPath, Driver), Driver}, Config, Provider).

init_lib({{error, Error}, _}, _, _) ->
    {stop, {Error, erl_ddll:format_error(Error)}};
init_lib({ok, Driver}, Config, Provider) ->
	LoadTimeout = proplists:get_value(load_timeout, Config, 1000000),
  Self = self(),
  Pid = spawn_link(
  	fun() ->
    	Port = open_port({spawn, Driver}, [binary]),
      Self ! {self(), {port, Port}}
    end
  ),
  receive
  	{Pid, {port,_}=PSpec} -> 
			init_port([PSpec|Config], Provider);
    {'EXIT', Pid, Reason} -> 
			{stop, Reason}
	after 
		LoadTimeout -> {stop, load_timeout}
  end.

init_port(Config, Provider) ->
	Port = proplists:get_value(port, Config),
	try (erlang:port_call(Port, ?PORT_INIT, term_to_binary(Provider))) of
		Status ->
			case binary_to_term(Status) of
				configured -> {ok, Config};
				Other -> {stop, {unexpected_driver_state, Other}}
			end
	catch 
		_:Badness -> 
			terminate(Badness, Config),
			{stop, Badness}
	end.
