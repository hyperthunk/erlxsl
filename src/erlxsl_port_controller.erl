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
-module(erlxsl_port_controller).
-author('Tim Watson <watson.timothy@gmail.com>').

-behaviour(gen_server).

-include("erlxsl.hrl").
-import(filename, [absname/1, dirname/1, rootname/2, join/2]).

%% OTP Exports
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Public API Exports
-export([start/0, start_link/0, start/1,
         start_link/1, stop/0, transform/2]).

-define(SERVER, ?MODULE).
-define(PORT_INIT, 9).    %% magic number indicating that the port should initialize itself
-record(state, {
  port          :: port(),
  logger        :: module(),
  engine        :: string(),
  driver        :: string(),
  load_path     :: string(),
  clients       :: [{pid(), pid()}]
}).

%% public api

start() ->
  start(application:get_all_env(erlxsl)).

%% @doc starts erlxsl_port_server with default options.
%% NB: this means that the XSLT provider will be a test stub!
start(Config) ->
  gen_server:start({local,?SERVER}, ?SERVER, Config, []).

start_link() ->
  start_link(application:get_all_env(erlxsl)).

%% @doc starts erlxsl_port_server with default options.
%% NB: this means that the XSLT provider will be a test stub!
start_link(Config) ->
  gen_server:start_link({local,?SERVER}, ?SERVER, Config, []).

stop() ->
  gen_server:cast(?SERVER, stop).

transform(Input, Xsl) ->
  processing = gen_server:call(?SERVER, {transform, Input, Xsl}),
  receive
    {_Ref, {result,_,Result}} ->
      Result;
    {data, Data} ->
      {error, Data};
    Other -> Other
  end.

%% gen_server api
init(Config) ->
  erlxsl_fast_log:info("initializing port_server with config [~p]~n",
                       [Config]),
  Options = proplists:get_value(driver_options, Config,
                                [{engine, "default_provider"}]),
  State = init_config([proplists:get_value(logger, Config)|Options]),
  case State#state.engine of
    undefined ->
      {stop, {config_error, "No XSLT Engine Specified."}};
    _ ->
      process_flag(trap_exit, true),
      init_driver(State)
  end.

handle_call({transform, Input, Stylesheet}, From,
            #state{ logger=Log, clients=CL }=State) ->
  WorkerPid = handle_transform(?BUFFER_INPUT, ?BUFFER_INPUT, Input,
                               Stylesheet, State, From),
  Log:debug("storing ~p for worker ~p~n", [From, WorkerPid]),
  NewState = State#state{ clients=[{WorkerPid, From}|CL] },
  start_worker(WorkerPid),
  {reply, processing, NewState};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, shutdown, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_info({'EXIT', _, normal}, State) ->
  %% worker has completed successfully
  {normal, State};
handle_info({'EXIT', Worker, Reason},
            #state{ logger=Log, clients=CL }=State) ->
  %% TODO: what does it mean if we have no client? Is this part
  %%       of the error kernel for this server?
  Client = proplists:get_value(Worker, CL),
  gen_server:reply(Client, {error, Reason}),
  {noreply, State}.

terminate(Reason, #state{ logger=Log, driver=Driver }) ->
  Unload = erl_ddll:unload_driver(Driver),
  Log:info("Terminating [~p] - driver unload [~p]~n", [Reason, Unload]).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% private api

start_worker(Pid) ->
  Pid ! start.

handle_transform(InType, XslType, Input, Stylesheet,
                 #state{ port=Port, logger=Log }, Client) ->
  %% TODO: don't let this potentially hang for ever:
  %%       (a) we might never receive a response, so use a (configurable?) timeout
  spawn_link(
    fun() ->
      %% TODO: find a neater way of doing this 'pause until ready' thing
      Log:debug("Waiting for start command..."),
      receive start ->
        port_command(Port, erlxsl_marshall:pack(InType, XslType,
                                                Input, Stylesheet)),
        receive
          Data -> gen_server:reply(Client, Data)
        end
      end
    end
  ).

init_config(Config) ->
  #state{
    logger=proplists:get_value(logger, Config, erlxsl_fast_log),
    engine=proplists:get_value(engine, Config, "default_provider"),
    driver=proplists:get_value(driver, Config, "erlxsl_drv"),
    load_path=proplists:get_value(load_path, Config, init_path())
  }.

init_path() ->
  BaseDir = rootname(dirname(absname(code:which(erlxsl_app))), "ebin"),
  join(BaseDir, "priv").

init_driver(#state{driver=Driver, load_path=BinPath}=State) ->
  erl_ddll:start(),
  {ok, Cwd} = file:get_cwd(),
  % load driver
  erlxsl_fast_log:debug("loading d:~p, p:~p env:~p~n",
    [Driver, BinPath, Cwd]),
  init_lib(erl_ddll:load_driver(BinPath, Driver), State).

init_lib({error, Error}, _) ->
  {stop, {Error, erl_ddll:format_error(Error)}};
init_lib(ok, #state{ driver=Driver }=State) ->
  erlxsl_fast_log:debug("opening port to ~p~n", [Driver]),
  Port = open_port({spawn, Driver}, [binary]),
  init_port(State#state{ port=Port }).

init_port(#state{ port=Port, engine=Engine }=State) when is_list(Engine) ->
  erlxsl_fast_log:debug("configuring driver with ~p~n", [Engine]),
  try (erlang:port_call(Port, 9, Engine)) of
    configured -> {ok, State};
    Other -> {stop, {unexpected_driver_state, Other}}
  catch
    _:Badness ->
      terminate(Badness, State),
      {stop, Badness}
  end.
