%% -----------------------------------------------------------------------------
%%
%% ErlXSL: Fire and Forget Console Logging
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
%% Fast fire & forget logging.
%%
%% -----------------------------------------------------------------------------

-module(erlxsl_fast_log).

-behaviour(gen_server).

-spec(start_link/0  :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).
-spec(info/1        :: (string()) -> 'ok').
-spec(info/2        :: (string(), [any()]) -> 'ok').
-spec(warn/1        :: (string()) -> 'ok').
-spec(warn/2        :: (string(), [any()]) -> 'ok').
-spec(error/1       :: (string()) -> 'ok').
-spec(error/2       :: (string(), [any()]) -> 'ok').

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-export([start/0
				,start_link/0
        ,info/1
        ,info/2
        ,warn/1
        ,warn/2
        ,error/1
        ,error/2]).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

info(Format) ->
  gen_server:cast(?MODULE, {info, Format}).

info(Format, Args) when is_list(Args) ->
  gen_server:cast(?MODULE, {info, Format, Args}).

warn(Format) ->
  gen_server:cast(?MODULE, {warn, Format}).

warn(Format, Args) when is_list(Args) ->
  gen_server:cast(?MODULE, {warn, Format, Args}).

error(Format) ->
  gen_server:cast(?MODULE, {error, Format}).

error(Format, Args) when is_list(Args) ->
  gen_server:cast(?MODULE, {error, Format, Args}).

%%--------------------------------------------------------------------

init([]) ->
	{ok, none}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({info, Format}, State) ->
  error_logger:info_msg(Format, []),
  {noreply, State};
handle_cast({info, Format, Args}, State) ->
  error_logger:info_msg(Format, Args),
  {noreply, State};
handle_cast({warn, Format}, State) ->
  error_logger:warning_msg(Format, []),
  {noreply, State};
handle_cast({warn, Format, Args}, State) ->
  error_logger:warning_msg(Format, Args),
  {noreply, State};
handle_cast({error, Format}, State) ->
  error_logger:error_msg(Format, []),
  {noreply, State};
handle_cast({error, Format, Args}, State) ->
  error_logger:error_msg(Format, Args),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
