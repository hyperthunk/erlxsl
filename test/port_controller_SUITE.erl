%
% Copyright (c) Tim Watson, 2008 - 2010
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without modification,
% are permitted provided that the following conditions are met:
%
%     * Redistributions of source code must retain the above copyright notice,
%       this list of conditions and the following disclaimer.
%
%     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions
%        and the following disclaimer in the documentation and/or other materials provided with the distribution.
%
%     * Neither the name of the author nor the names of any contributors may be used to endorse or
%        promote products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
% IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
% @author Tim Watson [http://hyperthunk.wordpress.com]
% @copyright (c) Tim Watson, 2008
% @since: 29 Feb 2008
% @version 0.3.0
% @hidden
% @doc  erlxsl port controller tests
%

-module(port_controller_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("../include/erlxsl.hrl").
-include("../include/test.hrl").

% public api exports

% automatically registers all exported functions as test cases
all() ->
   ?EXPORT_TESTS(?MODULE).

init_per_suite(C) ->
  BaseDir  = filename:rootname(filename:dirname(filename:absname(code:which(?MODULE))), "test"),
  PrivDir = filename:join(filename:join(BaseDir, "priv"), "bin"),
  % ?config(priv_dir, C),
  AppSpec = ct:get_config(test_app_config),
  {application, erlxsl, Conf} = AppSpec,
  {env, Env} = lists:keyfind(env, 1, Conf),
  {driver_options, Opts} = lists:keyfind(driver_options, 1, Env),
  UpdatedOpts =
  case erlxsl_util:os_platform() of
    %% FIXME: horrible - do this some other way (like switching on win32 and defaulting for *nix)
    Atom when Atom =:= darwin orelse Atom =:= linux ->
      lists:keyreplace(load_path, 1, Opts, {load_path, PrivDir});
    _ -> Opts
  end,
  Lib = proplists:get_value(engine, UpdatedOpts),
  ct:pal("startup with lib ~p~n", [Lib]),
  UpdatedOpts2 = lists:keyreplace(engine, 1, UpdatedOpts,
    {engine, filename:join(filename:join(BaseDir, "priv/test/bin"), Lib)}),
  UpdatedEnv = lists:keyreplace(driver_options, 1, Env, {driver_options, UpdatedOpts2}),
  UpdatedConf = lists:keyreplace(env, 1, Conf, {env, UpdatedEnv}),
  TestAppSpec = {application, erlxsl, UpdatedConf},
  application:load(TestAppSpec),
  erlxsl_app:start(),
  C.

end_per_suite(_) ->
  erlxsl_app:stop().

init_per_testcase(_, Config) ->
  DataDir = ?config(data_dir, Config),
  Fixtures = test_support:get_fixture_dir(DataDir),
  [{fixtures, Fixtures}|Config].

end_per_testcase(_, _) ->
  ok.

basic_transform(Config) ->
  {ok, Foo} = file:read_file(?fixture(Config, "foo.xml")),
  X = erlxsl_port_controller:transform(Foo, <<"<output name='foo' age='21'/>">>),
  ExpectedResult = binary_to_list(Foo) ++ binary_to_list(<<"<output name='foo' age='21'/>">>),
  ?assertThat(binary_to_list(X), equal_to(ExpectedResult)).

basic_transform_two_large_docs(Config) ->
  %% really just demonstrates handling a *reasonable* amount of data (in + out)
  %% TODO: proper test case around *very large* inputs/output
  %% TODO: streaming api support?
  {ok, Xml} = file:read_file(?fixture(Config, "to_xml.xsl")),
  X = erlxsl_port_controller:transform(Xml, Xml),
  ?assertThat(byte_size(X), equal_to(byte_size(Xml) * 2)).

bad_args_transform(_) ->
  X = erlxsl_port_controller:transform(bad, args),
  ?assertMatch({error, {function_clause,
               [{erlxsl_marshall,pack,[_, buffer, buffer, bad, args]},
                {erlxsl_port_controller, _, _}]}}, X).

transform_small_binaries(_) ->
  Foo = <<"<input />">>,
  X = erlxsl_port_controller:transform(Foo, <<"<output />">>),
  ?assertThat(binary_to_list(X), equal_to("<input /><output />")).
