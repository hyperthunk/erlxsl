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

-module(erlxsl_util).

-compile(export_all).

os_platform() ->
  match_platform(["darwin", "linux", "win32", "windows"]).

-spec(match_platform(list(string())) -> atom()).
match_platform([H|T]) ->
  case re:run(erlang:system_info(system_architecture), H, [{capture, none}]) of
    match -> list_to_atom(H);
    _ -> match_platform(T)
  end;
match_platform([]) -> unknown.

