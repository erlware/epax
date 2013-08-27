%% -*- erlang-indent-level: 4;
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%% --------------------------------------------------------------------------
%%% @author Aman Mangal <mangalaman93@gmail.com>
%%% @copyright (C) 2012 Erlware, LLC.
%%% @doc main epax module
-module(epax).
-export([main/1]).


%%============================================================================
%% API
%%============================================================================

%% main/1
%% ====================================================================
main(["install"]) ->
    epax_app:install();
main(["add"|[Link]]) ->
    epax_app:add_app(Link);
main(["list"]) ->
    epax_app:list_apps();
main(["remove"|[Appname]]) ->
    epax_app:remove_app(list_to_atom(Appname));
main(["update"]) ->
    epax_app:update();
main(["check"]) ->
    epax_app:check();
main(["bundle"|[Appname]]) ->
    epax_app:bundle(list_to_atom(Appname));
main(Args) ->
    io:format("** invalid subcommand: ~p~n", [string:join(Args, " ")]).
