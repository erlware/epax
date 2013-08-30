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
-include("epax.hrl").
-export([main/1]).


%%============================================================================
%% API
%%============================================================================

%% main/1
%% ====================================================================
main([]) ->
    print_epax_help();
main(["init"]) ->
    epax_app:init();
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
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, _NonOptArgs}} ->
            handle_options(Options);
        {error, {Reason, Data}} ->
            io:format("** error: ~s ~p~n~n", [Reason, Data]),
            print_epax_help()
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

option_spec_list() ->
    [
     %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
     {help,        $h,        "help",        undefined,             "Show the program options"}
    ].

handle_options([{help, true}]) ->
    print_epax_help();
handle_options(Args) ->
    io:format("** invalid options: ~p~n~n", [string:join(Args, " ")]),
    print_epax_help().

print_epax_help() ->
    Help_Message = <<"epax (erlang package manager) version 0.0.0
Usage: epax command [options]

Commands:
    init                Initialize the index, deletes old index or packages if any
    add    <repo_link>  Add new package into index (repo must follow OTP structure)
    list                List down all packages in the index in lexicographical order
    remove <appname>    Remove the package from index
    update              Update information about all packages added into the index
    check               Trie to fix broken packages if any, updates the index too
    bundle <appname>    Figure out the dependencies for the application and copies all valid packages into deps folder

Options:
    -h, --help          Show the commands and options (this message)
">>,
    io:put_chars(Help_Message).
