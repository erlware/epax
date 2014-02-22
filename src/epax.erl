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
%%%

-module(epax).
-include("epax.hrl").
-export([main/1]).


%%============================================================================
%% API
%%============================================================================

%% main/1
%% ====================================================================
main([]) ->
    print_help();
main(["init"]) ->
    epax_app:init();
main(["add"|Args]) ->
    OptSpecList = option_spec_list_for_add(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {[help], []}} ->
            print_help_for_add();
        {ok, {_, []}} ->
            epax_com:console("** Invalid command (repo_link not found).~n~n", []),
            print_help_for_add();
        {ok, {Options, [Link]}} ->
            epax_app:add_app(Link, Options);
        {ok, {_, NonOptArgs}} ->
            epax_com:console("** Invalid non option arguments: ~p.~n~n", [NonOptArgs]),
            print_help_for_add();
        {error, {Reason, Data}} ->
            epax_com:console("** Error: ~s ~p.~n", [Reason, Data]),
            print_help_for_add()
    end;
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
main(["show"|[Appname]]) ->
    epax_app:show(list_to_atom(Appname));
main(["search"|Args]) ->
    OptSpecList = option_spec_list_for_search(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {[help], []}} ->
            print_help_for_search();
        {ok, {_, []}} ->
            epax_com:console("** Invalid command (regex not found).~n~n", []),
            print_help_for_search();
        {ok, {Options, [Regex]}} ->
            epax_index:search(Regex, Options);
        {ok, {_, NonOptArgs}} ->
            epax_com:console("** Invalid non option arguments: ~p.~n~n", [NonOptArgs]),
            print_help_for_search();
        {error, {Reason, Data}} ->
            epax_com:console("** Error: ~s ~p.~n", [Reason, Data]),
            print_help_for_search()
    end;
main(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, []}} ->
            handle_options(Options);
        {ok, {_, NonOptArgs}} ->
            epax_com:console("** Invalid non option arguments: ~p.~n~n", [NonOptArgs]),
            print_help();
        {error, {Reason, Data}} ->
            epax_com:console("** Error: ~s ~p.~n", [Reason, Data]),
            print_help()
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

% main options
option_spec_list() ->
    [
     %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
     {help,        $h,        "help",        undefined,             "Show the program options"},
     {version,     $v,        "version",     undefined,             "Show the current version"}
    ].

handle_options([help]) ->
    print_help();
handle_options([version]) ->
    print_version();
handle_options(Options) ->
    epax_com:console("** Invalid options: ~p.~n", [Options]),
    print_help().

print_help() ->
    HelpMessage = << <<?EPAX>>/binary, <<" (Erlang Package Manager) version ">>/binary, <<?VERSION>>/binary, <<"
Usage: ">>/binary, <<?EPAX>>/binary, <<" command [options]

Commands:
  init                Initialize the index, deletes old index or packages if any
  add    <repo_link>  Add new package into index (repo must follow OTP structure)
  list                List down all packages in the index in lexicographical order
  remove <appname>    Remove the package from index
  update              Update details of all packages in the index
  check               Try to fix broken packages if any, updates the index as well
  bundle <appname>    Compute and copy non-standard dependencies for the pacakge
  show   <appname>    Print detailed information of the package
  search <regex>      Performs full text search on available package lists

Options:
  -h, --help          Show the commands and options (this message)
  -v, --version       Show the current version

">>/binary>>,
    io:put_chars(HelpMessage).

print_version() ->
    VersionMessage = << <<?EPAX>>/binary, <<" (Erlang Package Manager) version ">>/binary, <<?VERSION>>/binary, <<"
">>/binary >>,
    io:put_chars(VersionMessage).

% help subcommand add
option_spec_list_for_add() ->
    [
     %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
     {help,        $h,        "help",        undefined,             "Show the program options"},
     {repo_type,   $r,        "repo",        string,                "Specify type of repository (git, bzr, svn)"}
    ].

print_help_for_add() ->
    getopt:usage(option_spec_list_for_add(), ?EPAX).

% help subcommand add
option_spec_list_for_search() ->
    [
     %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
     {help,        $h,        "help",        undefined,             "Show the program options"},
     {names_only,  $n,        "names-only",  boolean,               "Search only package names, not decription"},
     {full,        $f,        "full",        boolean,               "Show output identical to `show` is produced for each matched package"}
    ].

print_help_for_search() ->
    getopt:usage(option_spec_list_for_search(), ?EPAX).
