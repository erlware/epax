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
%%% @doc main epax application module
%%%

-module(epax_app).
-include("epax.hrl").
-export([init/0,
         add_app/2,
         remove_app/1,
         list_apps/0,
         update/0,
         check/0,
         bundle/1,
         show/1,
         format_app/1]).


%%============================================================================
%% API
%%============================================================================

%% init/0
%% ====================================================================
%% @doc initializes index
-spec init() -> ok.
%% ====================================================================
init() ->
    EpaxLoc = epax_os:get_abs_path(""),
    epax_os:mkdir(EpaxLoc),
    epax_index:init(),
    epax_com:success("Index initialized successfully").

%% add_app/2
%% ====================================================================
%% @doc adds OTP application (package) stored at Link (only supports
%% git, bzr and svn repositories)
-spec add_app(Link, Options) -> ok when
    Link    :: string(),
    Options :: [term()].
%% ====================================================================
add_app(Link, Options) ->
    case epax_index:app_exists(Link) of
        {ok, false} ->
            case epax_index:checkout_repo_and_add_to_index(Link, Options) of
                {ok, Appname} ->
                    epax_com:success("Added ~s to index", [Appname]);
                {error, Reason} ->
                    epax_com:error(Reason, "Unable to add to index")
            end;
        {ok, Appname} ->
            epax_com:error(already_added, "~s is already added", [Appname]);
        {error, Reason} ->
             epax_com:error(Reason, "Unable to add to index")
    end.

%% remove_app/1
%% ====================================================================
%% @doc removes OTP application from index
-spec remove_app(Appname) -> ok when
    Appname :: atom().
%% ====================================================================
remove_app(Appname) ->
    case epax_index:remove_from_index(Appname) of
        ok ->
            epax_com:success("~s is removed successfully", [Appname]);
        {error, Reason} ->
            epax_com:error(Reason, "~s cannot be removed from index", [Appname])
    end.

%% list_apps/0
%% ====================================================================
%% @doc prints the app list stored in the index (in lexicographical order)
-spec list_apps() -> ok.
%% ====================================================================
list_apps() ->
    case epax_index:get_applist() of
        {ok, []} ->
            epax_com:success("No app is added yet");
        {ok, AllApps} ->
            epax_com:success("=== Erlang Apps ===~s~n===================",
                [lists:foldl(fun(Elem, Acc) ->
                        string:concat(Acc, io_lib:format("~n  - ~s", [Elem]))
                    end,
                    "",
                    AllApps)]);
        {error, Reason} ->
            epax_com:error(Reason, "Cannot retrieve the application list")
    end.

%% update/0
%% ====================================================================
%% @doc updates the index
-spec update() -> ok.
%% ====================================================================
update() ->
    case epax_index:update_index() of
        ok ->
            epax_com:success("Index updated successfully");
        {error, Reason} ->
            epax_com:error(Reason, "Unable to update index")
    end.

%% check/0
%% ====================================================================
%% @doc verifies that there are no broken packages (index description
%% matches with the downloaded package), updates the index simultaneously
-spec check() -> ok.
%% ====================================================================
check() ->
    case epax_index:check_index() of
        ok ->
            epax_com:success("Fixed broken packages");
        {error, Reason} ->
            epax_com:error(Reason, "Unable to fix broken packages, reinitialized the index")
    end.

%% bundle/1
%% ====================================================================
%% @doc bundles the app, finds all the dependencies and copies them into the
%% deps folder inside the app folder
-spec bundle(Appname) -> ok when
    Appname :: atom().
%% ====================================================================
bundle(Appname) ->
    case epax_dep:bundle(Appname) of
        ok ->
            epax_com:success("~s bundled successfully", [Appname]);
        {error, Reason} ->
            epax_com:error(Reason, "Unable to bundle ~s", [Appname])
    end.

%% show/1
%% ====================================================================
%% @doc prints detailed information about the application
-spec show(Appname) -> ok when
    Appname :: atom().
%% ====================================================================
show(Appname) ->
    case epax_index:get_index_entry(Appname) of
        {ok, App} ->
            epax_com:success("~s====================", [format_app(App)]);
        {error, Reason} ->
            epax_com:error(Reason, "Unable to locate package ~s", [Appname])
    end.

%% format_app/1
%% ====================================================================
%% @doc returns a package details in printable format
-spec format_app(App) -> term() when
    App :: #application{}.
%% ====================================================================
format_app(App) ->
    Appname = App#application.name,
    FmtdDetails = format_details(Appname, App#application.details),
    Eqs = string:copies("=", 9-round(length(atom_to_list(Appname))/2)),
    epax_com:format("~s ~s ~s~s~n", [Eqs, Appname, Eqs, FmtdDetails]).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

format_details(Appname, Details) ->
    AllDeps = case epax_dep:find_all_deps_for(Appname) of
        {ok, []} ->
            ['no dependencies'];
        {ok, Deps} ->
            Deps;
        {error, _Reason} ->
            ['unable to find dependencies']
    end,
    epax_com:format([
        io_lib:format("~nStatus: ~s", ["installed"]),
        io_lib:format("~nPublisher: ~s", [keyfind(publisher, 1, Details)]),
        io_lib:format("~nLatest Version: ~s", [keyfind(tags, 1, Details)]),
        io_lib:format("~nDepends: ~s", [string:join(lists:map(fun atom_to_list/1, AllDeps), ", ")]),
        io_lib:format("~nDescription: ~s", [keyfind(description, 1, Details)])]).

keyfind(Key, N, TupleList) ->
    case lists:keyfind(Key, N, TupleList) of
        {Key, []} ->
            "unknown";
        {Key, [H|_T]} when is_list(H) ->
            H;
        {Key, Value} ->
            Value;
        false ->
            "unknown"
    end.
