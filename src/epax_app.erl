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
-module(epax_app).
-include("epax.hrl").
-export([install/0,
         add_app/1,
         remove_app/1,
         list_apps/0,
         update/0,
         bundle/1]).


%%============================================================================
%% API
%%============================================================================

%% install/0
%% ====================================================================
%% @doc installs epax, initializes index
-spec install() -> ok.
%% ====================================================================
install() ->
    Epax_loc = epax_os:get_abs_path(""),
    epax_os:mkdir(Epax_loc),
    epax_index:init(),
    ?SUCCESS("epax successfully installed").

%% add_app/1
%% ====================================================================
%% @doc adds OTP application stored at Link (only supports git)
-spec add_app(Link) -> ok when
    Link :: string().
%% ====================================================================
add_app(Link) ->
    case epax_index:app_exists(Link) of
        {ok, false} ->
            case epax_index:checkout_repo_and_add_to_index(Link) of
                {ok, Appname} ->
                    ?SUCCESS("added ~s to index", [Appname]);
                {error, Reason} ->
                    ?ERROR(Reason, "unable to add to index")
            end;
        {ok, Appname} ->
            ?ERROR(already_added, "~s is already added", [Appname]);
        {error, Reason} ->
             ?ERROR(Reason, "unable to add to index")
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
            ?SUCCESS("~s is removed successfully", [Appname]);
        {error, Reason} ->
            ?ERROR(Reason, "~s cannot be removed from index", [Appname])
    end.

%% list_apps/0
%% ====================================================================
%% @doc prints the app list stored in the index (in alphabetical order)
-spec list_apps() -> ok.
%% ====================================================================
list_apps() ->
    case epax_index:get_applist() of
        {ok, []} ->
            ?SUCCESS("no app is added yet");
        {ok, All_apps} ->
            ?SUCCESS("=== Erlang Apps ===~s~n===================",
                [lists:foldl(fun(Elem, Acc) ->
                    string:concat(Acc, io_lib:format("~n  - ~s", [Elem])) end,
                    "",
                    All_apps)]);
        {error, Reason} ->
            ?ERROR(Reason, "cannot retrieve the application list")
    end.

%% update/0
%% ====================================================================
%% @doc updates the index
-spec update() -> ok.
%% ====================================================================
update() ->
    case epax_index:update_index() of
        ok ->
            ?SUCCESS("index updated successfully");
        {error, Reason} ->
            ?ERROR(Reason, "unable to update index")
    end.

%% bundle/0
%% ====================================================================
%% @doc bundles the app, finds all the dependencies and copies them into the
%% deps folder inside the app folder
-spec bundle(Appname) -> ok when
    Appname :: atom().
%% ====================================================================
bundle(Appname) ->
    case epax_dep:bundle(Appname) of
        ok ->
            ?SUCCESS("~s bundled successfully", [Appname]);
        {error, Reason} ->
            ?ERROR(Reason, "unable to bundle ~s", [Appname])
    end.
