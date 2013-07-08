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
-export([install/0,
         add_app/1,
         remove_app/1,
         list_apps/0,
         update/0]).


%%============================================================================
%% API
%%============================================================================

%% install/0
%% ====================================================================
%% @doc installs epax, initialize index
-spec install() -> ok.
%% ====================================================================
 install() ->
    os:cmd(lists:concat(["mkdir -p ", epax_com:get_abs_path("")])),           % creating .epax directory if not exists
    epax_index:init().

%% add_app/1
%% ====================================================================
%% @doc add OTP application stored at Link (only supports git)
-spec add_app(Link) -> ok when
    Link :: string().
%% ====================================================================
add_app(Link) ->
    case epax_index:app_exists(Link) of
        {ok, true} ->
            io:format("~p~n", ["Error: app is already added!"]);
        {ok, false} ->
            case epax_index:checkout_repo_and_add_to_index(Link) of
                {ok, Appname} ->
                    io:format("successfully added ~p to index!~n", [Appname]);
                {error, Reason} ->
                    io:format("Error! cannot add into index because ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

%% remove_app/1
%% ====================================================================
%% @doc removes OTP application from index
-spec remove_app(Appname) -> ok when
    Appname :: atom().
%% ====================================================================
remove_app(Appname) ->
    case epax_index:remove_from_index(Appname) of
        {error, Reason} ->
            io:format("Error! ~p~n", [Reason]);
        _ ->
            io:format("Application ~p removed successfully!~n", [Appname])
    end.

%% list_apps/0
%% ====================================================================
%% @doc prints the app list stored in the index
-spec list_apps() -> ok.
%% ====================================================================
list_apps() ->
    case epax_index:get_applist() of
        [] ->
            io:format("no app added yet!");
        All_apps ->
            lists:foreach(fun(Elem) -> io:format("~p~n", [Elem]) end, All_apps)
    end.

%% update/0
%% ====================================================================
%% @doc updates the index
-spec update() -> ok.
%% ====================================================================
update() ->
    case epax_index:update_index() of
        {error, Reason} ->
            io:format("Error! ~p~n", [Reason]);
        _ ->
            io:format("Index updated successfully!~n")
    end.
