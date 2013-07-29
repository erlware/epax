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
         update/0,
         bundle/1]).


%%============================================================================
%% API
%%============================================================================

%% install/0
%% ====================================================================
%% @doc installs epax, initialize index
-spec install() -> ok.
%% ====================================================================
install() ->
    Epax_loc = epax_os:get_abs_path(""),
    case epax_os:mkdir(Epax_loc) of
        {ok, _} ->
            epax_index:init();
        {error, Reason} ->
            epax_com:print_error(Reason, "exiting, installation failed!")
    end.

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
                    Message = lists:concat(["added ", atom_to_list(Appname), " to index."]),
                    epax_com:print_success(Message);
                {error, Reason} ->
                    epax_com:print_error(Reason, "unable to add to index!")
            end;
        {ok, Appname} ->
            Message = lists:concat([atom_to_list(Appname), " is already added!"]),
            epax_com:print_error(Message);
        {error, Reason} ->
             epax_com:print_error(Reason, "unable to add to index!")
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
            Message = lists:concat([atom_to_list(Appname), " is removed successfully."]),
            epax_com:print_success(Message);
        {error, Reason} ->
            Message = lists:concat([atom_to_list(Appname), " cannot be removed!"]),
            epax_com:print_error(Reason, Message)
    end.

%% list_apps/0
%% ====================================================================
%% @doc prints the app list stored in the index
-spec list_apps() -> ok.
%% ====================================================================
list_apps() ->
    case epax_index:get_applist() of
        {ok, []} ->
            epax_com:print_success("no app is added yet!");
        {ok, All_apps} ->
            epax_com:print_success("*** Erlang Apps ***"),
            lists:foreach(fun(Elem) -> io:format("  ~p~n", [Elem]) end, All_apps);
        {error, Reason} ->
            epax_com:print_error(Reason, "cannot retrieve the application list!")
    end.

%% update/0
%% ====================================================================
%% @doc updates the index
-spec update() -> ok.
%% ====================================================================
update() ->
    case epax_index:update_index() of
        ok ->
            epax_com:print_success("Index updated successfully.");
        {error, Reason} ->
            epax_com:print_error(Reason, "unable to update index!")
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
            Message = lists:concat([atom_to_list(Appname), " bundled successfully."]),
            epax_com:print_success(Message);
        {error, Reason} ->
            Message = lists:concat(["unable to bundle ", atom_to_list(Appname), "!"]),
            epax_com:print_error(Reason, Message)
    end.
