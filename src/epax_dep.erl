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
%%% @doc handles dependency solving step for epax
-module(epax_dep).
-export([bundle/1]).
-define(STAND_APPS, [sasl, ssl, stdlib, kernel]).

%%============================================================================
%% API
%%============================================================================

%% bundle/1
%% ====================================================================
%% @doc bundles the app, finds all the dependencies and copies them into the
%% app folder
-spec bundle(Appname) -> Result when
    Appname :: atom(),
    Result  ::  ok
             | {error, Reason},
    Reason  :: term().
%% ====================================================================
bundle(Appname) ->
    case find_all_deps_recursively_for(Appname) of
        {ok, Deps} ->
            copy_all_deps(Deps, Appname);
        {error, Reason} ->
            {error, Reason}
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================
find_all_deps_recursively_for(Appname) ->
    case find_all_deps_recursively_helper([Appname], []) of
        {ok, Deps} ->
            {ok, lists:delete(Appname, Deps)};
        {error, Reason} ->
            {error, Reason}
    end.

find_all_deps_recursively_helper([], Already_added) ->
    {ok, Already_added};
find_all_deps_recursively_helper([H|T], Already_added) ->
    case lists:member(H, Already_added)  of
        false ->
            case find_all_deps_for(H) of
                {ok, Deps} ->
                    find_all_deps_recursively_helper(lists:append(T, Deps), [H|Already_added]);
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            find_all_deps_recursively_helper(T, Already_added)
    end.

find_all_deps_for(Appname) ->
    case epax_index:app_exists(Appname) of
        {ok, Appname} ->
            find_all_deps_for_helper(Appname);
        {ok, false} ->
            {error, lists:concat([atom_to_list(Appname), " is not found!"])};
        {error, Reason} ->
            {error, Reason}
    end.

find_all_deps_for_helper(Appname) ->
    case epax_com:get_appfile_content(Appname) of
        {ok, [App_content]} ->
            App_details = element(3, App_content),
            Include_app = case lists:keyfind(included_applications, 1, App_details) of
                false ->
                    [];
                {included_applications, List} ->
                    List
            end,
            App = case lists:keyfind(applications, 1, App_details) of
                false ->
                    [];
                {applications, L} ->
                    L
            end,
            All_apps = lists:append(Include_app, App),
            {ok, delete_standard_apps(All_apps)};
        {error, Reason} ->
            {error, Reason}
    end.

delete_standard_apps(Deps) ->
    lists:filter(fun(X) ->
        case lists:member(X, ?STAND_APPS) of
            true -> false;
            false -> true
        end
    end,
    Deps).

copy_all_deps(Deps, Appname) ->
    To_deps_folder = epax_os:get_abs_path(lists:concat(["packages/", Appname, "/deps"])),
    lists:foreach(fun(Dep) ->
        copy_dep(Dep, To_deps_folder)
    end,
    Deps).

copy_dep(Dep, To_deps_folder) ->
    From = epax_os:get_abs_path(lists:concat(["packages/", Dep])),
    io:format("copying ~p~n", [Dep]),
    epax_os:copy_folder(From, To_deps_folder).
