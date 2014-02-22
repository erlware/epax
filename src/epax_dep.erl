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
%%%

-module(epax_dep).
-include("epax.hrl").
-export([bundle/1,
         find_all_deps_for/1]).


%%============================================================================
%% API
%%============================================================================

%% bundle/1
%% ====================================================================
%% @doc bundles the app, finds all the dependencies and copies them
%% into the app folder
-spec bundle(Appname) -> Result when
    Appname :: atom(),
    Result  :: ok
             | {error, Reason},
    Reason  :: term().
%% ====================================================================
bundle(Appname) ->
    case find_all_deps_recursively_for(Appname) of
        {ok, Deps} ->
            copy_all_deps(Deps, Appname);
        {error, _} = E ->
            E
    end.

%% final_all_deps_for/1
%% ====================================================================
%% @doc returns all no standard list of direct dependent applications for
%% given application/package (i.e. not recursively)
-spec find_all_deps_for(Appname) -> Result when
    Appname :: atom(),
    Result  :: {ok, [atom()]}
             | {error, Reason},
    Reason  :: term().
%% ====================================================================
find_all_deps_for(Appname) ->
    case epax_index:app_exists(Appname) of
        {ok, false} ->
            {error, epax_com:format("~s does not exist", [Appname])};
        {ok, Appname} ->
            find_all_deps_for_helper(Appname);
        {error, _} = E ->
            E
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

find_all_deps_recursively_for(Appname) ->
    case find_all_deps_recursively_helper([Appname], []) of
        {ok, Deps} ->
            {ok, lists:delete(Appname, Deps)};
        {error, _} = E ->
            E
    end.

find_all_deps_recursively_helper([], AlreadyAdded) ->
    {ok, AlreadyAdded};
find_all_deps_recursively_helper([H|T], AlreadyAdded) ->
    case lists:member(H, AlreadyAdded)  of
        false ->
            case find_all_deps_for(H) of
                {ok, Deps} ->
                    find_all_deps_recursively_helper(lists:append(T, Deps), [H|AlreadyAdded]);
                {error, _} = E ->
                    E
            end;
        true ->
            find_all_deps_recursively_helper(T, AlreadyAdded)
    end.

find_all_deps_for_helper(Appname) ->
    case epax_com:get_appfile_content(Appname) of
        {ok, [AppContent]} ->
            AppDetails = element(3, AppContent),
            IncludeApps = case lists:keyfind(included_applications, 1, AppDetails) of
                false ->
                    [];
                {included_applications, List} ->
                    List
            end,
            App = case lists:keyfind(applications, 1, AppDetails) of
                false ->
                    [];
                {applications, L} ->
                    L
            end,
            AllApps = lists:append(IncludeApps, App),
            {ok, delete_standard_apps(AllApps)};
        {error, _} = E ->
            E
    end.

delete_standard_apps(Deps) ->
    lists:filter(fun(X) ->
        case code:lib_dir(X) of
            {error, bad_name} -> true;
            _ -> false
        end
    end,
    Deps).

copy_all_deps(Deps, Appname) ->
    ToDepsFolder = filename:join([epax_os:get_abs_path("packages"), atom_to_list(Appname), "deps"]),
    lists:foreach(fun(Dep) ->
            copy_dep(Dep, ToDepsFolder)
        end,
        Deps).

copy_dep(Dep, ToDepsFolder) ->
    From = filename:join(epax_os:get_abs_path("packages"), Dep),
    epax_com:console("Copying ~p~n", [Dep]),
    epax_os:copy_folder(From, ToDepsFolder).
