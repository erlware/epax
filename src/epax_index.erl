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
%%% @doc provides an API to handle the index stored locally
-module(epax_index).
-include("epax.hrl").
-export([init/0,
		 app_exists/1,
		 checkout_repo_and_add_to_index/1,
         remove_from_index/1,
         get_applist/0,
         update_index/0,
         check_index/0]).


%%============================================================================
%% API
%%============================================================================

%% init/0
%% ====================================================================
%% @doc initializes index
-spec init() -> ok.
%% ====================================================================
init() ->
    epax_os:touch(epax_os:get_abs_path("index.cfg")),
    write_to_index_file([]),
    epax_os:mkdir(epax_os:get_abs_path("packages")),
    epax_os:rmdir(epax_os:get_abs_path("packages/*")),
    ok.

%% app_exists/1
%% ====================================================================
%% @doc returns true when the app is already added into the index,
%% false otherwise. Info can be either the link to the application or
%% the name of the application
-spec app_exists(Info) -> Result when
    Info   :: string()
            | atom(),
    Result  :: {ok, Status}
             | {error, Reason},
    Status  :: Appname
             | false,
    Appname :: atom(),
    Reason  :: string().
%% ====================================================================
app_exists(Info) ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            {ok, app_exists(Info, Existing_apps)};
        {error, _} ->
            {error, "run `epax init` before running other epax commands"}
    end.

%% checkout_repo_and_add_to_index/1
%% ====================================================================
%% @doc downloads the repo in the packages folder and adds details to
%% index
-spec checkout_repo_and_add_to_index(Link) -> Result when
    Link    :: string(),
    Result  :: {ok, App}
             | {error, Reason},
    App  :: atom(),
    Reason  :: term().
%% ====================================================================
checkout_repo_and_add_to_index(Link) ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            clone_app(Link, Existing_apps);
        {error, _} ->
            {error, "run `epax init` before running other epax commands"}
    end.

%% remove_from_index/1
%% ====================================================================
%% @doc deletes app from the index
-spec remove_from_index(Appname) -> Result when
    Appname :: atom(),
    Result  :: ok
             | {error, Reason},
    Reason  :: term().
%% ====================================================================
remove_from_index(Appname) ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            Path = filename:join("packages", Appname),
            epax_os:rmdir(epax_os:get_abs_path(Path)),
            write_to_index_file(lists:keydelete(Appname, #application.name, Existing_apps));
        {error, _} ->
            {error, "run `epax init` before running other epax commands"}
    end.

%% get_applist/0
%% ====================================================================
%% @doc returns the list of applications added into the index
-spec get_applist() -> Result when
    Result   :: {ok, Allapps}
              | {error, Reason},
    Allapps :: [atom()],
    Reason   :: term().
%% ====================================================================
get_applist() ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            Unsorted_apps = lists:map(fun(App) -> App#application.name end, Existing_apps),
            {ok, lists:sort(Unsorted_apps)};
        {error, _} ->
            {error, "run `epax init` before running other epax commands"}
    end.

%% update_index/0
%% ====================================================================
%% @doc pulls any changes in the repo and updates the details of apps in index
-spec update_index() -> ok | {error, Reason} when
    Reason :: term().
%% ====================================================================
update_index() ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            write_to_index_file(lists:reverse(
                lists:foldl(fun(App, Acc) ->
                    case epax_repo:update_repo(App) of
                        {ok, Newapp} ->
                            ?CONSOLE(" => ~p updated!~n", [App#application.name]),
                            [Newapp|Acc];
                        {error, Reason} ->
                            ?CONSOLE(" ** ~p: unable to update, because ~p~n", [App#application.name, Reason]),
                            [App|Acc]
                    end
                end,
                [],
                Existing_apps)));
        {error, _} ->
            {error, "run `epax init` before running other epax commands"}
    end.

%% check_index/0
%% ====================================================================
%% @doc assuming the index correct, verifies the downloaded packages,
%% deletes rest of the folders
-spec check_index() -> ok | {error, Reason} when
    Reason :: term().
%% ====================================================================
check_index() ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            check_apps(Existing_apps);
        {error, _} = E ->
            epax_app:init(),
            E
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

write_to_index_file(Data) ->
    file:write_file(epax_os:get_abs_path("index.cfg"), io_lib:fwrite("~p.\n",[Data])).

clone_app(Link, Existing_apps) ->
    case epax_repo:clone_app(Link) of
        {ok, Newapp_details} ->
            case write_to_index_file([Newapp_details|Existing_apps]) of
                ok ->
                    {ok, Newapp_details#application.name};
                {error, _} = E ->
                    Path = filename:join("packages", Newapp_details#application.name),
                    epax_os:rmdir(epax_os:get_abs_path(Path)),
                    E
             end;
        {error, _} = E ->
            E
    end.

app_exists(Info, Existing_apps) when is_list(Info) ->
    case lists:keyfind(Info, #application.repo_link, Existing_apps) of
        false ->
            false;
        App ->
            App#application.name
    end;
app_exists(Info, Existing_apps) when is_atom(Info) ->
    case lists:keyfind(Info, #application.name, Existing_apps) of
        false ->
            false;
        App ->
            App#application.name
    end;
app_exists(_, _) ->
    false.

check_apps(Existing_apps) ->
    {ok, All_pkgs} = file:list_dir(epax_os:get_abs_path("packages")),
    {Apps, Extra_dirs} = lists:foldl(fun(App_info, {Apps, Pkgs}) ->
        fix_package_if(App_info, Apps, Pkgs)
    end,
    {[], All_pkgs},
    Existing_apps),
    case write_to_index_file(Apps) of
        ok ->
            lists:foreach(fun(Dir) ->
                Path = epax_os:get_abs_path(filename:join("packages", Dir)),
                epax_os:rmdir(Path)
            end,
            Extra_dirs);
        {error, _} = E ->
            E
    end.

fix_package_if(App_info, Apps, Pkgs) ->
    App = App_info#application.name,
    Rest_pkgs = lists:delete(atom_to_list(App), Pkgs),
    case epax_repo:update_repo(App_info) of
        {ok, New_app_info} ->
            ?CONSOLE(" => ~p checked and updated!~n", [App]),
            {[New_app_info|Apps], Rest_pkgs};
        {error, _} ->
            case try_cloning_again(App_info) of
                {ok, New_app_info} ->
                    ?CONSOLE(" => ~p checked and updated!~n", [New_app_info#application.name]),
                    {[New_app_info|Apps], Rest_pkgs};
                {error, Reason} ->
                    ?CONSOLE(" ** ~p: unable to fix, because ~p~n", [App, Reason]),
                    {Apps, Rest_pkgs}
            end
    end.

try_cloning_again(App_info) ->
    Path = epax_os:get_abs_path(filename:join("packages", App_info#application.name)),
    case (catch epax_os:rmdir(Path)) of
        ok ->
            epax_repo:clone_app(App_info#application.repo_link);
        E ->
            {error, E}
    end.
