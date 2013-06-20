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
%%%

-module(epax_index).
-include("epax.hrl").
-export([init/0,
         app_exists/1,
         get_index_entry/1,
         checkout_repo_and_add_to_index/2,
         remove_from_index/1,
         get_applist/0,
         update_index/0,
         check_index/0,
         search/2]).


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
    Info    :: string()
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
        {ok, [ExistingApps]} ->
            {ok, app_exists(Info, ExistingApps)};
        {error, _} ->
            {error, "Run `epax init` before running other epax commands"}
    end.

%% get_index_entry/1
%% ====================================================================
%% @doc returns the application tuple stored in the index for the given
%% package/application
-spec get_index_entry(Appname) -> Result when
    Appname :: atom(),
    Result  :: {ok, #application{}}
             | {error, Reason},
    Reason  :: term().
%% ====================================================================
get_index_entry(Appname) ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [ExistingApps]} ->
            case lists:keyfind(Appname, #application.name, ExistingApps) of
                false ->
                    {error, not_found};
                Ret ->
                    {ok, Ret}
            end;
        {error, _} ->
            {error, "Run `epax init` before running other epax commands"}
    end.

%% checkout_repo_and_add_to_index/2
%% ====================================================================
%% @doc downloads the repo in the packages folder and adds details to
%% index
-spec checkout_repo_and_add_to_index(Link, Options) -> Result when
    Link    :: string(),
    Options :: [term()],
    Result  :: {ok, App}
             | {error, Reason},
    App     :: atom(),
    Reason  :: term().
%% ====================================================================
checkout_repo_and_add_to_index(Link, Options) ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [ExistingApps]} ->
            clone_app(Link, ExistingApps, Options);
        {error, _} ->
            {error, "Run `epax init` before running other epax commands"}
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
        {ok, [ExistingApps]} ->
            Path = filename:join("packages", atom_to_list(Appname)),
            epax_os:rmdir(epax_os:get_abs_path(Path)),
            write_to_index_file(lists:keydelete(Appname, #application.name, ExistingApps));
        {error, _} ->
            {error, "Run `epax init` before running other epax commands"}
    end.

%% get_applist/0
%% ====================================================================
%% @doc returns the list of applications added into the index
-spec get_applist() -> Result when
    Result  :: {ok, AllApps}
             | {error, Reason},
    AllApps :: [atom()],
    Reason  :: term().
%% ====================================================================
get_applist() ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [ExistingApps]} ->
            UnsortedApps = lists:map(fun(App) -> App#application.name end, ExistingApps),
            {ok, lists:sort(UnsortedApps)};
        {error, _} ->
            {error, "Run `epax init` before running other epax commands"}
    end.

%% update_index/0
%% ====================================================================
%% @doc pulls any changes in the repository and updates the details of
%% applications in index
-spec update_index() -> ok | {error, Reason} when
    Reason :: term().
%% ====================================================================
update_index() ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [ExistingApps]} ->
            write_to_index_file(lists:reverse(lists:foldl(
                fun(App, Acc) ->
                    case epax_repo:update_repo(App) of
                        {ok, Newapp} ->
                            epax_com:console(" => ~p updated~n", [App#application.name]),
                            [Newapp|Acc];
                        {error, Reason} ->
                            epax_com:console(" ** ~p: unable to update, because ~p~n", [App#application.name, Reason]),
                            [App|Acc]
                    end
                end,
                [],
                ExistingApps)));
        {error, _} ->
            {error, "Run `epax init` before running other epax commands"}
    end.

%% check_index/0
%% ====================================================================
%% @doc assuming the index correct, verifies the downloaded packages,
%% deletes rest of the folders. If anything goes wrong, it initializes
%% the index
-spec check_index() -> ok | {error, Reason} when
    Reason :: term().
%% ====================================================================
check_index() ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [ExistingApps]} ->
            check_apps(ExistingApps);
        {error, _} = E ->
            epax_app:init(),
            E
    end.

%% search/2
%% ====================================================================
%% @doc performs a full text search for given regular expression in the
%% list of installed packages. Various options can be provided to control
%% the search domain
-spec search(Regex, Option) -> ok | {error, Reason} when
    Regex  :: string(),
    Option :: [term()],
    Reason :: term().
%% ====================================================================
search(Regex, Option) ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [ExistingApps]} ->
            AnyPackageFound = lists:foldl(
                fun(App, Acc) ->
                    Appname = App#application.name,
                    IsPrint = (match == re:run(atom_to_list(Appname), Regex, [{capture, none}]))
                              or (not proplists:get_value(names_only, Option, false)
                              and (re:run(proplists:get_value(description, App#application.details, ""),
                                          Regex,
                                          [{capture, none}]) == match)),
                    case IsPrint of
                        true ->
                            print_package(proplists:get_value(full, Option, false), App, Acc),
                            true;
                        false ->
                            Acc
                    end
                end,
                false,
                ExistingApps),
            case AnyPackageFound of
                true ->
                    epax_com:console("====================~n", []);
                false ->
                    epax_com:success("no package found")
            end;
        {error, _} ->
            {error, "Run `epax init` before running other epax commands"}
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

write_to_index_file(Data) ->
    file:write_file(epax_os:get_abs_path("index.cfg"), io_lib:fwrite("~p.\n", [Data])).

clone_app(Link, ExistingApps, Options) ->
    case epax_repo:clone_app(Link, Options) of
        {ok, NewappDetails} ->
            case write_to_index_file([NewappDetails|ExistingApps]) of
                ok ->
                    {ok, NewappDetails#application.name};
                {error, _} = E ->
                    Path = filename:join("packages", NewappDetails#application.name),
                    epax_os:rmdir(epax_os:get_abs_path(Path)),
                    E
             end;
        {error, _} = E ->
            E
    end.

app_exists(Info, ExistingApps) when is_list(Info) ->
    case lists:keyfind(Info, #application.repo_link, ExistingApps) of
        false ->
            false;
        App ->
            App#application.name
    end;
app_exists(Info, ExistingApps) when is_atom(Info) ->
    case lists:keyfind(Info, #application.name, ExistingApps) of
        false ->
            false;
        App ->
            App#application.name
    end;
app_exists(_, _) ->
    false.

check_apps(ExistingApps) ->
    {ok, AllPkgs} = file:list_dir(epax_os:get_abs_path("packages")),
    {Apps, ExtraDirs} = lists:foldl(fun(AppInfo, {Apps, Pkgs}) ->
            fix_package_if(AppInfo, Apps, Pkgs)
        end,
        {[], AllPkgs},
        ExistingApps),
    case write_to_index_file(Apps) of
        ok ->
            lists:foreach(fun(Dir) ->
                Path = epax_os:get_abs_path(filename:join("packages", Dir)),
                epax_os:rmdir(Path)
            end,
            ExtraDirs);
        {error, _} = E ->
            E
    end.

fix_package_if(AppInfo, Apps, Pkgs) ->
    App = AppInfo#application.name,
    RestPkgs = lists:delete(atom_to_list(App), Pkgs),
    case epax_repo:update_repo(AppInfo) of
        {ok, NewAppInfo} ->
            epax_com:console(" => ~p checked and updated~n", [App]),
            {[NewAppInfo|Apps], RestPkgs};
        {error, _} ->
            case try_cloning_again(AppInfo) of
                {ok, NewAppInfo} ->
                    epax_com:console(" => ~p checked and updated~n", [NewAppInfo#application.name]),
                    {[NewAppInfo|Apps], RestPkgs};
                {error, Reason} ->
                    epax_com:console(" ** ~p unable to fix, because ~p~n", [App, Reason]),
                    {Apps, RestPkgs}
            end
    end.

try_cloning_again(AppInfo) ->
    Path = epax_os:get_abs_path(filename:join("packages", atom_to_list(AppInfo#application.name))),
    case (catch epax_os:rmdir(Path)) of
        ok ->
            epax_repo:clone_app(AppInfo#application.repo_link, [{repo_type, AppInfo#application.repo_type}]);
        Reason ->
            {error, Reason}
    end.

print_package(true, App, Acc) ->
    case Acc of
        true ->
            epax_com:console("~s~n", [epax_app:format_app(App)]);
        false ->
            epax_com:success("~s", [epax_app:format_app(App)])
    end;
print_package(false, App, Acc) ->
    case Acc of
        true ->
            epax_com:console("  - ~p~n", [App#application.name]);
        false ->
            epax_com:success("=== Erlang Apps ===~n  - ~p", [App#application.name])
    end.
