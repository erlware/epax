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
-export([init/0,
		 app_exists/1,
		 checkout_repo_and_add_to_index/1,
         remove_from_index/1,
         get_applist/0,
         update_index/0]).


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
        _ ->
            {error, "please run `epax install` before running other epax commands!"}
    end.

%% checkout_repo_and_add_to_index/1
%% ====================================================================
%% @doc checks out the repo in the packages folder and adds details to
%% index
-spec checkout_repo_and_add_to_index(Link) -> Result when
    Link    :: string(),
    Result  :: {ok, Newapp}
             | {error, Reason},
    Newapp  :: atom(),
    Reason  :: term().
%% ====================================================================
checkout_repo_and_add_to_index(Link) ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            case clone_app(Link) of
                {ok, Newapp} ->
                    write_to_index_file([Newapp|Existing_apps]),
                    {ok, element(1, Newapp)};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, "please run `epax install` before running other epax commands!"}
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
            write_to_index_file(lists:keydelete(Appname, 1, Existing_apps)),
            run_in_dir(epax_os:get_abs_path("packages"), lists:concat(["rm -rf ", atom_to_list(Appname)])),
            ok;
        {error, _} ->
            {error, "please run `epax install` before running other epax commands!"}
    end.

%% get_applist/0
%% ====================================================================
%% @doc returns list of application added into the index
-spec get_applist() -> Result when
    Result   :: {ok, Allapps}
              | {error, Reason},
    Allapps :: [atom()],
    Reason   :: term().
%% ====================================================================
get_applist() ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            Unsorted_apps = lists:map(fun(App) -> element(1, App) end, Existing_apps),
            {ok, lists:sort(Unsorted_apps)};
        {error, _} ->
            {error, "please run `epax install` before running other epax commands!"}
    end.

%% update_index/0
%% ====================================================================
%% @doc updates details of apps in index
-spec update_index() -> ok | {error, Reason} when
    Reason :: term().
%% ====================================================================
update_index() ->
    case file:consult(epax_os:get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            write_to_index_file(lists:reverse(lists:foldl(fun(App, Acc) ->
                    case update_app(App) of
                        {ok, Newapp} ->
                            io:format("~p updated!~n", [element(1, App)]),
                            [Newapp|Acc];
                        {error, Reason} ->
                            io:format("~p unable to update, because ~p~n", [element(1, App), Reason]),
                            [App|Acc]
                    end
                end,
                [],
                Existing_apps)));
        {error, _} ->
            {error, "please run `epax install` before running other epax commands!"}
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================
write_to_index_file(Data) ->
    file:write_file(epax_os:get_abs_path("index.cfg"), io_lib:fwrite("~p.\n",[Data])).

app_exists(Info, Existing_apps) when is_list(Info) ->
    case lists:keymember(Info, 2, Existing_apps) of
        false ->
            false;
        {Appname, Info, _} ->
            Appname
    end;
app_exists(Info, Existing_apps) when is_atom(Info) ->
    case lists:keymember(Info, 1, Existing_apps) of
        false ->
            false;
        {Info, _, _} ->
            Info
    end;
app_exists(_, _) ->
    false.

clone_app(Link) ->
    Path = epax_os:get_abs_path("packages/temp"),
    os:cmd(lists:concat(["git clone ", Link, " ", Path])),
    case filelib:is_dir(Path) of
        true ->
            get_app_info(Link, "packages/temp");
        false ->
            {error, "unable to clone repo!"}
    end.

get_app_info(Link, Path) ->
    Abs_path = epax_os:get_abs_path(Path),
    case find_publisher(Path) of
        {ok, {Appname, Description, Author}} ->
            Tags = collect_tags(Abs_path),
            Branches = collect_branches(Abs_path),
            os:cmd(lists:concat(["mv ", Abs_path, " ", epax_os:get_abs_path("packages/"), Appname])),
            {ok, {Appname, Link, [{description, Description},
                                  {publisher, Author},
                                  {tags, Tags},
                                  {branches, Branches}]}};
        {error, Reason} ->
            os:cmd(lists:concat(["rm -rf ", Abs_path])),
            {error, Reason}
    end.

find_publisher(Path) ->
    case epax_com:get_appfile_content(Path) of
        {ok, Info} ->
            Appname = element(2, Info),
            Description = find_key(description, element(3, Info)),
            Author = find_key(author, element(3, Info)),
            {ok, {Appname, Description, Author}};
        {error, Reason} ->
            {error, Reason}
    end.

find_key(Key, List) ->
    io:format("Format"),
    case lists:keyfind(Key, 1, List) of
        {Key, Value} ->
            Value;
        false ->
            ""
    end.

collect_tags(Path) ->
    List_tags = run_in_dir(Path, "git tag"),
    lists:foldl(fun(T, Acc) ->
            case Tag = binary_to_list(T) of
                [] ->
                    Acc;
                _ ->
                    [Tag|Acc]
            end
        end,
        [],
        re:split(List_tags, "\n")).

collect_branches(Path) ->
    Ret = run_in_dir(Path, "git branch --remote"),
    List_branches = re:split(Ret, "\n"),
    lists:foldl(fun(Branch, Acc) ->
            Full_branch = binary_to_list(Branch),
            case erlang:length(re:split(Full_branch, "origin/")) of
                2 ->
                    {match, [{Loc, Len}]} = re:run(Full_branch, "origin/"),
                    [lists:nthtail(Loc+Len, Full_branch)|Acc];
                _ ->
                    Acc
            end
        end,
        [],
        List_branches).

run_in_dir(Path, Cmd) ->
    os:cmd(lists:concat(["cd ", Path, " && ", Cmd])).

update_app(App) ->
    Path = epax_os:get_abs_path(lists:concat(["packages/", element(1, App)])),
    run_in_dir(Path, "git pull"),
    case filelib:is_dir(Path) of
        true ->
            get_app_info(element(2, App), Path);
        false ->
            {error, "unable to clone repo!"}
    end.
