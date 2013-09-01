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
%%% @doc main epax repo module, takes care of interaction with
%%% external repository
-module(epax_repo).
-include("epax.hrl").
-export([clone_app/1,
         update_repo/1]).


%%============================================================================
%% API
%%============================================================================

%% clone_app/1
%% ====================================================================
%% @doc downloads the repository, returns index entry
-spec clone_app(Link) -> Result when
    Link        :: string(),
    Result      :: {ok, Index_entry}
                 | {error, Reason},
    Index_entry :: {application, Appname, Link, Repo_type, [{Key, Value}]},
    Link        :: string(),
    Repo_type   :: atom(),
    Key         :: atom(),
    Value       :: term(),
    Appname     :: atom(),
    Reason      :: term().
%% ====================================================================
clone_app(Link) ->
    Path = epax_os:get_abs_path(filename:join("packages", "temp")),
    case type_of_repo(Link) of
        {ok, Repo_type} ->
            download_repo(Repo_type, Link, Path),
            case filelib:is_dir(Path) of
                true ->
                    get_info(Repo_type, Link, Path);
                false ->
                    {error, "unable to download repo"}
            end;
        {error, _} = E ->
            E
    end.

%% update_repo/1
%% ====================================================================
%% @doc updates the repository, returns the new index entry
-spec update_repo(App) -> Result when
    App         :: {application, Appname, Link, Repo_type, [{Key, Value}]},
    Result      :: {ok, Index_entry}
                 | {error, Reason},
    Index_entry :: {application, Appname, Link, Repo_type, [{Key, Value}]},
    Link        :: string(),
    Repo_type   :: atom(),
    Key         :: atom(),
    Value       :: term(),
    Appname     :: atom(),
    Reason      :: term().
%% ====================================================================
update_repo(App) ->
    Path = epax_os:get_abs_path(filename:join("packages", App#application.name)),
    case update_files(App#application.repo_link, Path) of
        {error, _} = E ->
            E;
        _ ->
            get_app_info(App#application.repo_type, App#application.repo_link, Path)
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

type_of_repo(Link) ->
    Git_test = string:str(Link, ".git") =/= 0,
    Bzr_test = (string:str(Link, "lp:") =/= 0) or (string:str(Link, "launchpad") =/= 0),
    Svn_test = string:str(Link, ".svn") == 0,
    if
        Git_test ->
            {ok, git};
        Bzr_test ->
            {ok, bzr};
        Bzr_test ->
            {ok, bzr};
        Svn_test ->
            {ok, svn};
        true ->
            {error, "unknown type of repo, use -r option to specify type of repo"}
    end.

download_repo(git, Link, Path) ->
    epax_os:run_in_dir("", lists:concat(["git clone ", Link, " ", Path]));
download_repo(bzr, Link, Path) ->
    epax_os:run_in_dir("", lists:concat(["bzr branch ", Link, " ", Path]));
download_repo(svn, Link, Path) ->
    epax_os:run_in_dir("", lists:concat(["svn checkout ", Link, " ", Path])).

get_info(Repo_type, Link, Path) ->
    case get_app_info(Repo_type, Link, Path) of
        {ok, App} = Ret ->
            To = filename:join(epax_os:get_abs_path("packages/"), App#application.name),
            epax_os:mv_folder(Path, To),
            Ret;
        {error, _} = E ->
            epax_os:rmdir(Path),
            E
    end.

get_app_info(git, Link, Path) ->
    case find_publisher(Path) of
        {ok, {Appname, Description, Author}} ->
            Tags = collect_tags(Path),
            Branches = collect_branches(Path),
            {ok, #application{name=Appname,
                              repo_link=Link,
                              repo_type=git,
                              details=[{description, Description},
                                        {publisher, Author},
                                        {tags, Tags},
                                        {branches, Branches}]}};
        {error, _} = E ->
            E
    end;
get_app_info(Repo_type, Link, Path) ->
    case find_publisher(Path) of
        {ok, {Appname, Description, Author}} ->
            Rev = collect_max_rev(Repo_type, Path),
            {ok, #application{name=Appname,
                              repo_link=Link,
                              repo_type=Repo_type,
                              details=[{description, Description},
                                        {publisher, Author},
                                        {max_rev, Rev}]}};
        {error, _} = E ->
            E
    end.

find_publisher(Path) ->
    case epax_com:get_appfile_content(Path) of
        {ok, [Info]} ->
            Appname = element(2, Info),
            Author = find_key(author, element(3, Info)),
            Description = find_key(description, element(3, Info)),
            {ok, {Appname, Description, Author}};
        {error, _} = E ->
            E
    end.

find_key(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} ->
            Value;
        false ->
            ""
    end.

collect_tags(Path) ->
    List_tags = epax_os:run_in_dir(Path, "git tag"),
    lists:foldl(fun(T, Acc) ->
        case Tag = binary_to_list(T) of
            [] ->
                Acc;
            _ ->
                [Tag|Acc]
        end
    end,
    [],
    re:split(List_tags, "[\n ]")).

collect_branches(Path) ->
    Ret = epax_os:run_in_dir(Path, "git branch --remote"),
    List_branches = re:split(Ret, "[\n ]"),
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

collect_max_rev(bzr, Path) ->
    Revs = epax_os:run_in_dir(Path, "bzr revno"),
    [Max_rev] = find_max_rev(Revs),
    Max_rev;
collect_max_rev(svn, Path) ->
    Revs = epax_os:run_in_dir(Path, "svnversion"),
    [Max_rev] = find_max_rev(Revs),
    Max_rev.

find_max_rev(Rev_List) ->
    lists:foldl(fun(R, Acc) ->
            case Rev = binary_to_list(R) of
                [] ->
                    Acc;
                _ ->
                    [Rev|Acc]
            end
        end,
        [],
        re:split(Rev_List, "[\n ]")).

update_files(Link, Path) ->
    case type_of_repo(Link) of
        {ok, git} ->
            epax_os:run_in_dir(Path, "git pull");
        {ok, bzr} ->
            epax_os:run_in_dir(Path, "bzr update");
        {ok, svn} ->
            epax_os:run_in_dir(Path, "svn update");
        {error, _} = E ->
            E
    end.
