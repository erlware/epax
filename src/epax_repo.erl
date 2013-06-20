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
%%%

-module(epax_repo).
-include("epax.hrl").
-export([clone_app/2,
         update_repo/1]).


%%============================================================================
%% API
%%============================================================================

%% clone_app/2
%% ====================================================================
%% @doc downloads the repository, returns index entry
-spec clone_app(Link, Options) -> Result when
    Link       :: string(),
    Options    :: [term()],
    Result     :: {ok, IndexEntry}
                | {error, Reason},
    IndexEntry :: {application, Appname, Link, RepoType, [{Key, Value}]},
    Link       :: string(),
    RepoType   :: atom(),
    Key        :: atom(),
    Value      :: term(),
    Appname    :: atom(),
    Reason     :: term().
%% ====================================================================
clone_app(Link, Options) ->
    Path = epax_os:get_abs_path(filename:join("packages", "temp")),
    case type_of_repo(Link, Options) of
        {ok, RepoType} ->
            download_repo(RepoType, Link, Path),
            case filelib:is_dir(Path) of
                true ->
                    get_info(RepoType, Link, Path);
                false ->
                    {error, "Unable to download repo"}
            end;
        {error, _} = E ->
            E
    end.

%% update_repo/1
%% ====================================================================
%% @doc updates the repository, returns the new index entry
-spec update_repo(App) -> Result when
    App        :: {application, Appname, Link, RepoType, [{Key, Value}]},
    Result     :: {ok, IndexEntry}
                | {error, Reason},
    IndexEntry :: {application, Appname, Link, RepoType, [{Key, Value}]},
    Link       :: string(),
    RepoType   :: atom(),
    Key        :: atom(),
    Value      :: term(),
    Appname    :: atom(),
    Reason     :: term().
%% ====================================================================
update_repo(App) ->
    Path = epax_os:get_abs_path(filename:join("packages", atom_to_list(App#application.name))),
    update_files(Path, App#application.repo_type),
    get_app_info(App#application.repo_type, App#application.repo_link, Path).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

type_of_repo(Link, Options) ->
    case proplists:get_value(repo_type, Options) of
        undefined ->
            GitTest = string:str(Link, ".git") =/= 0,
            BzrTest = (string:str(Link, "lp:") =/= 0) or (string:str(Link, "launchpad") =/= 0),
            SvnTest = string:str(Link, ".svn") =/= 0,
            if
                GitTest ->
                    {ok, git};
                BzrTest ->
                    {ok, bzr};
                SvnTest ->
                    {ok, svn};
                true ->
                    {error, "Unknown type of repo, use -r option to specify type of repo"}
            end;
        Else ->
            {ok, list_to_atom(Else)}
    end.

download_repo(git, Link, Path) ->
    epax_os:run_in_dir("", epax_com:format(["git clone ", Link, " ", Path]));
download_repo(bzr, Link, Path) ->
    epax_os:run_in_dir("", epax_com:format(["bzr branch ", Link, " ", Path]));
download_repo(svn, Link, Path) ->
    epax_os:run_in_dir("", epax_com:format(["svn checkout ", Link, " ", Path])).

get_info(RepoType, Link, Path) ->
    case get_app_info(RepoType, Link, Path) of
        {ok, App} = Ret ->
            To = filename:join(epax_os:get_abs_path("packages"), App#application.name),
            epax_os:mv_folder(Path, To),
            Ret;
        {error, _} = E ->
            epax_os:rmdir(Path),
            E
    end.

get_app_info(RepoType, Link, Path) ->
    case find_publisher(Path) of
        {ok, {Appname, Description, Author}} ->
            {ok, #application{name=Appname,
                              repo_link=Link,
                              repo_type=RepoType,
                              details=get_details(RepoType, Description, Author, Path)}};
        {error, _} = E ->
            E
    end.

get_details(git, Description, Author, Path) ->
    Tags = collect_tags(Path),
    Branches = collect_branches(Path),
    [{description, Description}, {publisher, Author}, {tags, Tags}, {branches, Branches}];
get_details(RepoType, Description, Author, Path) ->
    Rev = collect_max_rev(RepoType, Path),
    [{description, Description}, {publisher, Author}, {max_rev, Rev}].

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
    ListTags = epax_os:run_in_dir(Path, "git tag"),
    lists:foldl(fun(T, Acc) ->
            case Tag = binary_to_list(T) of
                [] ->
                    Acc;
                _ ->
                    [Tag|Acc]
            end
        end,
        [],
        re:split(ListTags, "[\n ]")).

collect_branches(Path) ->
    Ret = epax_os:run_in_dir(Path, "git branch --remote"),
    ListBranches = re:split(Ret, "\n"),
    lists:foldl(fun(Branch, Acc) ->
            FullBranch = binary_to_list(Branch),
            case erlang:length(re:split(FullBranch, "origin/")) of
                2 ->
                    {match, [{Loc, Len}]} = re:run(FullBranch, "origin/"),
                    [lists:nthtail(Loc+Len, FullBranch)|Acc];
                _ ->
                    Acc
            end
        end,
        [],
        ListBranches).

collect_max_rev(bzr, Path) ->
    Revs = epax_os:run_in_dir(Path, "bzr revno"),
    [MaxRev] = find_max_rev(Revs),
    list_to_integer(MaxRev);
collect_max_rev(svn, Path) ->
    Revs = epax_os:run_in_dir(Path, "svnversion"),
    [MaxRev] = find_max_rev(Revs),
    list_to_integer(MaxRev).

find_max_rev(RevList) ->
    lists:foldl(fun(R, Acc) ->
            case Rev = binary_to_list(R) of
                [] ->
                    Acc;
                _ ->
                    [Rev|Acc]
            end
        end,
        [],
        re:split(RevList, "[\n ]")).

update_files(Path, RepoType) ->
    case RepoType of
        git ->
            epax_os:run_in_dir(Path, "git pull");
        bzr ->
            epax_os:run_in_dir(Path, "bzr update");
        svn ->
            epax_os:run_in_dir(Path, "svn update")
    end.
