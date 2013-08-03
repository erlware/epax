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
%%% @doc main epax os module, runs os specific operations
-module(epax_repo).
-export([clone_app/1]).


%%============================================================================
%% API
%%============================================================================
clone_app(Link) ->
    Path = epax_os:get_abs_path("packages/temp"),
    os:cmd(lists:concat(["git clone ", Link, " ", Path])),
    case filelib:is_dir(Path) of
        true ->
            get_app_info(Link, "packages/temp");
        false ->
            {error, "unable to clone repo!"}
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================
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
        re:split(List_tags, "\n")).

collect_branches(Path) ->
    Ret = epax_os:run_in_dir(Path, "git branch --remote"),
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
