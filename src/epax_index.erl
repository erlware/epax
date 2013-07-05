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
         get_applist/0]).


%%============================================================================
%% API
%%============================================================================

%% init/0
%% ====================================================================
%% @doc initializes index
-spec init() -> ok.
%% ====================================================================
init() ->
    os:cmd(lists:concat(["mkdir -p ", get_abs_path("")])),           % creating .epax directory if not exists
    os:cmd(lists:concat(["touch ", get_abs_path("index.cfg")])),     % creating index.cfg file
    write_to_index_file([]),                                         % writing empty list to index.cfg
    os:cmd(lists:concat(["mkdir -p ", get_abs_path("packages")])),   % creating packages directory if not exists
    os:cmd(lists:concat(["rm -rf ", get_abs_path("packages/*")])),     % deleting existing packages
    ok.

%% app_exists/1
%% ====================================================================
%% @doc returns true when the app is already added into the index,
%% false otherwise
-spec app_exists(Link) -> Result when
    Link :: string(),
    Result  :: {ok, Status}
             | {error, Reason},
    Status  :: true
             | false,
    Reason  :: string().
%% ====================================================================
app_exists(Link) ->
    case file:consult(get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            {ok, app_exists(Link, Existing_apps)};
        _ ->
            {error, "please run init before running other epax commands!"}
    end.

%% checkout_repo_and_add_to_index/1
%% ====================================================================
%% @doc checks out the repo in the packages folder and adds details to
%% index
-spec checkout_repo_and_add_to_index(Link) -> ok when
    Link :: string().
%% ====================================================================
checkout_repo_and_add_to_index(Link) ->
    case file:consult(get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            case clone_app(Link) of
                {ok, Newapp} ->
                    write_to_index_file([Newapp|Existing_apps]);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, "please run init before running other epax commands!"}
    end.

%% remove_from_index/1
%% ====================================================================
%% @doc deletes app from the index
-spec remove_from_index(Appname) -> ok when
    Appname :: atom().
%% ====================================================================
remove_from_index(Appname) ->
    case file:consult(get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            write_to_index_file(lists:keydelete(Appname, 1, Existing_apps)),
            run_in_dir(get_abs_path("packages"), lists:concat(["rm -rf ", atom_to_list(Appname)])),
            ok;
        {error, _} ->
            {error, "please run init before running other epax commands!"}
    end.

%% get_applist/0
%% ====================================================================
%% @doc returns list of application added into the index
-spec get_applist() -> ok.
%% ====================================================================
get_applist() ->
    case file:consult(get_abs_path("index.cfg")) of
        {ok, [Existing_apps]} ->
            lists:map(fun(App) -> element(1, App) end, Existing_apps);
        {error, _} ->
            {error, "please run init before running other epax commands!"}
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================
get_abs_path(Location) ->
    {ok, [[Home]]} = init:get_argument(home),               % getting the home directory location
    lists:concat([Home, '/.epax/', Location]).              % getting absolute path to index.cfg

write_to_index_file(Data) ->
    file:write_file(get_abs_path("index.cfg"), io_lib:fwrite("~p.\n",[Data])).

app_exists(_Link, []) ->
    false;
app_exists(Link, [H|_]) when element(2, H) == Link ->
    true;
app_exists(Link, [_|Rest]) ->
    app_exists(Link, Rest).

clone_app(Link) ->
    Path = get_abs_path("packages/temp"),
    os:cmd(lists:concat(["git clone ", Link, " ", Path])),   % cloning the repo in the temp folder
    case filelib:is_dir(Path) of
        true ->
            get_app_info(Link, Path);
        false ->
            {error, "unable to clone repo!"}
    end.

get_app_info(Link, Path) ->
    case find_publisher(Path) of
        {ok, {Appname, Description, Author}} ->
            Tags = collect_tags(Path),
            Branches = collect_branches(Path),
            os:cmd(lists:concat(["mv ", Path, " ", get_abs_path("packages/"), Appname])),
            {ok, {Appname, Link, [{description, Description},
                                  {publisher, Author},
                                  {tags, Tags},
                                  {branches, Branches}]}};
        {error, Reason} ->
            os:cmd(lists:concat(["rm -rf ", Path])),
            {error, Reason}
    end.

find_publisher(Path) ->
    Src_folder = lists:concat([Path, "/src"]),
    {ok, Files} = file:list_dir(Src_folder),
    case get_appfile_name(Files) of
        {ok, File} ->
            App_file_loc = lists:concat([Src_folder, "/", File]),
            {ok, [Info]} = file:consult(App_file_loc),
            Appname = element(2, Info),
            Description = find_key(description, element(3, Info)),
            Author = find_key(author, element(3, Info)),
            {ok, {Appname, Description, Author}};
        {error, Reason} ->
            {error, Reason}
    end.

find_key(Key, List) ->
    case  lists:keyfind(Key, 1, List) of
        {Key, Value} ->
            Value;
        false ->
            ""
    end.

get_appfile_name([]) ->
    {error, "app.src file does not exist"};
get_appfile_name([File|Rest]) ->
    case re:run(File, ".*\.app\.src", [caseless]) of
        {match, _} ->
            {ok, File};
        _ ->
            get_appfile_name(Rest)
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
    [_|List_branches] = re:split(Ret, "\n"),
    lists:foldl(fun(Branch, Acc) ->
            Full_branch = binary_to_list(Branch),
            case Full_branch of
                "" ->
                    Acc;
                _ ->
                    {match,[{Loc, Len}]} = re:run(Full_branch, "origin/"),
                    [lists:nthtail(Loc+Len, Full_branch)|Acc]
            end
        end,
        [],
        List_branches).

run_in_dir(Path, Cmd) ->
    os:cmd(lists:concat(["cd ", Path, " && ", Cmd])).
