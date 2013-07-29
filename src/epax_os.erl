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
-module(epax_os).
-export([get_abs_path/1,
         mkdir/1,
         copy_folder/2,
         touch/1,
         rmdir/1]).


%%============================================================================
%% API
%%============================================================================

%% get_abs_path/1
%% ====================================================================
%% @doc returns absolute path to the given location which is relative to
%% .epax folder
-spec get_abs_path(Location) -> Result when
    Location :: string(),
    Result   :: string().
%% ====================================================================
get_abs_path(Location) ->
    case init:get_argument(home) of
        {ok, [[Home]]} ->
            append_loc(Home, case Location of
                "" ->
                    "/.epax";
                _ ->
                    lists:concat(["/.epax/", Location])
            end);
        error ->
            throw("cannot find the location to home folder")
    end.

%% mkdir/1
%% ====================================================================
%% @doc make the directory if it does not already exist. Also makes parent
%% directories as needed.
-spec mkdir(Path) -> Result when
    Path   :: list(),
    Result :: ok
            | {error, Reason},
    Reason :: term().
%% ====================================================================
mkdir(Path) ->
    case os:type() of
        {unix, _} ->
            Cmd = lists:concat(["mkdir -p ", Path]);
        {win32, _} ->
            Cmd = lists:concat(["mkdir ", Path])
    end,
    cmd(Cmd).

%% copy_folder/2
%% ====================================================================
%% @doc copies the content of one folder to another, creates the intermediate
%% directories if required
-spec copy_folder(From, To) -> ok when
    From :: list(),
    To   :: list().
%% ====================================================================
copy_folder(From, To) ->
    mkdir(To),
    case os:type() of
        {unix, _} ->
            Cmd = lists:concat(["cp -ir ", From, " ", To]);
        {win32, _} ->
            Cmd = lists:concat(["xcopy ", From, " ", To, " /s/e"])
    end,
    cmd(Cmd).

%% touch/1
%% ====================================================================
%% @doc creates an empty file
-spec touch(Path) -> ok when
    Path :: list().
%% ====================================================================
touch(Path) ->
    case os:type() of
        {unix, _} ->
            Cmd = lists:concat(["touch ", Path]);
        {win32, _} ->
            Cmd = lists:concat(["echo $null > ", Path])
    end,
    cmd(Cmd).

%% rmdir/1
%% ====================================================================
%% @doc deletes an empty file
-spec rmdir(Path) -> ok when
    Path :: list().
%% ====================================================================
rmdir(Path) ->
    case os:type() of
        {unix, _} ->
            Cmd = lists:concat(["rm -rf ", Path]);
        {win32, _} ->
            Cmd = lists:concat(["rmdir /s/q ", Path])
    end,
    cmd(Cmd).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
append_loc(Home, Loc) ->
    case os:type() of
        {unix, _} ->
            lists:append(Home, Loc);
        {win32, _} ->
            lists:append(Home, re:replace(Loc, "/", "\\", [global, {return, list}]))
    end.

cmd(Cmd) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status]),
    loop(Port, []).
    
loop(Port, Data) ->
    receive
        {Port, {data, NewData}} ->
            loop(Port, Data ++ NewData);
        {Port, {exit_status, 0}} ->
            {ok, Data};
        {Port, {exit_status, Reason}} ->
            {error, Reason}
    end.
