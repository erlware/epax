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
-include("epax.hrl").
-export([get_abs_path/1,
         mkdir/1,
         copy_folder/2,
         mv_folder/2,
         touch/1,
         rmdir/1,
         run_in_dir/2]).


%%============================================================================
%% API
%%============================================================================

%% get_abs_path/1
%% ====================================================================
%% @doc returns absolute path to the given location which is relative to
%% .epax folder
-spec get_abs_path(Location) -> Result | no_return() when
    Location :: string(),
    Result   :: string().
%% ====================================================================
get_abs_path(Location) ->
    case init:get_argument(home) of
        {ok, [[Home]]} ->
            case string:str(Location, Home) of
                0 ->
                    filename:join([Home, ".epax", Location]);
                _ ->
                    Location
            end;
        error ->
            throw("cannot find the path to home folder")
    end.

%% mkdir/1
%% ====================================================================
%% @doc make the directory if it does not exist already. Also makes parent
%% directories as needed.
-spec mkdir(Path) -> ok | no_return() when
    Path   :: string().
%% ====================================================================
mkdir(Path) ->
    case os:type() of
        {unix, _} ->
            Cmd = lists:concat(["mkdir -p ", Path]);
        {win32, _} ->
            Cmd = lists:concat(["mkdir ", Path])
    end,
    case cmd(Cmd) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ABORT(Reason, "cannot create directory ~s", [Path])
    end.

%% copy_folder/2
%% ====================================================================
%% @doc copies one folder to another, creates the intermediate
%% directories if required
-spec copy_folder(From, To) -> ok when
    From   :: string(),
    To     :: string().
%% ====================================================================
copy_folder(From, To) ->
    mkdir(To),
    case os:type() of
        {unix, _} ->
            Cmd = lists:concat(["cp -ir ", From, " ", To]);
        {win32, _} ->
            Cmd = lists:concat(["xcopy ", From, " ", To, " /s/e"])
    end,
    case cmd(Cmd) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ABORT(Reason, "cannot copy ~s to ~s", [From, To])
    end.

%% mv_folder/2
%% ====================================================================
%% @doc moves content of one folder to another (renames the folder)
-spec mv_folder(From, To) -> ok when
    From   :: string(),
    To     :: string().
%% ====================================================================
mv_folder(From, To) ->
    case os:type() of
        {unix, _} ->
            Cmd = lists:concat(["mv ", From, " ", To]);
        {win32, _} ->
            Cmd = lists:concat(["move ", From, " ", To])
    end,
    case cmd(Cmd) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ABORT(Reason, "cannot move ~s to ~s", [From, To])
    end.

%% touch/1
%% ====================================================================
%% @doc creates an empty file
-spec touch(Path) -> ok when
    Path   :: string().
%% ====================================================================
touch(Path) ->
    case os:type() of
        {unix, _} ->
            Cmd = lists:concat(["touch ", Path]);
        {win32, _} ->
            Cmd = lists:concat(["echo $null > ", Path])
    end,
    case cmd(Cmd) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ABORT(Reason, "cannot touch ~s", [Path])
    end.

%% rmdir/1
%% ====================================================================
%% @doc deletes the given directory
-spec rmdir(Path) -> ok when
    Path   :: string().
%% ====================================================================
rmdir(Path) ->
    case os:type() of
        {unix, _} ->
            Cmd = lists:concat(["rm -rf ", Path]);
        {win32, _} ->
            Cmd = lists:concat(["rmdir /s/q ", Path])
    end,
    case cmd(Cmd) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ABORT(Reason, "cannot delte directory ~s", [Path])
    end.

%% run_in_dir/2
%% ====================================================================
%% @doc runs a command in a directory. This command should only be used
%% to run external application like git.
-spec run_in_dir(Path, Cmd) -> Result when
    Path   :: string(),
    Cmd    :: string(),
    Result :: term().
%% ====================================================================
run_in_dir(Path, Cmd) ->
    NewCmd = lists:concat(["cd ", Path, " && ", Cmd]),
    os:cmd(NewCmd).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

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
