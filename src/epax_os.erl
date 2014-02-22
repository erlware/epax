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
%%% @doc handles os specific operations
%%%

-module(epax_os).
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
%% @doc returns absolute path to the given location which is relative
%% to ~/.epax folder
-spec get_abs_path(Location) -> Result | no_return() when
    Location :: string(),
    Result   :: string().
%% ====================================================================
get_abs_path(Location) ->
    case init:get_argument(home) of
        {ok, [[Home]]} ->
            case filename:pathtype(Location) of
                absolute ->
                    Location;
                _ ->
                    filename:join([Home, ".epax", Location])
            end;
        error ->
            epax_com:abort(home_folder_unavailable, "Cannot find the path to home folder")
    end.

%% mkdir/1
%% ====================================================================
%% @doc create the directory if it does not exist already. Also makes
%% parent directories as needed.
-spec mkdir(Path) -> ok | {error, Reason} when
    Path   :: string(),
    Reason :: term().
%% ====================================================================
mkdir(Path) ->
    filelib:ensure_dir(Path),
    case filelib:is_dir(Path) of
        false ->
            case file:make_dir(Path) of
                ok ->
                    ok;
                {error, eexist} ->
                    ok;
                {error, Reason} ->
                    epax_com:abort(Reason, "Cannot create directory ~s", [Path])
            end;
        true ->
            ok
    end.

%% copy_folder/2
%% ====================================================================
%% @doc copies one folder to another, creates the intermediate
%% directories if required
-spec copy_folder(From, To) -> ok when
    From :: string(),
    To   :: string().
%% ====================================================================
copy_folder(From, To) ->
    mkdir(To),
    case os:type() of
        {unix, _} ->
            Cmd = epax_com:format(["cp -fr ", From, " ", To]);
        {win32, _} ->
            Cmd = epax_com:format(["xcopy ", From, " ", To, " /s/e"])
    end,
    case cmd(Cmd) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            epax_com:abort(Reason, "Cannot copy ~s to ~s", [From, To])
    end.

%% mv_folder/2
%% ====================================================================
%% @doc moves content of one folder to another (renames the folder)
-spec mv_folder(From, To) -> ok when
    From :: string(),
    To   :: string().
%% ====================================================================
mv_folder(From, To) ->
    case os:type() of
        {unix, _} ->
            Cmd = epax_com:format(["mv ", From, " ", To]);
        {win32, _} ->
            Cmd = epax_com:format(["move ", From, " ", To])
    end,
    case cmd(Cmd) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            epax_com:abort(Reason, "Cannot move ~s to ~s", [From, To])
    end.

%% touch/1
%% ====================================================================
%% @doc creates an empty file
-spec touch(Path) -> ok when
    Path :: string().
%% ====================================================================
touch(Path) ->
    case file:write_file(Path, []) of
        ok ->
            ok;
        {error, Reason} ->
            epax_com:abort(Reason, "Cannot touch ~s", [Path])
    end.

%% rmdir/1
%% ====================================================================
%% @doc deletes the given directory
-spec rmdir(Path) -> ok when
    Path :: string().
%% ====================================================================
rmdir(Path) ->
    case os:type() of
        {unix, _} ->
            Cmd = epax_com:format(["rm -rf ", Path]);
        {win32, _} ->
            Cmd = epax_com:format(["rmdir /s/q ", Path])
    end,
    case cmd(Cmd) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            epax_com:abort(Reason, "Cannot delete directory ~s", [Path])
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
    NewCmd = epax_com:format(["cd ", Path, " && ", Cmd]),
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
