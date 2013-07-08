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
%%% @doc common functions used in epax modules
-module(epax_com).
-export([get_abs_path/1,
         get_appfile_loc/1]).


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
    {ok, [[Home]]} = init:get_argument(home),               % getting the home directory location
    lists:concat([Home, '/.epax/', Location]).              % getting absolute path to index.cfg

%% get_appfile_loc/1
%% ====================================================================
%% @doc find the .app or .app.src file in the application and returns
%% abosolute path to the file
-spec get_appfile_loc(Path_to_app) -> Result when
    Path_to_app :: string(),
    Result      :: string().
%% ====================================================================
get_appfile_loc(Path_to_app) ->
    Ebin = lists:concat([Path_to_app, "/ebin"]),
    case find_file_in_folder(".*\.app", Ebin) of
        {ok, Location} ->
            {ok, Location};
        {error, _} ->
            Src = lists:concat([Path_to_app, "/src"]),
            find_file_in_folder(".*\.app\.src", Src)
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================
find_file_in_folder(RE, Folder) ->
    case file:list_dir(Folder) of
        {ok, Files} ->
            case get_appfile_name(Files, RE) of
                {ok, File} ->
                    {ok, lists:concat([Folder, "/", File])};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_appfile_name([], _RE) ->
    {error, "app.src or .app file does not exist"};
get_appfile_name([File|Rest], RE) ->
    case re:run(File, RE, [caseless]) of
        {match, _} ->
            {ok, File};
        _ ->
            get_appfile_name(Rest, RE)
    end.
