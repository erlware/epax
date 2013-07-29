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
-export([print_error/1,
         print_error/2,
         print_success/1,
         get_appfile_content/1]).


%%============================================================================
%% API
%%============================================================================

%% print_error/1
%% ====================================================================
%% @doc prints error on terminal
-spec print_error(Message) -> ok when
    Message :: string().
%% ====================================================================
print_error(Message) ->
    io:format("Error! ~s~n", [Message]).

%% print_error/2
%% ====================================================================
%% @doc prints error on terminal
-spec print_error(Reason, Conclusion) -> ok when
    Reason     :: term(),
    Conclusion :: list().
%% ====================================================================
print_error(Reason, Conclusion) ->
    io:format("Error occurred! "),
    io:format(Reason),
    io:format("~n~s~n", [Conclusion]).

%% print_success/1
%% ====================================================================
%% @doc prints successful completion message on terminal
-spec print_success(Message) -> ok when
    Message :: string().
%% ====================================================================
print_success(Message) ->
    io:format("Success~n~s~n", [Message]).

%% get_appfile_content/1
%% ====================================================================
%% @doc find the .app or .app.src file in the application and returns
%% abosolute path to the file
-spec get_appfile_content(Appname) -> Result when
    Appname :: atom(),
    Result  :: {ok, Content}
             | {error, Reason},
    Content :: string(),
    Reason  :: term().
%% ====================================================================
get_appfile_content(Info) ->
    Base_folder = case is_atom(Info) of
        true ->
            lists:concat(["packages/", atom_to_list(Info)]);
        false ->
            Info
    end,
    case find_file_in_folder(".*\.app$", lists:concat([Base_folder, "/ebin"])) of
        {ok, Location} ->
            file:consult(Location);
        {error, _} ->
            case find_file_in_folder(".*\.app\.src$", lists:concat([Base_folder, "/src"])) of
                {ok, Location} ->
                    file:consult(Location);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
find_file_in_folder(RE, Folder) ->
    case file:list_dir(epax_os:get_abs_path(Folder)) of
        {ok, Files} ->
            case get_appfile_name(Files, RE) of
                {ok, File} ->
                    {ok, epax_os:get_abs_path(lists:concat([Folder, "/", File]))};
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
