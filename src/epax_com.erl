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
    io:format("** error occurred: ~s!~n", [Message]).

%% print_error/2
%% ====================================================================
%% @doc prints error on terminal
-spec print_error(Reason, Conclusion) -> ok when
    Reason     :: term(),
    Conclusion :: list().
%% ====================================================================
print_error(Reason, Conclusion) ->
    io:format("** error occurred: "),
    io:format(Reason),
    io:format("~n~s!~n", [Conclusion]).

%% print_success/1
%% ====================================================================
%% @doc prints successful completion message on terminal
-spec print_success(Message) -> ok when
    Message :: string().
%% ====================================================================
print_success(Message) ->
    io:format("=> success~n~s~n", [Message]).

%% get_appfile_content/1
%% ====================================================================
%% @doc finds the .app or .app.src file in the application folder and returns
%% the content of the file
-spec get_appfile_content(Info) -> Result when
    Info    :: atom()
             | string(),
    Result  :: {ok, [Content]}
             | {error, Reason},
    Content :: term(),
    Reason  :: term().
%% ====================================================================
get_appfile_content(Info) ->
    Base_folder = case is_atom(Info) of
        true ->
            filename:join(epax_os:get_abs_path("packages"), Info);
        false ->
            Info
    end,
    EbinDir = filename:join(Base_folder, "ebin"),
    case filelib:wildcard("*.app", EbinDir) of
        [File] ->
            file:consult(filename:join(EbinDir, File));
        [] ->
            SrcDir = filename:join(Base_folder, "src"),
            case filelib:wildcard("*.app.src", SrcDir) of
                [File] ->
                    file:consult(filename:join(SrcDir, File));
                [] ->
                    {error, "app or app.src file not found"};
                _ ->
                    {error, "more than one .app.src file in src folder"}
            end;
        _ ->
            {error, "more than one .app file in ebin folder"}
    end.
