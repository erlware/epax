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
%%%

-module(epax_com).
-export([format/1,
         format/2,
         console/2,
         success/1,
         success/2,
         error/2,
         error/3,
         abort/2,
         abort/3,
         get_appfile_content/1]).


%%============================================================================
%% API
%%============================================================================

%% format/1
%% ====================================================================
%% @doc returns the flattened string
-spec format(Deeplist) -> Result when
    Deeplist :: [term() | Deeplist],
    Result   :: [term()].
%% ====================================================================
format(Deeplist) ->
    lists:flatten(Deeplist).

%% format/2
%% ====================================================================
%% @doc returns the formatted string
-spec format(String, Args) -> Result when
    String :: string(),
    Args   :: [term()],
    Result :: [char() | Result].
%% ====================================================================
format(String, Args) ->
    lists:flatten(io_lib:format(String, Args)).

%% console/2
%% ====================================================================
%% @doc prints output on terminal
-spec console(Message, Args) -> ok when
    Message :: string(),
    Args    :: [term()].
%% ====================================================================
console(Message, Args) ->
    io:format(Message, Args).

%% success/1
%% ====================================================================
%% @doc prints successful completion message on terminal
-spec success(Message) -> ok when
    Message :: string().
%% ====================================================================
success(Message) ->
    io:format("=> Success~n~s~n", [Message]).

%% success/2
%% ====================================================================
%% @doc prints successful completion message on terminal
-spec success(Message, Args) -> ok when
    Message :: string(),
    Args    :: [term()].
%% ====================================================================
success(Message, Args) ->
    epax_com:success(epax_com:format(Message, Args)).

%% error/2
%% ====================================================================
%% @doc prints error on terminal
-spec error(Reason, Conclusion) -> ok when
    Reason     :: term(),
    Conclusion :: string().
%% ====================================================================
error(Reason, Conclusion) ->
    io:format("** Error occurred: "),
    io:format(Reason),
    io:format("~n~s~n", [Conclusion]).

%% error/3
%% ====================================================================
%% @doc prints error on terminal
-spec error(Reason, Conclusion, Args) -> ok when
    Reason     :: term(),
    Conclusion :: string(),
    Args       :: [term()].
%% ====================================================================
error(Reason, Conclusion, Args) ->
    epax_com:error(Reason, epax_com:format(Conclusion, Args)).

%% abort/2
%% ====================================================================
%% @doc aborts the process and throws error
-spec abort(Reason, Conclusion) -> ok when
    Reason     :: term(),
    Conclusion :: string().
%% ====================================================================
abort(Reason, Conclusion) ->
    io:format("~s~n", [Conclusion]),
    throw(Reason).

%% abort/3
%% ====================================================================
%% @doc aborts the process and throws error
-spec abort(Reason, String, Args) -> ok when
    Reason :: term(),
    String :: string(),
    Args   :: [term()].
%% ====================================================================
abort(Reason, String, Args) ->
    io:format("~s~n", [epax_com:format(String, Args)]),
    throw(Reason).

%% get_appfile_content/1
%% ====================================================================
%% @doc finds the .app or .app.src file in the application folder and
%% returns the content of the file
-spec get_appfile_content(Info) -> Result when
    Info    :: atom()
             | string(),
    Result  :: {ok, [Content]}
             | {error, Reason},
    Content :: term(),
    Reason  :: term().
%% ====================================================================
get_appfile_content(Info) ->
    BaseFolder = case is_atom(Info) of
        true ->
            filename:join(epax_os:get_abs_path("packages"), atom_to_list(Info));
        false ->
            Info
    end,
    % trying to find .app file in ebin directory
    EbinDir = filename:join(BaseFolder, "ebin"),
    case filelib:wildcard("*.app", EbinDir) of
        [File] ->
            file:consult(filename:join(EbinDir, File));
        [] ->
            % trying to find .app.src file in src folder
            SrcDir = filename:join(BaseFolder, "src"),
            case filelib:wildcard("*.app.src", SrcDir) of
                [File] ->
                    file:consult(filename:join(SrcDir, File));
                [] ->
                    {error, ".app or .app.src file not found"};
                _ ->
                    {error, "More than one .app.src file in src folder"}
            end;
        _ ->
            {error, "More than one .app file in ebin folder"}
    end.
