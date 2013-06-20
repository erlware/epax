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
-module(epax_os_tests).
-include_lib("eunit/include/eunit.hrl").

get_abs_path_test_() ->
    {foreach,
    fun() -> meck:new([]) end,
    fun(_) -> meck:unload([]) end,
    [{"test for get_abs_path function",
    fun() ->
        {ok, [[Home]]} = init:get_argument(home),
        ?assertEqual(filename:join([Home, ".epax"]), epax_os:get_abs_path(""))
    end},
    {"test for get_abs_path function for dir",
    fun() ->
        {ok, [[Home]]} = init:get_argument(home),
        ?assertEqual(filename:join([Home, ".epax/aman"]), epax_os:get_abs_path("aman"))
    end}]}.
