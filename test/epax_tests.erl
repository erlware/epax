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
-module(epax_tests).
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
    {foreach,
    fun() -> meck:new(epax_app) end,
    fun(_) -> meck:unload(epax_app) end,
    [{"test for install command",
    fun() ->
        meck:expect(epax_app, install, fun() -> ok end),
        ?assertEqual(ok, epax:main(["install"])),
        ?assertEqual(1, meck:num_calls(epax_app, install, [])),
        ?assert(meck:validate(epax_app))
    end},
    {"test for add app command",
    fun() ->
        meck:expect(epax_app, add_app, fun("link") -> ok end),
        ?assertEqual(ok, epax:main(["add", "link"])),
        ?assertEqual(1, meck:num_calls(epax_app, add_app, ["link"])),
        ?assert(meck:validate(epax_app))
    end},
    {"test for list command",
    fun() ->
        meck:expect(epax_app, list_apps, fun() -> ok end),
        ?assertEqual(ok, epax:main(["list"])),
        ?assertEqual(1, meck:num_calls(epax_app, list_apps, [])),
        ?assert(meck:validate(epax_app))
    end},
    {"test for remove command",
    fun() ->
        meck:expect(epax_app, remove_app, fun(appname) -> ok end),
        ?assertEqual(ok, epax:main(["remove", "appname"])),
        ?assertEqual(1, meck:num_calls(epax_app, remove_app, [appname])),
        ?assert(meck:validate(epax_app))
    end},
    {"test for update command",
    fun() ->
        meck:expect(epax_app, update, fun() -> ok end),
        ?assertEqual(ok, epax:main(["update"])),
        ?assertEqual(1, meck:num_calls(epax_app, update, [])),
        ?assert(meck:validate(epax_app))
    end},
    {"test for bundle command",
    fun() ->
        meck:expect(epax_app, bundle, fun(appname) -> ok end),
        ?assertEqual(ok, epax:main(["bundle", "appname"])),
        ?assertEqual(1, meck:num_calls(epax_app, bundle, [appname])),
        ?assert(meck:validate(epax_app))
    end},
    {"test for invalid commands",
    fun() ->
        ?assertEqual(ok, epax:main(["random"])),
        ?assertEqual(ok, epax:main([])),
        ?assertEqual(ok, epax:main(["some", "cmd"])),
        ?assertError(undef, epax:main(["install"])),
        ?assertError(undef, epax:main(["add", "link"])),
        ?assertError(undef, epax:main(["list"])),
        ?assertError(undef, epax:main(["remove", "appname"])),
        ?assertError(undef, epax:main(["update"])),
        ?assertError(undef, epax:main(["bundle", "appname"]))
    end}]}.
