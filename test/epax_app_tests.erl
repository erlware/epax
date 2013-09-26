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
%%%

-module(epax_app_tests).
-include_lib("eunit/include/eunit.hrl").

init_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_index, epax_os]) end,
    fun(_) -> meck:unload() end,
    [{"test for init of epax",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun("") -> "loc" end),
        meck:expect(epax_os, mkdir, fun("loc") -> {ok, done} end),
        meck:expect(epax_index, init, fun() -> ok end),
        meck:expect(epax_com, success, fun("Index initialized successfully") -> ok end),
        ?assertEqual(ok, epax_app:init()),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, [""])),
        ?assertEqual(1, meck:num_calls(epax_os, mkdir, ["loc"])),
        ?assertEqual(1, meck:num_calls(epax_index, init, [])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["Index initialized successfully"])),
        meck:validate([epax_com, epax_index, epax_os])
    end},
    {"test for initialization of epax when mkdir returns error",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun("") -> "loc" end),
        meck:expect(epax_os, mkdir, fun("loc") -> throw("error") end),
        ?assertThrow("error", epax_app:init()),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, [""])),
        ?assertEqual(1, meck:num_calls(epax_os, mkdir, ["loc"])),
        ?assertNot(meck:called(epax_index, init, [])),
        meck:validate([epax_com, epax_index, epax_os])
    end}]}.

add_app_test_() ->
    {foreach,
    fun() -> meck:new([epax_index, epax_com]) end,
    fun(_) -> meck:unload([epax_index, epax_com]) end,
    [{"test add_app",
    fun() ->
        meck:expect(epax_index, app_exists, fun("link") -> {ok, false} end),
        meck:expect(epax_index, checkout_repo_and_add_to_index, fun("link", []) -> {ok, appname} end),
        meck:expect(epax_com, success, fun("Added ~s to index", [appname]) -> ok end),
        ?assertEqual(ok, epax_app:add_app("link", [])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_index, checkout_repo_and_add_to_index, ["link", []])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["Added ~s to index", [appname]]))
    end},
    {"test add_app while error",
    fun() ->
        meck:expect(epax_index, app_exists, fun("link") -> {ok, false} end),
        meck:expect(epax_index, checkout_repo_and_add_to_index, fun("link", []) -> {error, "error"} end),
        meck:expect(epax_com, error, fun("error", "Unable to add to index") -> ok end),
        ?assertEqual(ok, epax_app:add_app("link", [])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_index, checkout_repo_and_add_to_index, ["link", []])),
        ?assertEqual(1, meck:num_calls(epax_com, error, ["error", "Unable to add to index"]))
    end},
    {"test add_app when app already exists",
    fun() ->
        meck:expect(epax_index, app_exists, fun("link") -> {ok, appname} end),
        meck:expect(epax_com, error, fun(already_added, "~s is already added", [appname]) -> ok end),
        ?assertEqual(ok, epax_app:add_app("link", [])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_com, error, [already_added, "~s is already added", [appname]])),
        ?assertNot(meck:called(epax_index, checkout_repo_and_add_to_index, ["link"]))
    end},
    {"test add_app when app_exist returns error",
    fun() ->
        meck:expect(epax_index, app_exists, fun("link") -> {error, "error"} end),
        meck:expect(epax_com, error, fun("error","Unable to add to index") -> ok end),
        ?assertEqual(ok, epax_app:add_app("link", [])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_com, error, ["error","Unable to add to index"])),
        ?assertNot(meck:called(epax_index, checkout_repo_and_add_to_index, ["link"]))
    end}]}.

remove_app_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_index]) end,
    fun(_) -> meck:unload([epax_com, epax_index]) end,
    [{"test remove_app",
    fun() ->
        meck:expect(epax_index, remove_from_index, fun(appname) -> ok end),
        meck:expect(epax_com, success, fun("~s is removed successfully",[appname]) -> ok end),
        ?assertEqual(ok, epax_app:remove_app(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, remove_from_index, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s is removed successfully",[appname]]))
    end},
    {"test remove_app while error",
    fun() ->
        meck:expect(epax_index, remove_from_index, fun(appname) -> {error, "error"} end),
        meck:expect(epax_com, error, fun("error", "~s cannot be removed from index", [appname]) -> ok end),
        ?assertEqual(ok, epax_app:remove_app(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, remove_from_index, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, error, ["error", "~s cannot be removed from index", [appname]]))
    end}]}.

list_apps_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_index]) end,
    fun(_) -> meck:unload([epax_com, epax_index]) end,
    [{"test list_apps when no application in the list",
    fun() ->
        meck:expect(epax_index, get_applist, fun() -> {ok, []} end),
        meck:expect(epax_com, success, fun("No app is added yet") -> ok end),
        ?assertEqual(ok, epax_app:list_apps()),
        ?assertEqual(1, meck:num_calls(epax_index, get_applist, [])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["No app is added yet"]))
    end},
    {"test list_apps",
    fun() ->
        meck:expect(epax_index, get_applist, fun() -> {ok, [a, b, c]} end),
        meck:expect(epax_com, success, fun("=== Erlang Apps ===~s~n===================",
                                           [["\n",32,32,45,32,[97],"\n",32,32,45,32,"b","\n",32,32,45,32,"c"]]) -> ok end),
        ?assertEqual(ok, epax_app:list_apps()),
        ?assertEqual(1, meck:num_calls(epax_index, get_applist, [])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["=== Erlang Apps ===~s~n===================",
                                                           [["\n",32,32,45,32,[97],"\n",32,32,45,32,"b","\n",32,32,45,32,"c"]]]))
    end},
    {"test list_apps when no application in the list",
    fun() ->
        meck:expect(epax_index, get_applist, fun() -> {error, "error"} end),
        meck:expect(epax_com, error, fun("error", "Cannot retrieve the application list") -> ok end),
        ?assertEqual(ok, epax_app:list_apps()),
        ?assertEqual(1, meck:num_calls(epax_index, get_applist, [])),
        ?assertEqual(1, meck:num_calls(epax_com, error, ["error", "Cannot retrieve the application list"]))
    end}]}.

update_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_index]) end,
    fun(_) -> meck:unload([epax_com, epax_index]) end,
    [{"test for update index",
    fun() ->
        meck:expect(epax_index, update_index, fun() -> ok end),
        meck:expect(epax_com, success, fun("Index updated successfully") -> ok end),
        ?assertEqual(ok, epax_app:update()),
        ?assertEqual(1, meck:num_calls(epax_index, update_index, [])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["Index updated successfully"]))
    end},
    {"test for update index while failed",
    fun() ->
        meck:expect(epax_index, update_index, fun() -> {error, "error"} end),
        meck:expect(epax_com, error, fun("error", "Unable to update index") -> ok end),
        ?assertEqual(ok, epax_app:update()),
        ?assertEqual(1, meck:num_calls(epax_index, update_index, [])),
        ?assertEqual(1, meck:num_calls(epax_com, error, ["error", "Unable to update index"]))
    end}]}.

check_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_index]) end,
    fun(_) -> meck:unload([epax_com, epax_index]) end,
    [{"test for check index",
    fun() ->
        meck:expect(epax_index, check_index, fun() -> ok end),
        meck:expect(epax_com, success, fun("Fixed broken packages") -> ok end),
        ?assertEqual(ok, epax_app:check()),
        ?assertEqual(1, meck:num_calls(epax_index, check_index, [])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["Fixed broken packages"]))
    end},
    {"test for check index while failed",
    fun() ->
        meck:expect(epax_index, check_index, fun() -> {error, "error"} end),
        meck:expect(epax_com, error, fun("error", "Unable to fix broken packages, reinitialized the index") -> ok end),
        ?assertEqual(ok, epax_app:check()),
        ?assertEqual(1, meck:num_calls(epax_index, check_index, [])),
        ?assertEqual(1, meck:num_calls(epax_com, error, ["error", "Unable to fix broken packages, reinitialized the index"]))
    end}]}.

bundle_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_dep]) end,
    fun(_) -> meck:unload([epax_com, epax_dep]) end,
    [{"test for bundle application",
    fun() ->
        meck:expect(epax_dep, bundle, fun(appname) -> ok end),
        meck:expect(epax_com, success, fun("~s bundled successfully", [appname]) -> ok end),
        ?assertEqual(ok, epax_app:bundle(appname)),
        ?assertEqual(1, meck:num_calls(epax_dep, bundle, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s bundled successfully", [appname]]))
    end},
    {"test for bundle application while failed",
    fun() ->
        meck:expect(epax_dep, bundle, fun(appname) -> {error, "error"} end),
        meck:expect(epax_com, error, fun("error", "Unable to bundle ~s", [appname]) -> ok end),
        ?assertEqual(ok, epax_app:bundle(appname)),
        ?assertEqual(1, meck:num_calls(epax_dep, bundle, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, error, ["error", "Unable to bundle ~s", [appname]]))
    end}]}.
