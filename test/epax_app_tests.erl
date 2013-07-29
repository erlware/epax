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
-module(epax_app_tests).
-include_lib("eunit/include/eunit.hrl").

install_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_index, epax_os]) end,
    fun(_) -> meck:unload() end,
    [{"test installation of epax",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun("") -> "loc" end),
        meck:expect(epax_os, mkdir, fun("loc") -> ok end),
        meck:expect(epax_index, init, fun() -> ok end),
        ?assertEqual(ok, epax_app:install()),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, [""])),
        ?assertEqual(1, meck:num_calls(epax_os, mkdir, ["loc"])),
        ?assertEqual(1, meck:num_calls(epax_index, init, [])),
        meck:validate([epax_com, epax_index, epax_os])
    end},
    {"test failed installation of epax when mkdir returns error",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun("") -> "loc" end),
        meck:expect(epax_os, mkdir, fun("loc") -> {error, "error"} end),
        meck:expect(epax_com, print_error, fun("error", _) -> ok end),
        ?assertEqual(ok, epax_app:install()),
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
        meck:expect(epax_index, checkout_repo_and_add_to_index, fun("link") -> {ok, appname} end),
        meck:expect(epax_com, print_success, fun("added appname to index.") -> ok end),
        ?assertEqual(ok, epax_app:add_app("link")),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_index, checkout_repo_and_add_to_index, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_com, print_success, ["added appname to index."]))
    end},
    {"test add_app while error",
    fun() ->
        meck:expect(epax_index, app_exists, fun("link") -> {ok, false} end),
        meck:expect(epax_index, checkout_repo_and_add_to_index, fun("link") -> {error, "error"} end),
        meck:expect(epax_com, print_error, fun("error", "unable to add to index!") -> ok end),
        ?assertEqual(ok, epax_app:add_app("link")),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_index, checkout_repo_and_add_to_index, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_com, print_error, ["error", "unable to add to index!"]))
    end},
    {"test add_app when app already exists",
    fun() ->
        meck:expect(epax_index, app_exists, fun("link") -> {ok, appname} end),
        meck:expect(epax_com, print_error, fun("appname is already added!") -> ok end),
        ?assertEqual(ok, epax_app:add_app("link")),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_com, print_error, ["appname is already added!"])),
        ?assertNot(meck:called(epax_index, checkout_repo_and_add_to_index, ["link"]))
    end},
    {"test add_app when app_exist returns error",
    fun() ->
        meck:expect(epax_index, app_exists, fun("link") -> {error, "error"} end),
        meck:expect(epax_com, print_error, fun("error", "unable to add to index!") -> ok end),
        ?assertEqual(ok, epax_app:add_app("link")),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, ["link"])),
        ?assertEqual(1, meck:num_calls(epax_com, print_error, ["error", "unable to add to index!"])),
        ?assertNot(meck:called(epax_index, checkout_repo_and_add_to_index, ["link"]))
    end}]}.

remove_app_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_index]) end,
    fun(_) -> meck:unload([epax_com, epax_index]) end,
    [{"test remove_app",
    fun() ->
        meck:expect(epax_index, remove_from_index, fun(appname) -> ok end),
        meck:expect(epax_com, print_success, fun("appname is removed successfully.") -> ok end),
        ?assertEqual(ok, epax_app:remove_app(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, remove_from_index, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, print_success, ["appname is removed successfully."]))
    end},
    {"test remove_app while error",
    fun() ->
        meck:expect(epax_index, remove_from_index, fun(appname) -> {error, "error"} end),
        meck:expect(epax_com, print_error, fun("error", "appname cannot be removed!") -> ok end),
        ?assertEqual(ok, epax_app:remove_app(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, remove_from_index, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, print_error, ["error", "appname cannot be removed!"]))
    end}]}.

list_apps_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_index]) end,
    fun(_) -> meck:unload([epax_com, epax_index]) end,
    [{"test list_apps when no application in the list",
    fun() ->
        meck:expect(epax_index, get_applist, fun() -> {ok, []} end),
        meck:expect(epax_com, print_success, fun("no app is added yet!") -> ok end),
        ?assertEqual(ok, epax_app:list_apps()),
        ?assertEqual(1, meck:num_calls(epax_index, get_applist, [])),
        ?assertEqual(1, meck:num_calls(epax_com, print_success, ["no app is added yet!"]))
    end},
    {"test list_apps",
    fun() ->
        meck:expect(epax_index, get_applist, fun() -> {ok, [a, b, c]} end),
        meck:expect(epax_com, print_success, fun("*** Erlang Apps ***") -> ok end),
        ?assertEqual(ok, epax_app:list_apps()),
        ?assertEqual(1, meck:num_calls(epax_index, get_applist, [])),
        ?assertEqual(1, meck:num_calls(epax_com, print_success, ["*** Erlang Apps ***"]))
    end},
    {"test list_apps when no application in the list",
    fun() ->
        meck:expect(epax_index, get_applist, fun() -> {error, "error"} end),
        meck:expect(epax_com, print_error, fun("error", "cannot retrieve the application list!") -> ok end),
        ?assertEqual(ok, epax_app:list_apps()),
        ?assertEqual(1, meck:num_calls(epax_index, get_applist, [])),
        ?assertEqual(1, meck:num_calls(epax_com, print_error, ["error", "cannot retrieve the application list!"]))
    end}]}.

update_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_index]) end,
    fun(_) -> meck:unload([epax_com, epax_index]) end,
    [{"test for update index",
    fun() ->
        meck:expect(epax_index, update_index, fun() -> ok end),
        meck:expect(epax_com, print_success, fun("Index updated successfully.") -> ok end),
        ?assertEqual(ok, epax_app:update()),
        ?assertEqual(1, meck:num_calls(epax_index, update_index, [])),
        ?assertEqual(1, meck:num_calls(epax_com, print_success, ["Index updated successfully."]))
    end},
    {"test for update index while failed",
    fun() ->
        meck:expect(epax_index, update_index, fun() -> {error, "error"} end),
        meck:expect(epax_com, print_error, fun("error", "unable to update index!") -> ok end),
        ?assertEqual(ok, epax_app:update()),
        ?assertEqual(1, meck:num_calls(epax_index, update_index, [])),
        ?assertEqual(1, meck:num_calls(epax_com, print_error, ["error", "unable to update index!"]))
    end}]}.

bundle_test_() ->
    {foreach,
    fun() -> meck:new([epax_com, epax_dep]) end,
    fun(_) -> meck:unload([epax_com, epax_dep]) end,
    [{"test for bundle application",
    fun() ->
        meck:expect(epax_dep, bundle, fun(appname) -> ok end),
        meck:expect(epax_com, print_success, fun("appname bundled successfully.") -> ok end),
        ?assertEqual(ok, epax_app:bundle(appname)),
        ?assertEqual(1, meck:num_calls(epax_dep, bundle, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, print_success, ["appname bundled successfully."]))
    end},
    {"test for bundle application while failed",
    fun() ->
        meck:expect(epax_dep, bundle, fun(appname) -> {error, "error"} end),
        meck:expect(epax_com, print_error, fun("error", "unable to bundle appname!") -> ok end),
        ?assertEqual(ok, epax_app:bundle(appname)),
        ?assertEqual(1, meck:num_calls(epax_dep, bundle, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, print_error, ["error",  "unable to bundle appname!"]))
    end}]}.
