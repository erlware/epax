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
-include("epax.hrl").
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

show_test_() ->
    {foreach,
    fun() -> meck:new([epax_index, epax_com, epax_dep]) end,
    fun(_) -> meck:unload([epax_index, epax_com, epax_dep]) end,
    [{"test for show application",
    fun() ->
        meck:expect(epax_index, get_index_entry, fun(app1) ->
                {ok, #application{name=app1,
                     repo_link="link1",
                     repo_type=git,
                     details=[{publisher, "publisher"}, {tags, ["tag"]}, {description, "description"}]}}
            end),
        meck:expect(epax_dep, find_all_deps_for, fun(app1) -> {ok, [dep1, dep2]} end),
        meck:expect(epax_com, format, fun(X) -> lists:flatten(X) end),
        meck:expect(epax_com, format, fun("~s ~s ~s~s~n", ["=======", app1, "=======",
         "\nStatus: installed\nPublisher: publisher\nLatest Version: tag\nDepends: dep1, dep2\nDescription: description"]) ->
                "formatted"
            end),
        meck:expect(epax_com, success, fun("~s====================", ["formatted"]) -> ok end),

        ?assertEqual(ok, epax_app:show(app1)),
        ?assertEqual(1, meck:num_calls(epax_index, get_index_entry, [app1])),
        ?assertEqual(1, meck:num_calls(epax_dep, find_all_deps_for, [app1])),
        ?assertEqual(1, meck:num_calls(epax_com, format, ["~s ~s ~s~s~n",
            ["=======", app1, "=======", "\nStatus: installed\nPublisher: publisher\nLatest Version: tag\nDepends: dep1, dep2\nDescription: description"]])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s====================", ["formatted"]]))
    end},
    {"test for show application when app is not found in index",
    fun() ->
        meck:expect(epax_index, get_index_entry, fun(appname) -> {error, "error"} end),
        meck:expect(epax_com, error, fun("error", "Unable to locate package ~s", [appname]) -> ok end),

        ?assertEqual(ok, epax_app:show(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, get_index_entry, [appname])),
        ?assertEqual(0, meck:num_calls(epax_dep, find_all_deps_for, [appname])),
        ?assertEqual(0, meck:num_calls(epax_com, format, ["~s ~s ~s~s~n",
            ["=====", appname, "=====", "\nStatus: installed\nPublisher: publisher\nLatest Version: tag\nDepends: dep1, dep2\nDescription: description"]])),
        ?assertEqual(0, meck:num_calls(epax_com, success, ["~s====================", [ok]]))
    end},
    {"test for show application when dependencies are not found",
    fun() ->
        meck:expect(epax_index, get_index_entry, fun(appname) ->
                {ok, #application{name=appname,
                     repo_link="link1",
                     repo_type=git,
                     details=[{publisher, "publisher"}, {tags, ["tag"]}, {description, "description"}]}}
            end),
        meck:expect(epax_dep, find_all_deps_for, fun(appname) -> {error, "error"} end),
        meck:expect(epax_com, format, fun(X) -> lists:flatten(X) end),
        meck:expect(epax_com, format, fun("~s ~s ~s~s~n", ["=====", appname, "=====",
         "\nStatus: installed\nPublisher: publisher\nLatest Version: tag\nDepends: unable to find dependencies\nDescription: description"]) ->
                ok
            end),
        meck:expect(epax_com, success, fun("~s====================", [ok]) -> ok end),

        ?assertEqual(ok, epax_app:show(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, get_index_entry, [appname])),
        ?assertEqual(1, meck:num_calls(epax_dep, find_all_deps_for, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, format, ["~s ~s ~s~s~n",
            ["=====", appname, "=====", "\nStatus: installed\nPublisher: publisher\nLatest Version: tag\nDepends: unable to find dependencies\nDescription: description"]])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s====================", [ok]]))
    end},
    {"test for show application when publisher is unknown",
    fun() ->
        meck:expect(epax_index, get_index_entry, fun(appname) ->
                {ok, #application{name=appname,
                     repo_link="link1",
                     repo_type=git,
                     details=[{publisher, []}, {tags, ["tag"]}, {description, "description"}]}}
            end),
        meck:expect(epax_dep, find_all_deps_for, fun(appname) -> {ok, [dep1, dep2]} end),
        meck:expect(epax_com, format, fun(X) -> lists:flatten(X) end),
        meck:expect(epax_com, format, fun("~s ~s ~s~s~n", ["=====", appname, "=====",
         "\nStatus: installed\nPublisher: unknown\nLatest Version: tag\nDepends: dep1, dep2\nDescription: description"]) ->
                ok
            end),
        meck:expect(epax_com, success, fun("~s====================", [ok]) -> ok end),

        ?assertEqual(ok, epax_app:show(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, get_index_entry, [appname])),
        ?assertEqual(1, meck:num_calls(epax_dep, find_all_deps_for, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, format, ["~s ~s ~s~s~n",
            ["=====", appname, "=====", "\nStatus: installed\nPublisher: unknown\nLatest Version: tag\nDepends: dep1, dep2\nDescription: description"]])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s====================", [ok]]))
    end},
    {"test for show application when no tags are available",
    fun() ->
        meck:expect(epax_index, get_index_entry, fun(appname) ->
                {ok, #application{name=appname,
                     repo_link="link1",
                     repo_type=git,
                     details=[{publisher, "publisher"}, {tags, []}, {description, "description"}]}}
            end),
        meck:expect(epax_dep, find_all_deps_for, fun(appname) -> {ok, [dep1, dep2]} end),
        meck:expect(epax_com, format, fun(X) -> lists:flatten(X) end),
        meck:expect(epax_com, format, fun("~s ~s ~s~s~n", ["=====", appname, "=====",
         "\nStatus: installed\nPublisher: publisher\nLatest Version: unknown\nDepends: dep1, dep2\nDescription: description"]) ->
                ok
            end),
        meck:expect(epax_com, success, fun("~s====================", [ok]) -> ok end),

        ?assertEqual(ok, epax_app:show(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, get_index_entry, [appname])),
        ?assertEqual(1, meck:num_calls(epax_dep, find_all_deps_for, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, format, ["~s ~s ~s~s~n",
            ["=====", appname, "=====", "\nStatus: installed\nPublisher: publisher\nLatest Version: unknown\nDepends: dep1, dep2\nDescription: description"]])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s====================", [ok]]))
    end},
    {"test for show application when description is empty",
    fun() ->
        meck:expect(epax_index, get_index_entry, fun(appname) ->
                {ok, #application{name=appname,
                     repo_link="link1",
                     repo_type=git,
                     details=[{publisher, "publisher"}, {tags, ["tag"]}, {description, []}]}}
            end),
        meck:expect(epax_dep, find_all_deps_for, fun(appname) -> {ok, [dep1, dep2]} end),
        meck:expect(epax_com, format, fun(X) -> lists:flatten(X) end),
        meck:expect(epax_com, format, fun("~s ~s ~s~s~n", ["=====", appname, "=====",
         "\nStatus: installed\nPublisher: publisher\nLatest Version: tag\nDepends: dep1, dep2\nDescription: unknown"]) ->
                ok
            end),
        meck:expect(epax_com, success, fun("~s====================", [ok]) -> ok end),

        ?assertEqual(ok, epax_app:show(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, get_index_entry, [appname])),
        ?assertEqual(1, meck:num_calls(epax_dep, find_all_deps_for, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, format, ["~s ~s ~s~s~n",
            ["=====", appname, "=====", "\nStatus: installed\nPublisher: publisher\nLatest Version: tag\nDepends: dep1, dep2\nDescription: unknown"]])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s====================", [ok]]))
    end},
    {"test for show application when no dependencies",
    fun() ->
        meck:expect(epax_index, get_index_entry, fun(appname) ->
                {ok, #application{name=appname,
                     repo_link="link1",
                     repo_type=git,
                     details=[{publisher, "publisher"}, {tags, ["tag"]}, {description, "description"}]}}
            end),
        meck:expect(epax_dep, find_all_deps_for, fun(appname) -> {ok, []} end),
        meck:expect(epax_com, format, fun(X) -> lists:flatten(X) end),
        meck:expect(epax_com, format, fun("~s ~s ~s~s~n", ["=====", appname, "=====",
         "\nStatus: installed\nPublisher: publisher\nLatest Version: tag\nDepends: no dependencies\nDescription: description"]) ->
                ok
            end),
        meck:expect(epax_com, success, fun("~s====================", [ok]) -> ok end),

        ?assertEqual(ok, epax_app:show(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, get_index_entry, [appname])),
        ?assertEqual(1, meck:num_calls(epax_dep, find_all_deps_for, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, format, ["~s ~s ~s~s~n",
            ["=====", appname, "=====", "\nStatus: installed\nPublisher: publisher\nLatest Version: tag\nDepends: no dependencies\nDescription: description"]])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s====================", [ok]]))
    end},
    {"test for show application when key does not exist -1",
    fun() ->
        meck:expect(epax_index, get_index_entry, fun(appname) ->
                {ok, #application{name=appname,
                     repo_link="link1",
                     repo_type=git,
                     details=[{publisher, "publisher"}, {description, "description"}]}}
            end),
        meck:expect(epax_dep, find_all_deps_for, fun(appname) -> {ok, [dep1, dep2]} end),
        meck:expect(epax_com, format, fun(X) -> lists:flatten(X) end),
        meck:expect(epax_com, format, fun("~s ~s ~s~s~n", ["=====", appname, "=====",
         "\nStatus: installed\nPublisher: publisher\nLatest Version: unknown\nDepends: dep1, dep2\nDescription: description"]) ->
                ok
            end),
        meck:expect(epax_com, success, fun("~s====================", [ok]) -> ok end),

        ?assertEqual(ok, epax_app:show(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, get_index_entry, [appname])),
        ?assertEqual(1, meck:num_calls(epax_dep, find_all_deps_for, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, format, ["~s ~s ~s~s~n",
            ["=====", appname, "=====", "\nStatus: installed\nPublisher: publisher\nLatest Version: unknown\nDepends: dep1, dep2\nDescription: description"]])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s====================", [ok]]))
    end},
    {"test for show application when key does not exist",
    fun() ->
        meck:expect(epax_index, get_index_entry, fun(appname) ->
                {ok, #application{name=appname,
                     repo_link="link1",
                     repo_type=git,
                     details=[]}}
            end),
        meck:expect(epax_dep, find_all_deps_for, fun(appname) -> {ok, [dep1, dep2]} end),
        meck:expect(epax_com, format, fun(X) -> lists:flatten(X) end),
        meck:expect(epax_com, format, fun("~s ~s ~s~s~n", ["=====", appname, "=====",
         "\nStatus: installed\nPublisher: unknown\nLatest Version: unknown\nDepends: dep1, dep2\nDescription: unknown"]) ->
                ok
            end),
        meck:expect(epax_com, success, fun("~s====================", [ok]) -> ok end),

        ?assertEqual(ok, epax_app:show(appname)),
        ?assertEqual(1, meck:num_calls(epax_index, get_index_entry, [appname])),
        ?assertEqual(1, meck:num_calls(epax_dep, find_all_deps_for, [appname])),
        ?assertEqual(1, meck:num_calls(epax_com, format, ["~s ~s ~s~s~n",
            ["=====", appname, "=====", "\nStatus: installed\nPublisher: unknown\nLatest Version: unknown\nDepends: dep1, dep2\nDescription: unknown"]])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s====================", [ok]]))
    end}]}.
