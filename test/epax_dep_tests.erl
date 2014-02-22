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

-module(epax_dep_tests).
-include_lib("eunit/include/eunit.hrl").

bundle_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, epax_index, epax_com]) end,
    fun(_) -> meck:unload([epax_index, epax_os, epax_com]) end,
    [{"test for bundle",
    fun() ->
        AAppfileContent = {application, a, [{applications, [kernel, stdlib, sasl]}, {included_applications, [b, c, d, ssl]}]},
        BAppfileContent = {application, b, [{applications, [kernel, stdlib, ssl]}, {included_applications, [c, e, f]}]},
        CAppfileContent = {application, c, [{applications, [kernel, stdlib]}, {included_applications, [f, sasl]}]},
        DAppfileContent = {application, d, [{included_applications, [e, stdlib, kernel]}]},
        EAppfileContent = {application, e, [{applications, [kernel, stdlib]}]},
        FAppfileContent = {application, f, [{applications, [kernel, stdlib]}]},

        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking copy_folder function for all dependencies
        meck:expect(epax_os, copy_folder, fun
            ("packages/b", "packages/a/deps") -> ok;
            ("packages/c", "packages/a/deps") -> ok;
            ("packages/d", "packages/a/deps") -> ok;
            ("packages/e", "packages/a/deps") -> ok;
            ("packages/f", "packages/a/deps") -> ok
        end),

        % mocking app_exists function call for all apps
        meck:expect(epax_index, app_exists, fun
            (a) -> {ok, a};
            (b) -> {ok, b};
            (c) -> {ok, c};
            (d) -> {ok, d};
            (e) -> {ok, e};
            (f) -> {ok, f}
        end),

        % mocking the call to get_appfile_content function
        meck:expect(epax_com, get_appfile_content,
            fun
                (a) -> {ok, [AAppfileContent]};
                (b) -> {ok, [BAppfileContent]};
                (c) -> {ok, [CAppfileContent]};
                (d) -> {ok, [DAppfileContent]};
                (e) -> {ok, [EAppfileContent]};
                (f) -> {ok, [FAppfileContent]}
            end),
        meck:expect(epax_com, console, fun("Copying ~p~n", [f]) ->
                                               ok;
                                          ("Copying ~p~n", [e]) ->
                                               ok;
                                          ("Copying ~p~n", [d]) ->
                                               ok;
                                          ("Copying ~p~n", [c]) ->
                                               ok;
                                          ("Copying ~p~n", [b]) ->
                                               ok end),

        ?assertEqual(ok, epax_dep:bundle(a)),

        % asserting that call is made for copy_folder function for each dep
        ?assertEqual(1, meck:num_calls(epax_os, copy_folder, ["packages/b", "packages/a/deps"])),
        ?assertEqual(1, meck:num_calls(epax_os, copy_folder, ["packages/c", "packages/a/deps"])),
        ?assertEqual(1, meck:num_calls(epax_os, copy_folder, ["packages/d", "packages/a/deps"])),
        ?assertEqual(1, meck:num_calls(epax_os, copy_folder, ["packages/e", "packages/a/deps"])),
        ?assertEqual(1, meck:num_calls(epax_os, copy_folder, ["packages/f", "packages/a/deps"])),

        % asserting call is made for app_exists function for each app
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [a])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [b])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [c])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [d])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [e])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [f])),

        % asserting call is made for get_appfile_content function
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [a])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [b])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [c])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [d])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [e])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [f])),

        ?assertEqual(1, meck:num_calls(epax_com, console, ["Copying ~p~n", [f]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["Copying ~p~n", [e]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["Copying ~p~n", [d]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["Copying ~p~n", [c]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["Copying ~p~n", [b]]))
    end},
    {"test for bundle when some app is not already added",
    fun() ->
        AAppfileContent = {application, a, [{applications, [kernel, stdlib, sasl]}, {included_applications, [b, c, d, ssl]}]},
        BAppfileContent = {application, b, [{applications, [kernel, stdlib, ssl]}, {included_applications, [c, e, f]}]},
        CAppfileContent = {application, c, [{applications, [kernel, stdlib]}, {included_applications, [f, sasl]}]},
        DAppfileContent = {application, d, [{included_applications, [e, stdlib, kernel]}]},
        EAppfileContent = {application, e, [{applications, [kernel, stdlib]}]},
        FAppfileContent = {application, f, [{applications, [kernel, stdlib]}]},

        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking app_exists function call for all apps
        meck:expect(epax_index, app_exists, fun
            (a) -> {ok, a};
            (b) -> {ok, b};
            (c) -> {ok, c};
            (d) -> {ok, d};
            (e) -> {ok, e};
            (f) -> {ok, false}
        end),

        % mocking the call to get_appfile_content function
        meck:expect(epax_com, get_appfile_content, fun
            (a) -> {ok, [AAppfileContent]};
            (b) -> {ok, [BAppfileContent]};
            (c) -> {ok, [CAppfileContent]};
            (d) -> {ok, [DAppfileContent]};
            (e) -> {ok, [EAppfileContent]};
            (f) -> {ok, [FAppfileContent]}
        end),
        meck:expect(epax_com, format, fun("~s does not exist", [f]) -> "f does not exist" end),

        ?assertEqual({error, "f does not exist"}, epax_dep:bundle(a)),

        % asserting call is made for app_exists function for each app
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [a])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [b])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [c])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [d])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [e])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [f])),

        % asserting call is made for get_appfile_content function
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [a])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [b])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [c])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [d])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [e])),
        ?assertEqual(0, meck:num_calls(epax_com, get_appfile_content, [f])),

        ?assertEqual(0, meck:num_calls(epax_com, console, ["Copying ~p~n", [f]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["Copying ~p~n", [e]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["Copying ~p~n", [d]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["Copying ~p~n", [c]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["Copying ~p~n", [b]])),
        ?assertEqual(1, meck:num_calls(epax_com, format, ["~s does not exist", [f]]))
    end},
    {"test for bundle when app_exists returns error",
    fun() ->
        AAppfileContent = {application, a, [{applications, [kernel, stdlib, sasl]}, {included_applications, [b, c, ssl]}]},
        BAppfileContent = {application, b, [{applications, [kernel, stdlib, ssl]}, {included_applications, [c]}]},
        CAppfileContent = {application, c, [{applications, [kernel, stdlib]}, {included_applications, [sasl]}]},

        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking app_exists function call for all apps
        meck:expect(epax_index, app_exists, fun
            (a) -> {ok, a};
            (b) -> {error, "error"};
            (c) -> {ok, c}
        end),

        % mocking the call to get_appfile_content function
        meck:expect(epax_com, get_appfile_content, fun
            (a) -> {ok, [AAppfileContent]};
            (b) -> {ok, [BAppfileContent]};
            (c) -> {ok, CAppfileContent}
        end),

        ?assertEqual({error, "error"}, epax_dep:bundle(a)),

        % asserting call is made for app_exists function for each app
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [a])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [b])),
        ?assertEqual(0, meck:num_calls(epax_index, app_exists, [c])),

        % asserting call is made for get_appfile_content function
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [a])),
        ?assertEqual(0, meck:num_calls(epax_com, get_appfile_content, [b])),
        ?assertEqual(0, meck:num_calls(epax_com, get_appfile_content, [c]))
    end},
    {"test for bundle when get_appfile_content returns error",
    fun() ->
        AAppfileContent = {application, a, [{applications, [kernel, stdlib, sasl]}, {included_applications, [b, c, d, ssl]}]},
        BAppfileContent = {application, b, [{applications, [kernel, stdlib, ssl]}, {included_applications, [c, e, f]}]},
        CAppfileContent = {application, c, [{applications, [kernel, stdlib]}, {included_applications, [f, sasl]}]},
        DAppfileContent = {application, d, [{included_applications, [e, stdlib, kernel]}]},
        FAppfileContent = {application, f, [{applications, [kernel, stdlib]}]},

        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking copy_folder function for all dependencies
        meck:expect(epax_os, copy_folder, fun
            ("packages/b", "packages/a/deps") -> ok;
            ("packages/c", "packages/a/deps") -> ok;
            ("packages/d", "packages/a/deps") -> ok;
            ("packages/e", "packages/a/deps") -> ok;
            ("packages/f", "packages/a/deps") -> ok
        end),

        % mocking app_exists function call for all apps
        meck:expect(epax_index, app_exists, fun
            (a) -> {ok, a};
            (b) -> {ok, b};
            (c) -> {ok, c};
            (d) -> {ok, d};
            (e) -> {ok, e};
            (f) -> {ok, f}
        end),

        % mocking the call to get_appfile_content function
        meck:expect(epax_com, get_appfile_content, fun
            (a) -> {ok, [AAppfileContent]};
            (b) -> {ok, [BAppfileContent]};
            (c) -> {ok, [CAppfileContent]};
            (d) -> {ok, [DAppfileContent]};
            (e) -> {error, "error"};
            (f) -> {ok, [FAppfileContent]}
        end),

        ?assertEqual({error, "error"}, epax_dep:bundle(a)),

        % asserting that call is made for copy_folder function for each dep
        ?assertEqual(0, meck:num_calls(epax_os, copy_folder, ["packages/b", "packages/a/deps"])),
        ?assertEqual(0, meck:num_calls(epax_os, copy_folder, ["packages/c", "packages/a/deps"])),
        ?assertEqual(0, meck:num_calls(epax_os, copy_folder, ["packages/d", "packages/a/deps"])),
        ?assertEqual(0, meck:num_calls(epax_os, copy_folder, ["packages/e", "packages/a/deps"])),
        ?assertEqual(0, meck:num_calls(epax_os, copy_folder, ["packages/f", "packages/a/deps"])),

        % asserting call is made for app_exists function for each app
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [a])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [b])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [c])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [d])),
        ?assertEqual(1, meck:num_calls(epax_index, app_exists, [e])),
        ?assertEqual(0, meck:num_calls(epax_index, app_exists, [f])),

        % asserting call is made for get_appfile_content function
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [a])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [b])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [c])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [d])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, [e])),
        ?assertEqual(0, meck:num_calls(epax_com, get_appfile_content, [f]))
    end}]}.
