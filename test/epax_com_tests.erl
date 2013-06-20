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

-module(epax_com_tests).
-include_lib("eunit/include/eunit.hrl").

print_test() ->
    ?assertEqual("1, 2, 3", epax_com:format(["1, ", "2, ", "3"])),
    ?assertEqual("1, 2, 3", epax_com:format("~p, ~p, ~p", [1, 2, 3])),
    ?assertEqual(ok, epax_com:success("message")),
    ?assertEqual(ok, epax_com:success("message and ~s", ["args"])),
    ?assertEqual(ok, epax_com:error("reason", "conclusion")),
    ?assertEqual(ok, epax_com:error("reason", "conclusion and ~s", ["args"])),
    ?assertThrow(reason, epax_com:abort(reason, "conclusion")),
    ?assertThrow(reason, epax_com:abort(reason, "conclusion and ~s", ["args"])).

get_appfile_content_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, file]) end,
    [{"test for get_appfile_content when .app file is used",
    fun() ->
        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking functions of file module
        meck:expect(file, consult, fun("packages/appname/ebin/meck.app") -> ok end),
        meck:expect(file, list_dir, fun("packages/appname/ebin") ->
            {ok,["meck_expect.beam","meck_code_gen.beam","meck_code.beam",
                 "meck_matcher.beam","meck_proc.beam","meck_args_matcher.beam",
                 "meck_ret_spec.beam","meck.app","meck_history.beam",
                 "meck.beam","meck_cover.beam","meck_util.beam"]} end),

        ?assertEqual(ok, epax_com:get_appfile_content(appname)),
        ?assertEqual(1, meck:num_calls(file, consult, ["packages/appname/ebin/meck.app"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/ebin"])),
        ?assertEqual(0, meck:num_calls(file, list_dir, ["packages/appname/src"]))
    end},
    {"test for get_appfile_content when .app.src file is used",
    fun() ->
        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking functions of file module
        meck:expect(file, consult, fun("packages/appname/src/meck.app.src") -> ok end),
        meck:expect(file, list_dir, fun
            ("packages/appname/ebin") ->
                {ok,["meck_expect.beam","meck_code_gen.beam","meck_code.beam",
                     "meck_matcher.beam","meck_proc.beam","meck_args_matcher.beam",
                     "meck_ret_spec.beam","meck_app.beam","meck_util.beam"]};
            ("packages/appname/src") ->
                {ok,["meck_expect.erl","meck_code_gen.erl", "meck_app.erl", "meck_app.src", "meck.app.src"]} end),

        ?assertEqual(ok, epax_com:get_appfile_content(appname)),
        ?assertEqual(1, meck:num_calls(file, consult, ["packages/appname/src/meck.app.src"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/ebin"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/src"]))
    end},
    {"test for get_appfile_content when .app.src or .app file is not found",
    fun() ->
        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking functions of file module
        meck:expect(file, consult, fun("packages/appname/src/meck.app.src") -> ok end),
        meck:expect(file, list_dir, fun
            ("packages/appname/ebin") ->
                {ok,["meck_expect.beam","meck_code_gen.beam","meck_code.beam",
                     "meck_matcher.beam","meck_proc.beam","meck_args_matcher.beam",
                     "meck_ret_spec.beam","meck_app.beam","meck_util.beam"]};
            ("packages/appname/src") ->
                {ok,["meck_expect.erl","meck_code_gen.erl", "meck_app.erl", "meck_app.src"]} end),

        ?assertEqual({error, ".app or .app.src file not found"}, epax_com:get_appfile_content(appname)),
        ?assertEqual(0, meck:num_calls(file, consult, ["packages/appname/src/meck.app.src"])),
        ?assertEqual(0, meck:num_calls(file, consult, ["packages/appname/ebin/meck.app"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/ebin"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/src"]))
    end},
    {"test for get_appfile_content when there are multiple .app files",
    fun() ->
        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking functions of file module
        meck:expect(file, consult, fun("packages/appname/src/meck.app.src") -> ok end),
        meck:expect(file, list_dir, fun
            ("packages/appname/ebin") ->
                {ok,["meck_expect.beam","meck_code_gen.beam","meck_code.beam",
                     "meck_matcher.beam","meck_proc.beam","meck_args_matcher.beam",
                     "meck_ret_spec.beam","meck_app.beam","meck_util.beam",
                     "meck_dup.app", "meck.app"]};
            ("packages/appname/src") ->
                {ok,["meck_expect.erl","meck_code_gen.erl", "meck_app.erl", "meck_app.src"]} end),

        ?assertEqual({error, "More than one .app file in ebin folder"}, epax_com:get_appfile_content(appname)),
        ?assertEqual(0, meck:num_calls(file, consult, ["packages/appname/src/meck.app.src"])),
        ?assertEqual(0, meck:num_calls(file, consult, ["packages/appname/ebin/meck.app"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/ebin"])),
        ?assertEqual(0, meck:num_calls(file, list_dir, ["packages/appname/src"]))
    end},
    {"test for get_appfile_content when there are multiple .app.src files",
    fun() ->
        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking functions of file module
        meck:expect(file, consult, fun("packages/appname/src/meck.app.src") -> ok end),
        meck:expect(file, list_dir, fun
            ("packages/appname/ebin") ->
                {ok,["meck_expect.beam","meck_code_gen.beam","meck_code.beam",
                     "meck_matcher.beam","meck_proc.beam","meck_args_matcher.beam",
                     "meck_ret_spec.beam","meck_app.beam","meck_util.beam"]};
            ("packages/appname/src") ->
                {ok,["meck_expect.erl","meck_code_gen.erl", "meck_app.erl", "meck_app.src",
                    "meck.app.src", "meck_dup.app.src"]} end),

        ?assertEqual({error, "More than one .app.src file in src folder"}, epax_com:get_appfile_content(appname)),
        ?assertEqual(0, meck:num_calls(file, consult, ["packages/appname/src/meck.app.src"])),
        ?assertEqual(0, meck:num_calls(file, consult, ["packages/appname/ebin/meck.app"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/ebin"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/src"]))
    end},
    {"test for get_appfile_content when list_dir throws error",
    fun() ->
        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking functions of file module
        meck:expect(file, list_dir, fun
            ("packages/appname/ebin") ->
                {ok,["meck_expect.beam","meck_code_gen.beam","meck_code.beam",
                     "meck_matcher.beam","meck_proc.beam","meck_args_matcher.beam",
                     "meck_ret_spec.beam","meck_app.beam","meck_util.beam"]};
            ("packages/appname/src") ->
                {error, "error"} end),

        ?assertEqual({error, ".app or .app.src file not found"}, epax_com:get_appfile_content(appname)),
        ?assertEqual(0, meck:num_calls(file, consult, ["packages/appname/src/meck.app.src"])),
        ?assertEqual(0, meck:num_calls(file, consult, ["packages/appname/ebin/meck.app"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/ebin"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages/appname/src"]))
    end},
    {"test for get_appfile_content when path is given",
    fun() ->
        % mocking get_abs_path function
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),

        % mocking functions of file module
        meck:expect(file, consult, fun("temp/src/meck.app.src") -> ok end),
        meck:expect(file, list_dir, fun
            ("temp/ebin") ->
                {ok,["meck_expect.beam","meck_code_gen.beam","meck_code.beam",
                     "meck_matcher.beam","meck_proc.beam","meck_args_matcher.beam",
                     "meck_ret_spec.beam","meck_app.beam","meck_util.beam"]};
            ("temp/src") ->
                {ok,["meck_expect.erl","meck_code_gen.erl", "meck_app.erl", "meck.app.src"]} end),

        ?assertEqual(ok, epax_com:get_appfile_content("temp")),
        ?assertEqual(1, meck:num_calls(file, consult, ["temp/src/meck.app.src"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["temp/ebin"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["temp/src"]))
    end}]}.
