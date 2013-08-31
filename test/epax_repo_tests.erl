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
-module(epax_repo_tests).
-include_lib("eunit/include/eunit.hrl").

clone_app_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, epax_com, filelib], [passthrough, unstick]) end,
    fun(_) -> meck:unload([epax_os, epax_com, filelib]) end,
    [{"test for clone_app function",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(filelib, is_dir, fun("packages/temp") -> true end),
        Appfile_content = {ok, [{application, app1, [{applications, [kernel, stdlib, sasl]},
                                                     {included_applications, [b, c, d, ssl]},
                                                     {author, "author"},
                                                     {description, "description"}]}]},
        meck:expect(epax_com, get_appfile_content, fun("packages/temp") -> Appfile_content end),
        meck:expect(epax_os, run_in_dir, fun("", "git clone .git packages/temp") ->
                                                ok;
                                            ("packages/temp", "git tag") ->
                                                "v0.2.0\nv0.3.0\nv0.4.0";
                                            ("packages/temp", "git branch --remote") ->
                                                "  origin/HEAD -> origin/master\n  origin/dev\n  origin/master\n" end),
        meck:expect(epax_os, mv_folder, fun("packages/temp", "packages/app1") -> ok end),

        ?assertEqual({ok, {app1, ".git", [{description, "description"},
                                           {publisher, "author"},
                                           {tags, ["v0.4.0","v0.3.0","v0.2.0"]},
                                           {branches, ["master","dev"]}]}},
                      epax_repo:clone_app(".git")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/temp"])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, ["packages/temp"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/temp", "git tag"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/temp", "git branch --remote"])),
        ?assertEqual(1, meck:num_calls(epax_os, mv_folder, ["packages/temp", "packages/app1"]))
    end},
    {"test for clone_app function when get_appfile_content returns error",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(filelib, is_dir, fun("packages/temp") -> true end),
        meck:expect(epax_com, get_appfile_content, fun("packages/temp") -> {error, "error"} end),
        meck:expect(epax_os, run_in_dir, fun("", "git clone .git packages/temp") -> ok end),
        meck:expect(epax_os, rmdir, fun("packages/temp") -> ok end),

        ?assertEqual({error, "error"}, epax_repo:clone_app(".git")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/temp"])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, ["packages/temp"])),
        ?assertEqual(0, meck:num_calls(epax_os, run_in_dir, ["packages/temp", "git tag"])),
        ?assertEqual(0, meck:num_calls(epax_os, run_in_dir, ["packages/temp", "git branch --remote"])),
        ?assertEqual(0, meck:num_calls(epax_os, mv_folder, ["packages/temp", "packages/app1"])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/temp"]))
    end},
    {"test for clone_app function when download repo fails",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(epax_os, run_in_dir, fun("", "git clone .git packages/temp") -> ok end),
        meck:expect(filelib, is_dir, fun("packages/temp") -> false end), 
        ?assertEqual({error, "unable to download repo"}, epax_repo:clone_app(".git")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/temp"]))
    end},
    {"test for clone_app function when type of repo cannot be determined",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        ?assertEqual({error, "unknown type of repo, use -r option to specify type of repo"},
                      epax_repo:clone_app("link")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/temp"]))
    end}]}.

update_repo_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, epax_com], [passthrough]) end,
    fun(_) -> meck:unload([epax_os, epax_com]) end,
    [{"test for update_repo function 1",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        Appfile_content = {ok, [{application, app1, [{applications, [kernel, stdlib, sasl]},
                                                     {included_applications, [b, c, d, ssl]},
                                                     {author, "author"},
                                                     {description, "description"}]}]},
        meck:expect(epax_com, get_appfile_content, fun("packages/app1") -> Appfile_content end),
        meck:expect(epax_os, run_in_dir, fun("packages/app1", "git pull") ->
                                                {ok, data};
                                            ("packages/app1", "git tag") ->
                                                "0.2.12\n 0.2.13\n 0.2.14\n 0.2.15\n 0.2.16\n v0.1.0\n v0.1.1\n v0.1.2\n v0.2.0\n v0.2.1\n v0.2.10\n v0.2.11\n v0.2.2\n v0.2.3\n v0.2.4\n v0.2.5\n v0.2.6\n v0.2.7\n v0.2.8\n v0.2.9\n";
                                            ("packages/app1", "git branch --remote") ->
                                                "  origin/HEAD -> origin/master\n  origin/feature_pages\n  origin/master\n" end),

        App = {app1, ".git", []},
        ?assertEqual({ok, {app1, ".git", [{description, "description"},
                                           {publisher, "author"},
                                           {tags, ["v0.2.9","v0.2.8","v0.2.7","v0.2.6","v0.2.5","v0.2.4",
                                                   "v0.2.3","v0.2.2","v0.2.11","v0.2.10","v0.2.1","v0.2.0",
                                                   "v0.1.2","v0.1.1","v0.1.0","0.2.16","0.2.15","0.2.14",
                                                   "0.2.13","0.2.12"]},
                                           {branches, ["master","feature_pages"]}]}},
                      epax_repo:update_repo(App)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/app1"])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, ["packages/app1"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/app1", "git pull"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/app1", "git tag"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/app1", "git branch --remote"]))
    end},
    {"test for update_repo function 2",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        Appfile_content = {ok, [{application, app1, [{applications, [kernel, stdlib, sasl]},
                                                     {included_applications, [b, c, d, ssl]},
                                                     {author, "author"},
                                                     {description, "description"}]}]},
        meck:expect(epax_com, get_appfile_content, fun("packages/app1") -> Appfile_content end),
        meck:expect(epax_os, run_in_dir, fun("packages/app1", "git pull") ->
                                                {ok, data};
                                            ("packages/app1", "git tag") ->
                                                [];
                                            ("packages/app1", "git branch --remote") ->
                                                "  origin/HEAD -> origin/master\n  origin/gh-pages\n  origin/master\n" end),

        App = {app1, ".git", []},
        ?assertEqual({ok, {app1, ".git", [{description, "description"},
                                           {publisher, "author"},
                                           {tags, []},
                                           {branches, ["master","gh-pages"]}]}},
                      epax_repo:update_repo(App)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/app1"])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, ["packages/app1"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/app1", "git pull"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/app1", "git tag"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/app1", "git branch --remote"]))
    end},
    {"test for update_repo function 3",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        Appfile_content = {ok, [{application, app1, [{applications, [kernel, stdlib, sasl]},
                                                     {included_applications, [b, c, d, ssl]},
                                                     {author, "author"}]}]},
        meck:expect(epax_com, get_appfile_content, fun("packages/app1") -> Appfile_content end),
        meck:expect(epax_os, run_in_dir, fun("packages/app1", "git pull") ->
                                                "ok";
                                            ("packages/app1", "git tag") ->
                                                "0.2.12\n 0.2.13\n 0.2.14\n 0.2.15\n 0.2.16\n v0.1.0\n v0.1.1\n v0.1.2\n v0.2.0\n v0.2.1\n v0.2.10\n v0.2.11\n v0.2.2\n v0.2.3\n v0.2.4\n v0.2.5\n v0.2.6\n v0.2.7\n v0.2.8\n v0.2.9\n";
                                            ("packages/app1", "git branch --remote") ->
                                                "  origin/HEAD -> origin/master\n  origin/feature_pages\n  origin/master\n" end),

        App = {app1, ".git", []},
        ?assertEqual({ok, {app1, ".git", [{description, ""},
                                           {publisher, "author"},
                                           {tags, ["v0.2.9","v0.2.8","v0.2.7","v0.2.6","v0.2.5","v0.2.4",
                                                   "v0.2.3","v0.2.2","v0.2.11","v0.2.10","v0.2.1","v0.2.0",
                                                   "v0.1.2","v0.1.1","v0.1.0","0.2.16","0.2.15","0.2.14",
                                                   "0.2.13","0.2.12"]},
                                           {branches, ["master","feature_pages"]}]}},
                      epax_repo:update_repo(App)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/app1"])),
        ?assertEqual(1, meck:num_calls(epax_com, get_appfile_content, ["packages/app1"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/app1", "git pull"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/app1", "git tag"])),
        ?assertEqual(1, meck:num_calls(epax_os, run_in_dir, ["packages/app1", "git branch --remote"]))
    end},
    {"test for update_repo function when appfile cannot be read",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(epax_os, run_in_dir, fun("packages/app1", "git pull")-> {ok, data} end),
        meck:expect(epax_com, get_appfile_content, fun("packages/app1") -> {error, "error"} end),

        App = {app1, ".git", []},
        ?assertEqual({error, "error"}, epax_repo:update_repo(App)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/app1"]))
    end},
    {"test for update_repo function when type of repo not determined",
    fun() ->    
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        App = {app1, "link", []},
        ?assertEqual({error, "unknown type of repo, use -r option to specify type of repo"},
                      epax_repo:update_repo(App)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/app1"]))
    end}]}.
