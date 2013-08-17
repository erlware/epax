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
-module(epax_index_tests).
-include_lib("eunit/include/eunit.hrl").

init_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, file]) end,
    [{"test for init function",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(epax_os, touch, fun("index.cfg") -> ok end),
        meck:expect(epax_os, mkdir, fun("packages") -> ok end),
        meck:expect(epax_os, rmdir, fun("packages/*") -> ok end),
        meck:expect(file, write_file, fun("index.cfg", ["[]",46,10]) -> ok end),

        ?assertEqual(ok, epax_index:init()),
        ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_os, touch, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages"])),
        ?assertEqual(1, meck:num_calls(epax_os, mkdir, ["packages"])),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/*"])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/*"])),
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", ["[]",46,10]]))
    end}]}.

app_exists_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, file]) end,
    [{"test for app_exists when index file is not found",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),

        ?assertEqual({error, "please run `epax install` before running other epax commands"},
                     epax_index:app_exists(appname)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists when index file is empty",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[]]} end),

        ?assertEqual({ok, false}, epax_index:app_exists(appname)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists when app does not exist",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []}, {app2, "link2", []}]]} end),

        ?assertEqual({ok, false}, epax_index:app_exists(appname)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists when given app link",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),

        ?assertEqual({ok, app2}, epax_index:app_exists("link2")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists when given app link not found",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),

        ?assertEqual({ok, false}, epax_index:app_exists("link3")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists when function is called wrong",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),

        ?assertEqual({ok, false}, epax_index:app_exists({})),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),

        ?assertEqual({ok, app2}, epax_index:app_exists(app2)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end}]}.

checkout_repo_and_add_to_index_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, epax_repo, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, epax_repo, file]) end,
    [{"test for checkout_repo_and_add_to_index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),
        meck:expect(epax_repo, clone_app, fun("link3") -> {ok, {app3, "link3", []}} end),
        meck:expect(file, write_file, fun("index.cfg", [[91,[[123,["app3",44,"\"link3\"",44,"[]"],125],
                                                             44,
                                                             [123,["app1",44,"\"link1\"",44,"[]"],125],
                                                             44,
                                                             [123,["app2",44,"\"link2\"",44,"[]"],125]],
                                                             93],
                                                            46,10]) -> ok end),

        ?assertEqual({ok, app3}, epax_index:checkout_repo_and_add_to_index("link3")),
        ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_repo, clone_app, ["link3"])),
        Result_apps = [[91,[[123,["app3",44,"\"link3\"",44,"[]"],125],
                        44,
                        [123,["app1",44,"\"link1\"",44,"[]"],125],
                        44,
                        [123,["app2",44,"\"link2\"",44,"[]"],125]],
                        93],
                       46,10],
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", Result_apps])),
        ?assertEqual(0, meck:num_calls(epax_os, rmdir, ["packages/app3"]))
    end},
    {"test for checkout_repo_and_add_to_index when index file cannot be read",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),

        ?assertEqual({error, "please run `epax install` before running other epax commands"},
                     epax_index:checkout_repo_and_add_to_index("link3")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(0, meck:num_calls(epax_repo, clone_app, ["link3"])),
        Result_apps = [[91,[[123,["app3",44,"\"link3\"",44,"[]"],125],
                        44,
                        [123,["app1",44,"\"link1\"",44,"[]"],125],
                        44,
                        [123,["app2",44,"\"link2\"",44,"[]"],125]],
                        93],
                       46,10],
        ?assertEqual(0, meck:num_calls(file, write_file, ["index.cfg", Result_apps])),
        ?assertEqual(0, meck:num_calls(epax_os, rmdir, ["packages/app3"]))
    end},
    {"test for checkout_repo_and_add_to_index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),
        meck:expect(epax_repo, clone_app, fun("link3") -> {error, "error"} end),
        
        ?assertEqual({error, "error"}, epax_index:checkout_repo_and_add_to_index("link3")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_repo, clone_app, ["link3"])),
        Result_apps = [[91,[[123,["app3",44,"\"link3\"",44,"[]"],125],
                        44,
                        [123,["app1",44,"\"link1\"",44,"[]"],125],
                        44,
                        [123,["app2",44,"\"link2\"",44,"[]"],125]],
                        93],
                       46,10],
        ?assertEqual(0, meck:num_calls(file, write_file, ["index.cfg", Result_apps])),
        ?assertEqual(0, meck:num_calls(epax_os, rmdir, ["packages/app3"]))
    end},
    {"test for checkout_repo_and_add_to_index when index file cannot be written",
        fun() ->
            meck:expect(epax_os, get_abs_path, fun(X) -> X end),
            meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                                  {app2, "link2", []}]]} end),
            meck:expect(epax_repo, clone_app, fun("link3") -> {ok, {app3, "link3", []}} end),
            meck:expect(file, write_file, fun("index.cfg", [[91,[[123,["app3",44,"\"link3\"",44,"[]"],125],
                                                                 44,
                                                                 [123,["app1",44,"\"link1\"",44,"[]"],125],
                                                                 44,
                                                                 [123,["app2",44,"\"link2\"",44,"[]"],125]],
                                                                 93],
                                                                46,10]) -> {error, "error"} end),
            meck:expect(epax_os, rmdir, fun("packages/app3") -> ok end),

            ?assertEqual({error, "error"}, epax_index:checkout_repo_and_add_to_index("link3")),
            ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
            ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
            ?assertEqual(1, meck:num_calls(epax_repo, clone_app, ["link3"])),
            Result_apps = [[91,[[123,["app3",44,"\"link3\"",44,"[]"],125],
                            44,
                            [123,["app1",44,"\"link1\"",44,"[]"],125],
                            44,
                            [123,["app2",44,"\"link2\"",44,"[]"],125]],
                            93],
                           46,10],
            ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", Result_apps])),
            ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/app3"]))
        end}]}.

remove_from_index_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, file]) end,
    [{"test for remove_from_index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),
        meck:expect(epax_os, rmdir, fun("packages/app2") -> ok end),

        ?assertEqual(ok, epax_index:remove_from_index(app2)),
        ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["packages/app2"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/app2"]))
    end},
    {"test for remove_from_index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),

        ?assertEqual({error, "please run `epax install` before running other epax commands"},
                     epax_index:remove_from_index(app2)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(0, meck:num_calls(epax_os, get_abs_path, ["packages/app2"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(0, meck:num_calls(epax_os, rmdir, ["packages/app2"]))
    end}]}.

get_applist_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, file]) end,
    [{"test for get_applist",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),

        ?assertEqual({ok, [app1, app2]}, epax_index:get_applist()),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for get_applist",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),

        ?assertEqual({error, "please run `epax install` before running other epax commands"},
                     epax_index:get_applist()),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end}]}.

update_index_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, epax_repo, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, epax_repo, file]) end,
    [{"test for update_index when index cannot be read",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),

        ?assertEqual({error, "please run `epax install` before running other epax commands"},
                     epax_index:update_index()),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for update_index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),
        meck:expect(file, write_file, fun("index.cfg", [[91,
                                                         [[123,["app1",44,"\"link1\"",44,[91,[[123,["detail1",44,"\"detail1\""],125]],93]],125],
                                                          44,
                                                          [123,["app2",44,"\"link2\"",44,[91,[[123,["detail2",44,"\"detail2\""],125]],93]],125]],
                                                         93],
                                                        46,10]) -> ok end),
        meck:expect(epax_repo, update_repo, fun({app1, "link1", []}) ->
                                                    {ok, {app1, "link1", [{detail1, "detail1"}]}};
                                               ({app2, "link2", []}) ->
                                                    {ok, {app2, "link2", [{detail2, "detail2"}]}} end),

        ?assertEqual(ok ,epax_index:update_index()),
        ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", [[91,
                                                                         [[123,["app1",44,"\"link1\"",44,[91,[[123,["detail1",44,"\"detail1\""],125]],93]],125],
                                                                          44,
                                                                          [123,["app2",44,"\"link2\"",44,[91,[[123,["detail2",44,"\"detail2\""],125]],93]],125]],
                                                                         93],
                                                                        46,10]]))
    end},
    {"test for update_index when app1 cannot be updated",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[{app1, "link1", []},
                                                              {app2, "link2", []}]]} end),
        meck:expect(file, write_file, fun("index.cfg", [[91,
                                                         [[123,["app1",44,"\"link1\"",44,"[]"],125],
                                                          44,
                                                          [123,["app2",44,"\"link2\"",44,[91,[[123,["detail2",44,"\"detail2\""],125]],93]],125]],
                                                         93],
                                                        46,10]) -> ok end),
        meck:expect(epax_repo, update_repo, fun({app1, "link1", []}) ->
                                                    {error, "error"};
                                               ({app2, "link2", []}) ->
                                                    {ok, {app2, "link2", [{detail2, "detail2"}]}} end),

        ?assertEqual(ok ,epax_index:update_index()),
        ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", [[91,
                                                                         [[123,["app1",44,"\"link1\"",44,"[]"],125],
                                                                          44,
                                                                          [123,["app2",44,"\"link2\"",44,[91,[[123,["detail2",44,"\"detail2\""],125]],93]],125]],
                                                                         93],
                                                                        46,10]]))
    end}]}.
