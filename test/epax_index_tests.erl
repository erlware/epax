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

-module(epax_index_tests).
-include("epax.hrl").
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

        ?assertEqual({error, "Run `epax init` before running other epax commands"},
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
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),
        ?assertEqual({ok, false}, epax_index:app_exists(appname)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists when given app link",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),

        ?assertEqual({ok, app2}, epax_index:app_exists("link2")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists when given app link not found",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),

        ?assertEqual({ok, false}, epax_index:app_exists("link3")),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists when function is called wrong",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),

        ?assertEqual({ok, false}, epax_index:app_exists({})),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for app_exists",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),

        ?assertEqual({ok, app2}, epax_index:app_exists(app2)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end}]}.

get_index_entry_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, file]) end,
    [{"test for get_index_entry",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),

        ?assertEqual({ok, #application{name=app1, repo_link="link1", repo_type=git, details=[]}},
                     epax_index:get_index_entry(app1)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for get_index_entry when app does not exists",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),

        ?assertEqual({error, not_found},
                     epax_index:get_index_entry(app3)),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for get_index_entry when index file does not exists",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, ""} end),

        ?assertEqual({error, "Run `epax init` before running other epax commands"},
                     epax_index:get_index_entry(app3)),
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
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),
        meck:expect(epax_repo, clone_app, fun("link3", []) -> {ok, #application{name=app3, repo_link="link3", repo_type=git, details=[]}} end),
        meck:expect(file, write_file, fun("index.cfg", [[91,
                                                          [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,"[]"],125],
                                                           44,10," ",
                                                           [123,["application",44,"app1",44,"\"link1\"",44,"git",44,"[]"],125],
                                                           44,10," ",
                                                           [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125]],
                                                          93],
                                                         46,10]) -> ok end),

        ?assertEqual({ok, app3}, epax_index:checkout_repo_and_add_to_index("link3", [])),
        ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_repo, clone_app, ["link3", []])),
        ResultApps = [[91,
                         [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,"[]"],125],
                          44,10," ",
                          [123,["application",44,"app1",44,"\"link1\"",44,"git",44,"[]"],125],
                          44,10," ",
                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125]],
                         93],
                        46,10],
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", ResultApps])),
        ?assertEqual(0, meck:num_calls(epax_os, rmdir, ["packages/app3"]))
    end},
    {"test for checkout_repo_and_add_to_index when index file cannot be read",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),

        ?assertEqual({error, "Run `epax init` before running other epax commands"},
                     epax_index:checkout_repo_and_add_to_index("link3", [])),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(0, meck:num_calls(epax_repo, clone_app, ["link3"])),
        ResultApps = [[91,
                        [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,"[]"],125],
                         44,10," ",
                         [123,["application",44,"app1",44,"\"link1\"",44,"git",44,"[]"],125],
                         44,10," ",
                         [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125]],
                        93],
                       46,10],
        ?assertEqual(0, meck:num_calls(file, write_file, ["index.cfg", ResultApps])),
        ?assertEqual(0, meck:num_calls(epax_os, rmdir, ["packages/app3"]))
    end},
    {"test for checkout_repo_and_add_to_index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),
        meck:expect(epax_repo, clone_app, fun("link3", []) -> {error, "error"} end),

        ?assertEqual({error, "error"}, epax_index:checkout_repo_and_add_to_index("link3", [])),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_repo, clone_app, ["link3", []])),
        ResultApps = [[91,
                        [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,"[]"],125],
                         44,10," ",
                         [123,["application",44,"app1",44,"\"link1\"",44,"git",44,"[]"],125],
                         44,10," ",
                         [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125]],
                        93],
                       46,10],
        ?assertEqual(0, meck:num_calls(file, write_file, ["index.cfg", ResultApps])),
        ?assertEqual(0, meck:num_calls(epax_os, rmdir, ["packages/app3"]))
    end},
    {"test for checkout_repo_and_add_to_index when index file cannot be written",
        fun() ->
            meck:expect(epax_os, get_abs_path, fun(X) -> X end),
            meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                                  #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),
            meck:expect(epax_repo, clone_app, fun("link3", []) -> {ok, #application{name=app3, repo_link="link3", repo_type=git, details=[]}} end),
            meck:expect(file, write_file, fun("index.cfg", [[91,
                                                             [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,"[]"],125],
                                                              44,10," ",
                                                              [123,["application",44,"app1",44,"\"link1\"",44,"git",44,"[]"],125],
                                                              44,10," ",
                                                              [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125]],
                                                             93],
                                                            46,10]) -> {error, "error"} end),
            meck:expect(epax_os, rmdir, fun("packages/app3") -> ok end),

            ?assertEqual({error, "error"}, epax_index:checkout_repo_and_add_to_index("link3", [])),
            ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
            ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
            ?assertEqual(1, meck:num_calls(epax_repo, clone_app, ["link3", []])),
            ResultApps = [[91,
                            [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,"[]"],125],
                             44,10," ",
                             [123,["application",44,"app1",44,"\"link1\"",44,"git",44,"[]"],125],
                             44,10," ",
                             [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125]],
                            93],
                           46,10],
            ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", ResultApps])),
            ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/app3"]))
        end}]}.

remove_from_index_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, file]) end,
    [{"test for remove_from_index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),
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

        ?assertEqual({error, "Run `epax init` before running other epax commands"},
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
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),

        ?assertEqual({ok, [app1, app2]}, epax_index:get_applist()),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for get_applist",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),

        ?assertEqual({error, "Run `epax init` before running other epax commands"},
                     epax_index:get_applist()),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end}]}.

check_index_test_() ->
    {foreach,
    fun() -> meck:new([epax_app, epax_os, epax_repo, epax_com, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_app, epax_os, epax_repo, epax_com, file]) end,
    [{"test for check index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]},
                                                              #application{name=app3, repo_link="link3", repo_type=git, details=[]}]]} end),
        meck:expect(file, list_dir, fun("packages") -> {ok, ["app1", "app2","app3", "app4", "temp"]} end),
        meck:expect(epax_repo, update_repo, fun(#application{name=app1, repo_link="link1", repo_type=git, details=[]}) ->
                                                    {ok, #application{name=app1, repo_link="link1", repo_type=git, details=[{key, "value"}]}};
                                               (#application{name=app2, repo_link="link2", repo_type=git, details=[]}) ->
                                                    {ok, #application{name=app2, repo_link="link2", repo_type=git, details=[]}};
                                               (#application{name=app3, repo_link="link3", repo_type=git, details=[]}) ->
                                                    {error, "error"} end),
        meck:expect(epax_com, console, fun(" => ~p checked and updated~n", [app1]) ->
                                              ok;
                                          (" => ~p checked and updated~n", [app3]) ->
                                              ok;
                                          (" => ~p checked and updated~n", [app2]) ->
                                              ok end),
        meck:expect(epax_repo, clone_app, fun("link3", [{repo_type,git}]) -> {ok, #application{name=app3, repo_link="link3", repo_type=git, details=[{k, "v"}]}} end),
        meck:expect(file, write_file, fun("index.cfg", [[91,
                                                         [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,[91,[[123,["k",44,"\"v\""],125]],93]],125],
                                                          44,10," ",
                                                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125],
                                                          44,10," ",
                                                          [123,["application",44,"app1",44,"\"link1\"",44,"git",44,[91,[[123,["key",44,"\"value\""],125]],93]],125]],
                                                         93],
                                                        46,10]) -> ok end),
        meck:expect(epax_os, rmdir, fun("packages/app3") -> ok;
                                       ("packages/app4") -> ok;
                                       ("packages/temp") -> ok end),

        ?assertEqual(ok, epax_index:check_index()),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages"])),
        ?assertEqual(1, meck:num_calls(epax_repo, update_repo, [#application{name=app1, repo_link="link1", repo_type=git, details=[]}])),
        ?assertEqual(1, meck:num_calls(epax_repo, update_repo, [#application{name=app2, repo_link="link2", repo_type=git, details=[]}])),
        ?assertEqual(1, meck:num_calls(epax_repo, update_repo, [#application{name=app3, repo_link="link3", repo_type=git, details=[]}])),
        ?assertEqual(1, meck:num_calls(epax_com, console, [" => ~p checked and updated~n", [app1]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, [" => ~p checked and updated~n", [app2]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, [" => ~p checked and updated~n", [app3]])),
        ?assertEqual(1, meck:num_calls(epax_repo, clone_app, ["link3", [{repo_type,git}]])),
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", [[91,
                                                                         [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,[91,[[123,["k",44,"\"v\""],125]],93]],125],
                                                                          44,10," ",
                                                                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125],
                                                                          44,10," ",
                                                                          [123,["application",44,"app1",44,"\"link1\"",44,"git",44,[91,[[123,["key",44,"\"value\""],125]],93]],125]],
                                                                         93],
                                                                        46,10]])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/app3"])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/app4"])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/temp"]))
    end},
    {"test for check index when writing to index file fails",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]},
                                                              #application{name=app3, repo_link="link3", repo_type=git, details=[]}]]} end),
        meck:expect(file, list_dir, fun("packages") -> {ok, ["app1", "app2","app3", "app4", "temp"]} end),
        meck:expect(epax_repo, update_repo, fun(#application{name=app1, repo_link="link1", repo_type=git, details=[]}) ->
                                                    {ok, #application{name=app1, repo_link="link1", repo_type=git, details=[{key, "value"}]}};
                                               (#application{name=app2, repo_link="link2", repo_type=git, details=[]}) ->
                                                    {ok, #application{name=app2, repo_link="link2", repo_type=git, details=[]}};
                                               (#application{name=app3, repo_link="link3", repo_type=git, details=[]}) ->
                                                    {error, "error"} end),
        meck:expect(epax_com, console, fun(" => ~p checked and updated~n", [app1]) ->
                                               ok;
                                          (" => ~p checked and updated~n", [app3]) ->
                                               ok;
                                          (" => ~p checked and updated~n", [app2]) ->
                                               ok end),
        meck:expect(epax_repo, clone_app, fun("link3", [{repo_type,git}]) -> {ok, #application{name=app3, repo_link="link3", repo_type=git, details=[{k, "v"}]}} end),
        meck:expect(file, write_file, fun("index.cfg", [[91,
                                                         [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,[91,[[123,["k",44,"\"v\""],125]],93]],125],
                                                          44,10," ",
                                                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125],
                                                          44,10," ",
                                                          [123,["application",44,"app1",44,"\"link1\"",44,"git",44,[91,[[123,["key",44,"\"value\""],125]],93]],125]],
                                                         93],
                                                        46,10]) -> {error, "error"} end),
        meck:expect(epax_os, rmdir, fun("packages/app3") -> ok end),

        ?assertEqual({error, "error"}, epax_index:check_index()),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages"])),
        ?assertEqual(1, meck:num_calls(epax_repo, update_repo, [#application{name=app1, repo_link="link1", repo_type=git, details=[]}])),
        ?assertEqual(1, meck:num_calls(epax_repo, update_repo, [#application{name=app2, repo_link="link2", repo_type=git, details=[]}])),
        ?assertEqual(1, meck:num_calls(epax_repo, update_repo, [#application{name=app3, repo_link="link3", repo_type=git, details=[]}])),
        ?assertEqual(1, meck:num_calls(epax_com, console, [" => ~p checked and updated~n", [app1]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, [" => ~p checked and updated~n", [app2]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, [" => ~p checked and updated~n", [app3]])),
        ?assertEqual(1, meck:num_calls(epax_repo, clone_app, ["link3", [{repo_type,git}]])),
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", [[91,
                                                                         [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,[91,[[123,["k",44,"\"v\""],125]],93]],125],
                                                                          44,10," ",
                                                                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125],
                                                                          44,10," ",
                                                                          [123,["application",44,"app1",44,"\"link1\"",44,"git",44,[91,[[123,["key",44,"\"value\""],125]],93]],125]],
                                                                         93],
                                                                        46,10]])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/app3"])),
        ?assertEqual(0, meck:num_calls(epax_os, rmdir, ["packages/app4"])),
        ?assertEqual(0, meck:num_calls(epax_os, rmdir, ["packages/temp"]))
    end},
    {"test for check index when deleting the dir fails",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]},
                                                              #application{name=app3, repo_link="link3", repo_type=git, details=[]}]]} end),
        meck:expect(file, list_dir, fun("packages") -> {ok, ["app1", "app2","app3", "app4", "temp"]} end),
        meck:expect(epax_repo, update_repo, fun(#application{name=app1, repo_link="link1", repo_type=git, details=[]}) ->
                                                    {ok, #application{name=app1, repo_link="link1", repo_type=git, details=[{key, "value"}]}};
                                               (#application{name=app2, repo_link="link2", repo_type=git, details=[]}) ->
                                                    {ok, #application{name=app2, repo_link="link2", repo_type=git, details=[]}};
                                               (#application{name=app3, repo_link="link3", repo_type=git, details=[]}) ->
                                                    {error, "error"} end),
        meck:expect(epax_com, console, fun(" => ~p checked and updated~n", [app1]) ->
                                              ok;
                                          (" ** ~p unable to fix, because ~p~n", [app3, "error"]) ->
                                              ok;
                                          (" => ~p checked and updated~n", [app2]) ->
                                              ok end),
        meck:expect(file, write_file, fun("index.cfg", [[91,
                                                         [[123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125],
                                                          44,10," ",
                                                          [123,["application",44,"app1",44,"\"link1\"",44,"git",44,[91,[[123,["key",44,"\"value\""],125]],93]],125]],
                                                         93],
                                                        46,10]) -> ok end),
        meck:expect(epax_os, rmdir, fun("packages/app3") -> throw("error");
                                       ("packages/app4") -> ok;
                                       ("packages/temp") -> ok end),

        ?assertEqual(ok, epax_index:check_index()),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, list_dir, ["packages"])),
        ?assertEqual(1, meck:num_calls(epax_repo, update_repo, [#application{name=app1, repo_link="link1", repo_type=git, details=[]}])),
        ?assertEqual(1, meck:num_calls(epax_repo, update_repo, [#application{name=app2, repo_link="link2", repo_type=git, details=[]}])),
        ?assertEqual(1, meck:num_calls(epax_repo, update_repo, [#application{name=app3, repo_link="link3", repo_type=git, details=[]}])),
        ?assertEqual(1, meck:num_calls(epax_com, console, [" => ~p checked and updated~n", [app1]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, [" => ~p checked and updated~n", [app2]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, [" ** ~p unable to fix, because ~p~n", [app3,"error"]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, [" => ~p checked and updated~n", [app3]])),
        ?assertEqual(0, meck:num_calls(epax_repo, clone_app, ["link3"])),
        ?assertEqual(0, meck:num_calls(file, write_file, ["index.cfg", [[91,
                                                                         [[123,["application",44,"app3",44,"\"link3\"",44,"git",44,[91,[[123,["k",44,"\"v\""],125]],93]],125],
                                                                          44,10," ",
                                                                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125],
                                                                          44,10," ",
                                                                          [123,["application",44,"app1",44,"\"link1\"",44,"git",44,[91,[[123,["key",44,"\"value\""],125]],93]],125]],
                                                                         93],
                                                                        46,10]])),
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", [[91,
                                                                         [[123,["application",44,"app2",44,"\"link2\"",44,"git",44,"[]"],125],
                                                                          44,10," ",
                                                                          [123,["application",44,"app1",44,"\"link1\"",44,"git",44,[91,[[123,["key",44,"\"value\""],125]],93]],125]],
                                                                         93],
                                                                        46,10]])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/app3"])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/app4"])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["packages/temp"]))
    end},
    {"test for check index when index file not found",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),
        meck:expect(epax_app, init, fun() -> ok end),

        ?assertEqual({error, "error"}, epax_index:check_index()),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"]))
    end}]}.

update_index_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, epax_repo, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, epax_repo, file]) end,
    [{"test for update_index when index cannot be read",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),

        ?assertEqual({error, "Run `epax init` before running other epax commands"},
                     epax_index:update_index()),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"]))
    end},
    {"test for update_index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),
        meck:expect(file, write_file, fun("index.cfg", [[91,
                                                         [[123,["application",44,"app1",44,"\"link1\"",44,"git",44,[91,[[123,["detail1",44,"\"detail1\""],125]],93]],125],
                                                          44,10," ",
                                                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,[91,[[123,["detail2",44,"\"detail2\""],125]],93]],125]],
                                                         93],
                                                        46,10]) -> ok end),
        meck:expect(epax_repo, update_repo, fun(#application{name=app1, repo_link="link1", repo_type=git, details=[]}) ->
                                                    {ok, #application{name=app1, repo_link="link1", repo_type=git, details=[{detail1, "detail1"}]}};
                                               (#application{name=app2, repo_link="link2", repo_type=git, details=[]}) ->
                                                    {ok, #application{name=app2, repo_link="link2", repo_type=git, details=[{detail2, "detail2"}]}} end),

        ?assertEqual(ok ,epax_index:update_index()),
        ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", [[91,
                                                                         [[123,["application",44,"app1",44,"\"link1\"",44,"git",44,[91,[[123,["detail1",44,"\"detail1\""],125]],93]],125],
                                                                          44,10," ",
                                                                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,[91,[[123,["detail2",44,"\"detail2\""],125]],93]],125]],
                                                                         93],
                                                                        46,10]]))
    end},
    {"test for update_index when app1 cannot be updated",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),
        meck:expect(file, write_file, fun("index.cfg", [[91,
                                                         [[123,["application",44,"app1",44,"\"link1\"",44,"git",44,"[]"],125],
                                                          44,10," ",
                                                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,[91,[[123,["detail2",44,"\"detail2\""],125]],93]],125]],
                                                         93],
                                                        46,10]) -> ok end),
        meck:expect(epax_repo, update_repo, fun(#application{name=app1, repo_link="link1", repo_type=git, details=[]}) ->
                                                    {error, "error"};
                                               (#application{name=app2, repo_link="link2", repo_type=git, details=[]}) ->
                                                    {ok, #application{name=app2, repo_link="link2", repo_type=git, details=[{detail2, "detail2"}]}} end),

        ?assertEqual(ok ,epax_index:update_index()),
        ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, consult, ["index.cfg"])),
        ?assertEqual(1, meck:num_calls(file, write_file, ["index.cfg", [[91,
                                                                         [[123,["application",44,"app1",44,"\"link1\"",44,"git",44,"[]"],125],
                                                                          44,10," ",
                                                                          [123,["application",44,"app2",44,"\"link2\"",44,"git",44,[91,[[123,["detail2",44,"\"detail2\""],125]],93]],125]],
                                                                         93],
                                                                        46,10]]))
    end}]}.


search_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, epax_com, epax_app, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, epax_com, epax_app, file]) end,
    [{"test for search index",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[]}]]} end),
        meck:expect(epax_com, success, fun("=== Erlang Apps ===~n  - ~p", [app1]) -> ok end),
        meck:expect(epax_com, console, fun("  - ~p~n", [app2]) -> ok;
                                          ("====================~n", []) -> ok end),
        ?assertEqual(ok, epax_index:search("app", [])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["=== Erlang Apps ===~n  - ~p", [app1]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["  - ~p~n", [app2]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["====================~n", []]))
    end},
    {"test for search index when searched through description",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[{description, "app"}]}]]} end),
        meck:expect(epax_com, success, fun("=== Erlang Apps ===~n  - ~p", [app1]) -> ok end),
        meck:expect(epax_com, console, fun("====================~n", []) -> ok end),
        ?assertEqual(ok, epax_index:search("erlang", [])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["=== Erlang Apps ===~n  - ~p", [app1]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["  - ~p~n", [app2]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["====================~n", []]))
    end},
    {"test for search index when no app found",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[{description, "app"}]}]]} end),
        meck:expect(epax_com, success, fun("no package found") -> ok end),
        meck:expect(epax_com, console, fun("====================~n", []) -> ok end),
        ?assertEqual(ok, epax_index:search("erlang*app", [])),
        ?assertEqual(0, meck:num_calls(epax_com, success, ["=== Erlang Apps ===~n  - ~p", [app1]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["  - ~p~n", [app2]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["====================~n", []])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["no package found"]))
    end},
    {"test for search index when only names searched",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[{description, "app"}]}]]} end),
        meck:expect(epax_com, success, fun("no package found") -> ok end),
        meck:expect(epax_com, console, fun("====================~n", []) -> ok end),
        ?assertEqual(ok, epax_index:search("erlang", [{names_only, true}])),
        ?assertEqual(0, meck:num_calls(epax_com, success, ["=== Erlang Apps ===~n  - ~p", [app1]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["  - ~p~n", [app2]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["====================~n", []])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["no package found"]))
    end},
    {"test for search index when only names searched II",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=app1, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[{description, "app"}]}]]} end),
        meck:expect(epax_com, success, fun("no package found") -> ok end),
        meck:expect(epax_com, console, fun("====================~n", []) -> ok end),
        ?assertEqual(ok, epax_index:search("er.*", [{names_only, true}])),
        ?assertEqual(0, meck:num_calls(epax_com, success, ["=== Erlang Apps ===~n  - ~p", [app1]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["  - ~p~n", [app2]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["====================~n", []])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["no package found"]))
    end},
    {"test for search index when only names searched III",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=epax, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[{description, "alternative of epax"}]}]]} end),
        meck:expect(epax_com, success, fun("=== Erlang Apps ===~n  - ~p", [epax]) -> ok end),
        meck:expect(epax_com, console, fun("====================~n", []) -> ok end),
        ?assertEqual(ok, epax_index:search("epax", [{names_only, true}])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["=== Erlang Apps ===~n  - ~p", [epax]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["  - ~p~n", [app2]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["====================~n", []]))
    end},
    {"test for search index when full description printed",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=epax, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[{description, "alternative of epax"}]}]]} end),
        meck:expect(epax_app, format_app, fun(#application{name=epax, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]}) -> "formatted epax" end),
        meck:expect(epax_com, success, fun("~s", ["formatted epax"]) -> ok end),
        meck:expect(epax_com, console, fun("====================~n", []) -> ok end),
        ?assertEqual(ok, epax_index:search("epax", [{names_only, true}, {full, true}])),
        ?assertEqual(1, meck:num_calls(epax_app, format_app, [#application{name=epax, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]}])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s", ["formatted epax"]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["====================~n", []]))
    end},
    {"test for search index when full description printed",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {ok, [[#application{name=epax, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]},
                                                              #application{name=app2, repo_link="link2", repo_type=git, details=[{description, "alternative of epax"}]}]]} end),
        meck:expect(epax_app, format_app, fun(#application{name=epax, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]}) -> "formatted epax";
                                             (#application{name=app2, repo_link="link2", repo_type=git, details=[{description, "alternative of epax"}]}) -> "app2 formatted" end),
        meck:expect(epax_com, success, fun("~s", ["formatted epax"]) -> ok end),
        meck:expect(epax_com, console, fun("~s~n", ["app2 formatted"]) -> ok;
                                          ("====================~n", []) -> ok end),
        ?assertEqual(ok, epax_index:search("epax", [{full, true}])),
        ?assertEqual(1, meck:num_calls(epax_app, format_app, [#application{name=epax, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]}])),
        ?assertEqual(1, meck:num_calls(epax_app, format_app, [#application{name=app2, repo_link="link2", repo_type=git, details=[{description, "alternative of epax"}]}])),
        ?assertEqual(1, meck:num_calls(epax_com, success, ["~s", ["formatted epax"]])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["====================~n", []])),
        ?assertEqual(1, meck:num_calls(epax_com, console, ["~s~n", ["app2 formatted"]]))
    end},
    {"test for search index when index file is not found",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(file, consult, fun("index.cfg") -> {error, "error"} end),

        ?assertEqual({error, "Run `epax init` before running other epax commands"},
                     epax_index:search("epax", [{full, true}])),
        ?assertEqual(0, meck:num_calls(epax_app, format_app, [#application{name=epax, repo_link="link1", repo_type=git, details=[{description, "erlang app"}]}])),
        ?assertEqual(0, meck:num_calls(epax_app, format_app, [#application{name=app2, repo_link="link2", repo_type=git, details=[{description, "alternative of epax"}]}])),
        ?assertEqual(0, meck:num_calls(epax_com, success, ["~s", ["formatted epax"]])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["====================~n", []])),
        ?assertEqual(0, meck:num_calls(epax_com, console, ["~s~n", ["app2 formatted"]]))
    end}]}.
