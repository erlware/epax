%% -*- erlang-indent-level: 4;
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%% ---------------------------------------------------------------------------
%% @author Aman Mangal <mangalaman93@gmail.com>
%% @copyright (C) 2012 Erlware, LLC.

% macro to fromat string
-define(FMT(String, Args), lists:flatten(io_lib:format(String, Args))).

% success macro
-define(SUCCESS(Message), epax_com:print_success(Message)).
-define(SUCCESS(Message, Args), epax_com:print_success(?FMT(Message, Args))).

% error macro
-define(ERROR(Reason, Conclusion, Args), epax_com:print_error(Reason, ?FMT(Conclusion, Args))).
-define(ERROR(Reason, Conclusion), epax_com:print_error(Reason, Conclusion)).

% console output
-define(CONSOLE(Message, Args), io:format(Message, Args)).

% abort the process
-define(ABORT(Reason, Conclusion, Args), throw(Reason)).

% other macros
-define(STAND_APPS, [sasl, ssl, stdlib, kernel]).
