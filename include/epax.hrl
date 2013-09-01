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

% application details
-define(EPAX, "epax").
-define(VERSION, "1.0.0").

% index entry record
-record(application, {name, repo_link, repo_type, details}).

% macro to format string
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

% standard application list
-define(STAND_APPS, [compiler,
                     erts,
                     kernel,
                     sasl,
                     stdlib,
                     mnesia,
                     odbc,
                     os_mon,
                     otp_mibs,
                     snmp,
                     asn1,
                     crypto,
                     diameter,
                     eldap,
                     erl_interface,
                     gs,
                     inets,
                     jinterface,
                     megaco,
                     public_key,
                     ssh,
                     ssl,
                     wx,
                     xmerl,
                     appmon,
                     debugger,
                     et,
                     observer,
                     parsetools,
                     percept,
                     pman,
                     reltool,
                     runtime_tools,
                     syntax_tools,
                     toolbar,
                     tools,
                     tv,
                     webtool,
                     hipe]).
