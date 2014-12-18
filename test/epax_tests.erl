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
%%% @doc main epax test module
%%%

-module(epax_tests).
-include_lib("eunit/include/eunit.hrl").
-include("epax.hrl").

% help messages
-define(CONFIG_OPT, [
					 %{Name,       ShortOpt,  LongOpt,       ArgSpec,     HelpMsg}
					 {help,        $h,        "help",        undefined,   "Show the config subcommand options"},
					 {username,    undefined, "user-name",   string,      "Change user name, must set before using epax"},
					 {useremail,   undefined, "user-email",  string,      "Change user email, must set before using epax"},
					 {indexpath,   undefined, "index-path",  string,      "Change default path for keeping index and packages"}
					]
	   ).

main_test_() ->
	{foreach,
	 fun() ->
			 meck:new([])
	 end,
	 fun(_) ->
			 meck:unload()
	 end,
	 [{"test for empty command",
	   fun() ->
			   ?assertEqual(ok, epax:main([]))
	   end}
	 ]
	}.

main_init_test_() ->
	{foreach,
	 fun() ->
			 meck:new([epax_app], [non_strict])
	 end,
	 fun(_) ->
			 meck:unload([epax_app])
	 end,
	 [{"test for init command",
	   fun() ->
			   meck:expect(epax_app, init, fun() -> ok end),
			   ?assertEqual(ok, epax:main(["init"])),
			   ?assertEqual(1, meck:num_calls(epax_app, init, [])),
			   ?assert(meck:validate(epax_app))
	   end}
	 ]
	}.

main_config_test_() ->
	{foreach,
	 fun() ->
			 meck:new([epax_config, getopt], [non_strict])
	 end,
	 fun(_) ->
			 meck:unload([epax_config, getopt])
	 end,
	 [{"test for config command (help)",
	   fun() ->
			   meck:expect(getopt, parse, fun(?CONFIG_OPT, ["-h"]) -> meck:passthrough([?CONFIG_OPT, ["-h"]]) end),
			   meck:expect(getopt, usage, fun(?CONFIG_OPT, ?EPAX) -> meck:passthrough([?CONFIG_OPT, ?EPAX]) end),
			   ?assertEqual(ok, epax:main(["config", "-h"])),
			   ?assertEqual(1, meck:num_calls(getopt, parse, [?CONFIG_OPT, ["-h"]])),
			   ?assertEqual(1, meck:num_calls(getopt, usage, [?CONFIG_OPT, ?EPAX])),
			   ?assert(meck:validate(getopt))
	   end},
	  {"test for config command, option email",
	   fun() ->
			   meck:expect(getopt, parse, fun(?CONFIG_OPT, ["--user-email", "test@test-epax.com"]) ->
												  meck:passthrough([?CONFIG_OPT, ["--user-email", "test@test-epax.com"]])
						   end),
			   meck:expect(epax_config, set, fun([{useremail, "test@test-epax.com"}]) -> ok end),
			   ?assertEqual(ok, epax:main(["config", "--user-email", "test@test-epax.com"])),
			   ?assertEqual(1, meck:num_calls(getopt, parse, [?CONFIG_OPT, ["--user-email", "test@test-epax.com"]])),
			   ?assertEqual(0, meck:num_calls(getopt, usage, [?CONFIG_OPT, ?EPAX])),
			   ?assertEqual(1, meck:num_calls(epax_config, set, [[{useremail, "test@test-epax.com"}]])),
			   ?assert(meck:validate([epax_config, getopt]))
	   end},
	  {"test for config command, option name",
	   fun() ->
			   meck:expect(getopt, parse, fun(?CONFIG_OPT, ["--user-name", "test name"]) ->
												  meck:passthrough([?CONFIG_OPT, ["--user-name", "test name"]])
						   end),
			   meck:expect(epax_config, set, fun([{username, "test name"}]) -> ok end),
			   ?assertEqual(ok, epax:main(["config", "--user-name", "test name"])),
			   ?assertEqual(1, meck:num_calls(getopt, parse, [?CONFIG_OPT, ["--user-name", "test name"]])),
			   ?assertEqual(0, meck:num_calls(getopt, usage, [?CONFIG_OPT, ?EPAX])),
			   ?assertEqual(1, meck:num_calls(epax_config, set, [[{username, "test name"}]])),
			   ?assert(meck:validate([epax_config, getopt]))
	   end},
	  {"test for config command, option path",
	   fun() ->
			   meck:expect(getopt, parse, fun(?CONFIG_OPT, ["--index-path", "/weird/path/to/index"]) ->
												  meck:passthrough([?CONFIG_OPT, ["--index-path", "/weird/path/to/index"]])
						   end),
			   meck:expect(epax_config, set, fun([{indexpath, "/weird/path/to/index"}]) -> ok end),
			   ?assertEqual(ok, epax:main(["config", "--index-path", "/weird/path/to/index"])),
			   ?assertEqual(1, meck:num_calls(getopt, parse, [?CONFIG_OPT, ["--index-path", "/weird/path/to/index"]])),
			   ?assertEqual(0, meck:num_calls(getopt, usage, [?CONFIG_OPT, ?EPAX])),
			   ?assertEqual(1, meck:num_calls(epax_config, set, [[{indexpath, "/weird/path/to/index"}]])),
			   ?assert(meck:validate([epax_config, getopt]))
	   end},
	  {"test for config command, all options",
	   fun() ->
			   meck:expect(getopt, parse, fun(?CONFIG_OPT, ["--index-path", "/weird/path/to/index", "--user-name", "myname", "--user-email", "weird@awesome.com"]) ->
												  meck:passthrough([?CONFIG_OPT, ["--index-path", "/weird/path/to/index", "--user-name", "myname", "--user-email", "weird@awesome.com"]])
						   end),
			   meck:expect(epax_config, set, fun([{indexpath, "/weird/path/to/index"},
												  {username, "myname"},
												  {useremail, "weird@awesome.com"}]) -> ok end),
			   ?assertEqual(ok, epax:main(["config", "--index-path", "/weird/path/to/index", "--user-name", "myname", "--user-email", "weird@awesome.com"])),
			   ?assertEqual(1, meck:num_calls(getopt, parse, [?CONFIG_OPT, ["--index-path", "/weird/path/to/index", "--user-name", "myname", "--user-email", "weird@awesome.com"]])),
			   ?assertEqual(0, meck:num_calls(getopt, usage, [?CONFIG_OPT, ?EPAX])),
			   ?assertEqual(1, meck:num_calls(epax_config, set, [[{indexpath, "/weird/path/to/index"},
																  {username, "myname"},
																  {useremail, "weird@awesome.com"}]])),
			   ?assert(meck:validate([epax_config, getopt]))
	   end},
	  {"test for config command, all options-2",
	   fun() ->
			   meck:expect(getopt, parse, fun(?CONFIG_OPT, ["--index-path", "/weird/path/to/index", "--user-name", "myname"]) ->
												  meck:passthrough([?CONFIG_OPT, ["--index-path", "/weird/path/to/index", "--user-name", "myname"]])
						   end),
			   meck:expect(epax_config, set, fun([{indexpath, "/weird/path/to/index"},
												  {username, "myname"}]) -> ok end),
			   ?assertEqual(ok, epax:main(["config", "--index-path", "/weird/path/to/index", "--user-name", "myname"])),
			   ?assertEqual(1, meck:num_calls(getopt, parse, [?CONFIG_OPT, ["--index-path", "/weird/path/to/index", "--user-name", "myname"]])),
			   ?assertEqual(0, meck:num_calls(getopt, usage, [?CONFIG_OPT, ?EPAX])),
			   ?assertEqual(1, meck:num_calls(epax_config, set, [[{indexpath, "/weird/path/to/index"},
																  {username, "myname"}]])),
			   ?assert(meck:validate([epax_config, getopt]))
	   end}
	 ]
	}.
