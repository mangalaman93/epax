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

-module(epax_pub_tests).
-include("epax.hrl").
-include_lib("eunit/include/eunit.hrl").

init_test_() ->
    {foreach,
    fun() -> meck:new([epax_os, file], [unstick, passthrough]) end,
    fun(_) -> meck:unload([epax_os, file]) end,
    [{"test for init function",
    fun() ->
        meck:expect(epax_os, get_abs_path, fun(X) -> X end),
        meck:expect(epax_os, touch, fun("publisher.cfg") -> ok end),
        meck:expect(epax_os, mkdir, fun("publisher") -> ok end),
        meck:expect(epax_os, rmdir, fun("publisher/*") -> ok end),
        meck:expect(file, write_file, fun("publisher.cfg", ["[]",46,10]) -> ok end),

        ?assertEqual(ok, epax_pub:init()),
        ?assertEqual(2, meck:num_calls(epax_os, get_abs_path, ["publisher.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_os, touch, ["publisher.cfg"])),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["publisher"])),
        ?assertEqual(1, meck:num_calls(epax_os, mkdir, ["publisher"])),
        ?assertEqual(1, meck:num_calls(epax_os, get_abs_path, ["publisher/*"])),
        ?assertEqual(1, meck:num_calls(epax_os, rmdir, ["publisher/*"])),
        ?assertEqual(1, meck:num_calls(file, write_file, ["publisher.cfg", ["[]",46,10]]))
    end}]}.
