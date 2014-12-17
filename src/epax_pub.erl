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
%%% @doc main epax application module
%%%

-module(epax_pub).
-include("epax.hrl").
-export([init/0,
         pub_exists/1,
         add_pub/2]).


%%============================================================================
%% API
%%============================================================================

%% init/0
%% ====================================================================
%% @doc initializes publisher index
-spec init() -> ok.
%% ====================================================================
init() ->
    epax_os:touch(epax_os:get_abs_path("publisher.cfg")),
    write_to_publisher_file([]),
    epax_os:mkdir(epax_os:get_abs_path("publisher")),
    epax_os:rmdir(epax_os:get_abs_path("publisher/*")),
    ok.

%% add_pub/2
%% ====================================================================
%% @doc adds a publisher into the publisher index
-spec add_pub(Link, Options) -> Result when
    Link      :: string(),
    Options   :: [term()],
    Result    :: {ok, Publisher}
               | {error, Reason},
    Publisher :: atom(),
    Reason    :: term().
%% ====================================================================
add_pub(_Link, _Options) ->
    {error, reason}.

%% exists/1
%% ====================================================================
%% @doc check whether a publisher already exists in publisher index
-spec pub_exists(Info) -> Result when
    Info   :: atom()
            | string(),
    Result :: {ok, false}
            | {ok, Pub}
            | {error, Reason},
    Pub    :: atom(),
    Reason :: term().
%% ====================================================================
pub_exists(Info) ->
    case file:consult(epax_os:get_abs_path("publisher.cfg")) of
        {ok, [ExistingPubs]} ->
            {ok, pub_exists(Info, ExistingPubs)};
        {error, _} ->
            {error, "Run `epax init` before running other epax commands"}
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

write_to_publisher_file(Data) ->
    file:write_file(epax_os:get_abs_path("publisher.cfg"), io_lib:fwrite("~p.\n", [Data])).

pub_exists(Info, ExistingPubs) when is_list(Info) ->
    case lists:keyfind(Info, #publisher.index_link, ExistingPubs) of
        false ->
            false;
        App ->
            App#publisher.name
    end;
pub_exists(Info, ExistingPubs) when is_atom(Info) ->
    case lists:keyfind(Info, #publisher.name, ExistingPubs) of
        false ->
            false;
        App ->
            App#publisher.name
    end;
pub_exists(_, _) ->
    false.
