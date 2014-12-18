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
%%% @doc main epax module
%%%

-module(epax).
-include("epax.hrl").
-export([main/1]).


%%============================================================================
%% API
%%============================================================================

%% main/1
%% ====================================================================
main([]) ->
    print_help();
main(["init"]) ->
    epax_app:init();
main(["config"|Args]) ->
    handle_po_for_config(getopt:parse(option_spec_list_for_config(), Args));
main(["add"|Args]) ->
    handle_po_for_add(getopt:parse(option_spec_list_for_add(), Args));
main(["list"]) ->
    epax_app:list_apps();
main(["remove"|[Appname]]) ->
    epax_app:remove_app(erlang:list_to_atom(Appname));
main(["check"]) ->
    epax_app:check();
main(["bundle"|[Appname]]) ->
    epax_app:bundle(erlang:list_to_atom(Appname));
main(["show"|[Appname]]) ->
    epax_app:show(erlang:list_to_atom(Appname));
main(["search"|Args]) ->
  handle_po_for_search(getopt:parse(option_spec_list_for_search(), Args));
main(["publisher"|Args]) ->
    handle_publisher_args(Args);
main(["update"]) ->
    epax_app:update();
main(Args) ->
    handle_po(getopt:parse(option_spec_list(), Args)).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

print_help() ->
    HelpMessage = << <<?EPAX>>/binary, <<" (Erlang Package Manager) version ">>/binary, <<?VERSION>>/binary, <<"
Usage: ">>/binary, <<?EPAX>>/binary, <<" command [subcmd] [options]

Commands:
  init               Initialize the index, deletes old index or packages if any
  config [subcmd]    Change default configuration for epax
  add    <link>      Add new package into index (repo must follow OTP structure)
  list               List down all packages in the index in lexicographical order
  remove <appname>   Remove the package from index
  check              Try to fix broken packages if any, updates the index as well
  bundle <appname>   Compute and copy non-standard dependencies for the pacakge
  show   <appname>   Print detailed information of the package
  search <regex>     Perform full text search on available package lists
  publisher [subcmd] Carry out publisher related operations
  update             Update details of all packages in the index

Options:
  -h, --help         Show the commands and options (this message)
  -v, --version      Show the current version

">>/binary>>,
    io:put_chars(HelpMessage).

print_version() ->
    VersionMessage = << <<?EPAX>>/binary, <<" (Erlang Package Manager) version ">>/binary, <<?VERSION>>/binary, <<"
">>/binary >>,
    io:put_chars(VersionMessage).

% main options
option_spec_list() ->
	[
     %{Name,       ShortOpt,  LongOpt,       ArgSpec,     HelpMsg}
     {help,        $h,        "help",        undefined,   "Show the program options"},
     {version,     $v,        "version",     undefined,   "Show the current version"}
    ].

handle_options([help]) ->
    print_help();
handle_options([version]) ->
    print_version();
handle_options(Options) ->
    epax_com:console("** Invalid options: ~p.~n", [Options]),
    print_help().

handle_po({ok, {Options, []}}) ->
    handle_options(Options);
handle_po({ok, {_, NonOptArgs}}) ->
    epax_com:console("** Invalid non option arguments: ~p.~n~n", [NonOptArgs]),
    print_help();
handle_po({error, {Reason, Data}}) ->
    epax_com:console("** Error: ~s ~p.~n", [Reason, Data]),
    print_help().

% config subcommand
option_spec_list_for_config() ->
	[
     %{Name,       ShortOpt,  LongOpt,       ArgSpec,     HelpMsg}
     {help,        $h,        "help",        undefined,   "Show the config subcommand options"},
     {username,    undefined, "user-name",   string,      "Change user name, must set before using epax"},
	 {useremail,   undefined, "user-email",  string,      "Change user email, must set before using epax"},
	 {indexpath,   undefined, "index-path",  string,      "Change default path for keeping index and packages"}
    ].

print_help_for_config() ->
	getopt:usage(option_spec_list_for_config(), ?EPAX).

handle_po_for_config({ok, {[help], []}}) ->
    print_help_for_config();
handle_po_for_config({ok, {Options, []}}) ->
    epax_config:set(Options);
handle_po_for_config({ok, {_, NonOptArgs}}) ->
    epax_com:console("** Invalid non option arguments: ~p.~n~n", [NonOptArgs]),
    print_help_for_config();
handle_po_for_config({error, {Reason, Data}}) ->
    epax_com:console("** Error: ~s ~p.~n", [Reason, Data]),
    print_help_for_config().

% help subcommand add
option_spec_list_for_add() ->
    [
     %{Name,       ShortOpt,  LongOpt,       ArgSpec,     HelpMsg}
     {help,        $h,        "help",        undefined,   "Show the add subcommand options"},
     {repotype,    $r,        "repo",        string,      "Specify type of repository (git, bzr, svn)"}
    ].

print_help_for_add() ->
    getopt:usage(option_spec_list_for_add(), ?EPAX).

handle_po_for_add({ok, {[help], []}}) ->
    print_help_for_add();
handle_po_for_add({ok, {_, []}}) ->
    epax_com:console("** Invalid command (package location required).~n~n", []),
    print_help_for_add();
handle_po_for_add({ok, {Options, [Link]}}) ->
    epax_app:add_app(Link, Options);
handle_po_for_add({ok, {_, NonOptArgs}}) ->
    epax_com:console("** Invalid non option arguments: ~p.~n~n", [NonOptArgs]),
    print_help_for_add();
handle_po_for_add({error, {Reason, Data}}) ->
    epax_com:console("** Error: ~s ~p.~n", [Reason, Data]),
    print_help_for_add().

% help subcommand search
option_spec_list_for_search() ->
    [
     %{Name,       ShortOpt,  LongOpt,       ArgSpec,     HelpMsg}
     {help,        $h,        "help",        undefined,   "Show the search subcommand options"},
     {namesonly,   $n,        "names-only",  undefined,   "Search only package names (default is full search)"},
     {full,        $f,        "full",        undefined,   "Show output identical to `show`, for each matched package"},
	 {publisher,   $p,        "publisher",   string,      "Search packages only from the publisher"}
    ].

print_help_for_search() ->
    getopt:usage(option_spec_list_for_search(), ?EPAX).

handle_po_for_search({ok, {[help], []}}) ->
    print_help_for_search();
handle_po_for_search({ok, {_, []}}) ->
    epax_com:console("** Invalid command (regex required).~n~n", []),
    print_help_for_search();
handle_po_for_search({ok, {Options, [Regex]}}) ->
    epax_index:search(Regex, Options);
handle_po_for_search({ok, {_, NonOptArgs}}) ->
    epax_com:console("** Invalid non option arguments: ~p.~n~n", [NonOptArgs]),
    print_help_for_search();
handle_po_for_search({error, {Reason, Data}}) ->
    epax_com:console("** Error: ~s ~p.~n", [Reason, Data]),
    print_help_for_search().

% publisher subcommand
print_help_for_publisher() ->
	HelpMessagePublisherCmd = << <<?EPAX>>/binary, <<" (Erlang Package Manager) version ">>/binary, <<?VERSION>>/binary, <<"
Usage: ">>/binary, <<?EPAX>>/binary, <<" publisher [subcmd] [options]

SubCommands:
  add    <link>      Add publisher to index
  remove <publisher> Remove publisher from index
  search <regex>     Perform text search on publishers

Options:
  -h, --help         Show the sub-commands and options (this message)

">>/binary>>,
    io:put_chars(HelpMessagePublisherCmd).

option_spec_list_for_publisher() ->
	[
	 %{Name,       ShortOpt,  LongOpt,       ArgSpec,     HelpMsg}
     {help,        $h,        "help",        undefined,   "Show the publisher subcommand options"}
    ].

handle_po_for_publisher({ok, {[help], []}}) ->
	print_help_for_publisher();
handle_po_for_publisher({ok, {Options, []}}) ->
	epax_com:console("** Invalid option arguments: ~p.~n~n", [Options]),
	print_help_for_publisher();
handle_po_for_publisher({ok, {_, NonOptArgs}}) ->
	epax_com:console("** Invalid non option arguments: ~p.~n~n", [NonOptArgs]),
	print_help_for_publisher();
handle_po_for_publisher({error, {Reason, Data}}) ->
    epax_com:console("** Error: ~s ~p.~n", [Reason, Data]),
	print_help_for_publisher().

handle_publisher_args(["add", Link]) ->
	epax_pub:add(Link);
handle_publisher_args(["remove", PubName]) ->
	epax_pub:remove(PubName);
handle_publisher_args(["search", Regex]) ->
	epax_pub:search(Regex);
handle_publisher_args(Args) ->
	handle_po_for_publisher(getopt:parse(option_spec_list_for_publisher(), Args)).
