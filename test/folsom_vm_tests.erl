%%-------------------------------------------------------------------
%%% File:      folsom_vm_tests.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% tests for _system, _statistics, _memory
%%% @end
%%%------------------------------------------------------------------

-module(folsom_vm_tests).

-include_lib("eunit/include/eunit.hrl").

-export([run/0]).

-define(SYSTEM_URL, "http://localhost:5555/_system").
-define(STATISTICS_URL, "http://localhost:5555/_statistics").
-define(MEMORY_URL, "http://localhost:5555/_memory").

run() ->
    system_checks(),
    statistics_checks(),
    memory_checks().

system_checks() ->
    % check _system stats
    {"200", _, Body1} = http_helpers:http_get(?SYSTEM_URL),
    {struct, List1} = mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"allocated_areas">>, 1, List1).

statistics_checks() ->
    % check _statistics stats
    {"200", _, Body1} = http_helpers:http_get(?STATISTICS_URL),
    {struct, List1} = mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"context_switches">>, 1, List1).

memory_checks() ->
    % check _memory stats
    {"200", _, Body1} = http_helpers:http_get(?MEMORY_URL),
    {struct, List1} = mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"total">>, 1, List1).
