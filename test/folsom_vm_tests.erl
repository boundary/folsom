%%%
%%% Copyright 2011, fast_ip
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%


%%%-------------------------------------------------------------------
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

-define(SYSTEM_URL, "http://localhost:5565/_system").
-define(STATISTICS_URL, "http://localhost:5565/_statistics").
-define(MEMORY_URL, "http://localhost:5565/_memory").

run() ->
    system_checks(),
    statistics_checks(),
    memory_checks().

system_checks() ->
    % check _system stats
    Body1 = http_helpers:http_get(?SYSTEM_URL),
    {struct, List1} = mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"allocated_areas">>, 1, List1).

statistics_checks() ->
    % check _statistics stats
    Body1 = http_helpers:http_get(?STATISTICS_URL),
    {struct, List1} = mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"context_switches">>, 1, List1).

memory_checks() ->
    % check _memory stats
    Body1 = http_helpers:http_get(?MEMORY_URL),
    {struct, List1} = mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"total">>, 1, List1).
