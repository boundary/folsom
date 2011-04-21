-module(folsom_http_checks).

-include_lib("eunit/include/eunit.hrl").

-export([run/0]).

-define(SYSTEM_URL, "http://localhost:5565/_system").
-define(STATISTICS_URL, "http://localhost:5565/_statistics").
-define(MEMORY_URL, "http://localhost:5565/_memory").
-define(BASE_METRICS_URL, "http://localhost:5565/_metrics").

run() ->
    metrics_checks(),
    system_checks(),
    statistics_checks(),
    memory_checks().

metrics_checks() ->
    Body1 = http_helpers:http_get(?BASE_METRICS_URL),
    List1 = mochijson2:decode(Body1),
    true = lists:member(<<"counter">>, List1),

    Url1 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", counter])),
    Body2 = http_helpers:http_get(Url1),
    0 = mochijson2:decode(Body2).

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

