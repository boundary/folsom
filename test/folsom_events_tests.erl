%%-------------------------------------------------------------------
%%% File:      folsom_events_tests.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% tests for _events
%%% @end
%%%------------------------------------------------------------------

-module(folsom_events_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BASE_EVENTS_URL, "http://localhost:5555/_events").
-define(RAND, 9999999999).
-define(DEFAULT_SIZE, 5).

-export([run/0]).


run() ->
    events_populate(),
    base_events_checks(),
    base_events_info_checks(),
    individual_events_checks(),
    tagged_handlers_checks(),
    event_limits_checks(),
    event_tags_checks(),
    delete_events_checks().

events_populate() ->
    % create a event handler
    create_event(a, ?DEFAULT_SIZE, ["a", "taco"]),
    % populate handler 'a' with values
    populate_event(a, 10),

    % create a event handler
    create_event(b, ?DEFAULT_SIZE, ["b", "taco"]),
    % populate handler 'b' with values
    populate_event(b, 10),

    % create a event handler
    create_event(c, ?DEFAULT_SIZE, ["c", "taco"]),
    % populate handler 'c' with values
    populate_event(c, 10).

base_events_checks() ->
    % check _events list
    Body1 = http_helpers:http_get(?BASE_EVENTS_URL),
    List1 = mochijson2:decode(Body1),

    % make sure list length is 3 since we created a, b and c
    3 = length(List1).

base_events_info_checks() ->
    % check _events?info=true list
    Url1 = lists:append(io_lib:format("~s~s", [?BASE_EVENTS_URL, "?info=true"])),
    Body1 = http_helpers:http_get(Url1),
    [{struct, C}, {struct, B}, {struct, A}] = mochijson2:decode(Body1),

    % make sure the keys we setup are in there
    true = lists:keymember(<<"a">>, 1, A),
    true = lists:keymember(<<"b">>, 1, B),
    true = lists:keymember(<<"c">>, 1, C).

individual_events_checks() ->
    % build events urls, _events/a, etc
    Url1 = lists:append(io_lib:format("~s~s~p", [?BASE_EVENTS_URL, "/", a])),
    Body1 = http_helpers:http_get(Url1),
    {struct, List1} = mochijson2:decode(Body1),

    Url2 = lists:append(io_lib:format("~s~s~p", [?BASE_EVENTS_URL, "/", b])),
    Body2 = http_helpers:http_get(Url2),
    {struct, List2} = mochijson2:decode(Body2),

    Url3 = lists:append(io_lib:format("~s~s~p", [?BASE_EVENTS_URL, "/", c])),
    Body3 = http_helpers:http_get(Url3),
    {struct, List3} = mochijson2:decode(Body3),

    % check each handler for events
    ?DEFAULT_SIZE = length(List1),
    ?DEFAULT_SIZE = length(List2),
    ?DEFAULT_SIZE = length(List3).

tagged_handlers_checks() ->
    % check _events?tag=taco for 3 items
    Url1 = lists:append(io_lib:format("~s~s", [?BASE_EVENTS_URL, "?tag=taco"])),
    Body1 = http_helpers:http_get(Url1),
    List1 = mochijson2:decode(Body1),

    % make sure there are 3 items with taco as a tag
    3 = length(List1),

    % build some urls to make tag requests on
    Url2 = lists:append(io_lib:format("~s~s", [?BASE_EVENTS_URL, "?tag=a"])),
    Url3 = lists:append(io_lib:format("~s~s", [?BASE_EVENTS_URL, "?tag=b"])),
    Url4 = lists:append(io_lib:format("~s~s", [?BASE_EVENTS_URL, "?tag=c"])),

    Body2 = http_helpers:http_get(Url2),
    List2 = mochijson2:decode(Body2),
    1 = length(List2),

    Body3 = http_helpers:http_get(Url3),
    List3 = mochijson2:decode(Body3),
    1 = length(List3),

    Body4 = http_helpers:http_get(Url4),
    List4 = mochijson2:decode(Body4),
    1 = length(List4).

event_limits_checks() ->
    % check _events/a?limit=1
    Url1 = lists:append(io_lib:format("~s~s~p~s", [?BASE_EVENTS_URL, "/", a, "?limit=1"])),
    Body1 = http_helpers:http_get(Url1),
    {struct, List1} = mochijson2:decode(Body1),

    % make sure there is 1 item when limit=1 is used
    1 = length(List1),

    % check _events/b?limit=5
    Url2 = lists:append(io_lib:format("~s~s~p~s", [?BASE_EVENTS_URL, "/", b, "?limit=5"])),
    Body2 = http_helpers:http_get(Url2),
    {struct, List2} = mochijson2:decode(Body2),

    % make sure there are 5 items when limit=5 is used
    5 = length(List2).

event_tags_checks() ->
    % check _events/a?tag=error
    Url1 = lists:append(io_lib:format("~s~s~p~s", [?BASE_EVENTS_URL, "/", a, "?tag=error"])),
    Body1 = http_helpers:http_get(Url1),
    {struct, List1} = mochijson2:decode(Body1),

    % make sure there are 5 events with the tag error
    ?DEFAULT_SIZE = length(List1),

    % check _events/a?tag=error&limit=1
    Url2 = lists:append(io_lib:format("~s~s~p~s", [?BASE_EVENTS_URL, "/", a, "?tag=error&limit=1"])),
    Body2 = http_helpers:http_get(Url2),
    {struct, List2} = mochijson2:decode(Body2),

    % make sure there are 1 events with the tag error
    1 = length(List2).

delete_events_checks() ->
    Url1 = lists:append(io_lib:format("~s~s~p", [?BASE_EVENTS_URL, "/", a])),
    ok = http_helpers:http_delete(Url1),

    Url2 = lists:append(io_lib:format("~s~s~p", [?BASE_EVENTS_URL, "/", b])),
    ok = http_helpers:http_delete(Url2),

    Url3 = lists:append(io_lib:format("~s~s~p", [?BASE_EVENTS_URL, "/", c])),
    ok = http_helpers:http_delete(Url3),

    % check _events list
    Body1 = http_helpers:http_get(?BASE_EVENTS_URL),
    List = mochijson2:decode(Body1),

    % make sure list length is 3 since we created a, b and c
    0 = length(List).

% internal functions

create_event(Name, Size, Tags) ->
    Proplist = [
                {id, Name},
                {size, Size},
                {tags, Tags}
               ],
    Body = mochijson2:encode(Proplist),
    ok = http_helpers:http_put(?BASE_EVENTS_URL, Body).

populate_event(Name, Count) ->
    Values = get_sample_values(Count),
    Url = lists:append(io_lib:format("~s~s~p", [?BASE_EVENTS_URL, "/", Name])),
    [ok = http_helpers:http_put(Url, mochijson2:encode([{event, Value}, {tags, ["error"]}])) || Value <- Values].

get_sample_values(Count) ->
    get_sample_values(Count, []).

get_sample_values(Count, Acc) when Count > length(Acc) ->
    Rand = random:uniform(?RAND),
    get_sample_values(Count, lists:append(Acc, [integer_to_list(Rand)]));
get_sample_values(_, Acc) ->
    Acc.
