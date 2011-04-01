%%%-------------------------------------------------------------------
%%% File:      folsom_metrics_tests.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% tests for _metrics
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BASE_METRICS_URL, "http://localhost:5555/_metrics").
-define(RAND, 1500).
-define(DEFAULT_SIZE, 5).

-export([run/0]).

run() ->
    metrics_populate(),
    base_metrics_checks(),
    base_metrics_info_checks(),
    individual_metrics_checks(),
    raw_individual_metrics_checks(),
    covariance_checks(),
    tags_metrics_checks(),
    agg_tags_metrics_checks(),
    delete_metrics_checks().

metrics_populate() ->
    % create a exdec metric
    create_metric(a, exdec, ?DEFAULT_SIZE, 1, ["exdec", "taco"]),

    % populate metric 'a' with values
    AList = populate_metric(a, 10),
    [http_helpers:check_put_response_code(Response) || Response <- AList],


    % create a uniform metric
    create_metric(b, uniform, ?DEFAULT_SIZE, ["uniform", "taco"]),

    % populate metric 'b' with values
    BList = populate_metric(b, 10),
    [http_helpers:check_put_response_code(Response) || Response <- BList],

    % create a none metric
    create_metric(c, none, ?DEFAULT_SIZE, ["none", "taco"]),

    % populate metric 'c' with values
    CList = populate_metric(c, 10),
    [http_helpers:check_put_response_code(Response) || Response <- CList].

base_metrics_checks() ->
    % check _metrics list
    {"200", _, Body1} = http_helpers:http_get(?BASE_METRICS_URL),
    List1 = mochijson2:decode(Body1),

    % make sure list length is 3 since we created a, b and c
    3 = length(List1).

base_metrics_info_checks() ->
    % check _metrics?info=true list
    Url1 = lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "?info=true"])),
    {"200", _, Body2} = http_helpers:http_get(Url1),
    [{struct, C}, {struct, B}, {struct, A}] = mochijson2:decode(Body2),

    % make sure the keys we setup are in there
    true = lists:keymember(<<"a">>, 1, A),
    true = lists:keymember(<<"b">>, 1, B),
    true = lists:keymember(<<"c">>, 1, C).

individual_metrics_checks() ->
    % build metric urls, _metrics/a, etc
    Url1 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", a])),
    {"200", _, Body1} = http_helpers:http_get(Url1),
    {struct, List1} = mochijson2:decode(Body1),

    Url2 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", b])),
    {"200", _, Body2} = http_helpers:http_get(Url2),
    {struct, List2} = mochijson2:decode(Body2),

    Url3 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", c])),
    {"200", _, Body3} = http_helpers:http_get(Url3),
    {struct, List3} = mochijson2:decode(Body3),

    % check each metric for stats
    stats_keys_check(List1),
    stats_keys_check(List2),
    stats_keys_check(List3).

raw_individual_metrics_checks() ->
    % build metric urls, _metrics/a?raw=true, etc
    Url1 = lists:append(io_lib:format("~s~s~p~s", [?BASE_METRICS_URL, "/", a, "?raw=true"])),
    {"200", _, Body1} = http_helpers:http_get(Url1),
    List1 = mochijson2:decode(Body1),

    Url2 = lists:append(io_lib:format("~s~s~p~s", [?BASE_METRICS_URL, "/", b, "?raw=true"])),
    {"200", _, Body2} = http_helpers:http_get(Url2),
    List2 = mochijson2:decode(Body2),

    Url3 = lists:append(io_lib:format("~s~s~p~s", [?BASE_METRICS_URL, "/", c, "?raw=true"])),
    {"200", _, Body3} = http_helpers:http_get(Url3),
    List3 = mochijson2:decode(Body3),

    % check values
    ?DEFAULT_SIZE = length(List1),
    ?DEFAULT_SIZE = length(List2),
    ?DEFAULT_SIZE = length(List3).

covariance_checks() ->
    % build metric urls, _metrics/a?covariance=b
    Url1 = lists:append(io_lib:format("~s~s~p~s~p", [?BASE_METRICS_URL, "/", a, "?covariance=", b])),
    {"200", _, Body1} = http_helpers:http_get(Url1),
    Value = mochijson2:decode(Body1),
    true = is_float(Value).

tags_metrics_checks() ->
    % check _metrics?tag=taco for 3 items
    Url1 = lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "?tag=taco"])),
    {"200", _, Body1} = http_helpers:http_get(Url1),
    List1 = mochijson2:decode(Body1),

    % make sure there are 3 items with taco as a tag
    3 = length(List1),

    % build some urls to make tag requests on
    Url2 = lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "?tag=exdec"])),
    Url3 = lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "?tag=uniform"])),
    Url4 = lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "?tag=none"])),

    {"200", _, Body2} = http_helpers:http_get(Url2),
    List2 = mochijson2:decode(Body2),
    1 = length(List2),

    {"200", _, Body3} = http_helpers:http_get(Url3),
    List3 = mochijson2:decode(Body3),
    1 = length(List3),

    {"200", _, Body4} = http_helpers:http_get(Url4),
    List4 = mochijson2:decode(Body4),
    1 = length(List4).

agg_tags_metrics_checks() ->
    % check the agg stats
    Url1 = lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "?tag=taco&aggregate=true"])),
    {"200", _, Body1} = http_helpers:http_get(Url1),
    {struct, List1} = mochijson2:decode(Body1),

    % make sure the keys exist
    stats_keys_check(List1),

    % make sure there are some values in there
    true = proplists:get_value(<<"max">>, List1) > 0,

    % check agg raw values
    Url2 = lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "?tag=taco&aggregate=true&raw=true"])),
    {"200", _, Body2} = http_helpers:http_get(Url2),
    List2 = mochijson2:decode(Body2),
    ?DEFAULT_SIZE * 3 = length(List2).

delete_metrics_checks() ->
    Url1 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", a])),
    {"204", _, _} = http_helpers:http_delete(Url1),

    Url2 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", b])),
    {"204", _, _} = http_helpers:http_delete(Url2),

    Url3 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", c])),
    {"204", _, _} = http_helpers:http_delete(Url3),

    % check _metrics list
    {"200", _, Body4} = http_helpers:http_get(?BASE_METRICS_URL),
    List = mochijson2:decode(Body4),

    % make sure list length is 3 since we created a, b and c
    0 = length(List).


% internal functions

create_metric(Name, Type, Size, Tags) ->
    Proplist = [
                {id, Name},
                {size, Size},
                {type, Type},
                {tags, Tags}
               ],
    Body = mochijson2:encode(Proplist),
    Response = http_helpers:http_put(?BASE_METRICS_URL, Body),
    http_helpers:check_put_response_code(Response).

create_metric(Name, Type, Size, Alpha, Tags) ->
    Proplist = [
                {id, Name},
                {size, Size},
                {type, Type},
                {tags, Tags},
                {alpha, Alpha}
               ],
    Body = mochijson2:encode(Proplist),
    Response = http_helpers:http_put(?BASE_METRICS_URL, Body),
    http_helpers:check_put_response_code(Response).

populate_metric(Name, Count) ->
    Values = get_sample_values(Count),
    Url = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", Name])),
    [http_helpers:http_put(Url, mochijson2:encode([{value, Value}])) || Value <- Values].

get_sample_values(Count) ->
    get_sample_values(Count, []).

get_sample_values(Count, Acc) when Count > length(Acc) ->
    Rand = random:uniform(?RAND),
    get_sample_values(Count, lists:append(Acc, [Rand]));
get_sample_values(_, Acc) ->
    Acc.

stats_keys_check(List) ->
    true = lists:keymember(<<"min">>, 1, List),
    true = lists:keymember(<<"max">>, 1, List),
    true = lists:keymember(<<"mean">>, 1, List),
    true = lists:keymember(<<"median">>, 1, List),
    true = lists:keymember(<<"variance">>, 1, List),
    true = lists:keymember(<<"standard_deviation">>, 1, List),
    true = lists:keymember(<<"skewness">>, 1, List),
    true = lists:keymember(<<"kurtosis">>, 1, List),
    true = lists:keymember(<<"percentile">>, 1, List),
    true = lists:keymember(<<"histogram">>, 1, List).
