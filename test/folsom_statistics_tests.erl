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
%%% File:      folsom_statistics_tests.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% tests for stats functions
%%% @end
%%%------------------------------------------------------------------

-module(folsom_statistics_tests).

-define(BASE_METRICS_URL, "http://localhost:5565/_metrics").
-define(DATA, [1, 5, 10, 100, 200, 500, 750, 1000, 2000, 5000]).
-define(DEFAULT_SIZE, 5).
-define(NAME, stats).

-export([run/0]).

run() ->
    metrics_populate(),
    stats_checks().

metrics_populate() ->
    Proplist = [
                {id, ?NAME},
                {size, length(?DATA)},
                {type, exdec},
                {tags, ["stats"]},
                {alpha, 1}
               ],
    Body = mochijson2:encode(Proplist),
    ok = http_helpers:http_put(?BASE_METRICS_URL, Body),
    Url = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", ?NAME])),
    [ok = http_helpers:http_put(Url, mochijson2:encode([{value, Value}])) || Value <- ?DATA].

stats_checks() ->
    % build metric urls, _metrics/a, etc
    Url = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", ?NAME])),
    Body = http_helpers:http_get(Url),
    {struct, List} = mochijson2:decode(Body),
    stats_values_checks(List).

stats_values_checks(List) ->
    1 = proplists:get_value(<<"min">>, List),
    5000 = proplists:get_value(<<"max">>, List),
    956.6 = proplists:get_value(<<"mean">>, List),
    200 = proplists:get_value(<<"median">>, List),
    2412421.1555555556 = proplists:get_value(<<"variance">>, List),
    1553.1970755688267 = proplists:get_value(<<"standard_deviation">>, List),
    2.3535226547841104 = proplists:get_value(<<"skewness">>, List),
    2.190080666007587 = proplists:get_value(<<"kurtosis">>, List),
    {struct, List1} = proplists:get_value(<<"percentile">>, List),
    percentile_check(List1),
    {struct, List2} = proplists:get_value(<<"histogram">>, List),
    histogram_check(List2).

percentile_check(List) ->
    1000 = proplists:get_value(<<"75">>, List),
    5000 = proplists:get_value(<<"95">>, List),
    5000 = proplists:get_value(<<"99">>, List),
    5000 = proplists:get_value(<<"999">>, List).

histogram_check(List) ->
    1 = proplists:get_value(<<"1">>, List),
    1 = proplists:get_value(<<"5">>, List),
    1 = proplists:get_value(<<"10">>, List),
    0 = proplists:get_value(<<"20">>, List),
    0 = proplists:get_value(<<"30">>, List),
    0 = proplists:get_value(<<"40">>, List),
    0 = proplists:get_value(<<"50">>, List),
    1 = proplists:get_value(<<"100">>, List),
    0 = proplists:get_value(<<"150">>, List),
    1 = proplists:get_value(<<"200">>, List),
    0 = proplists:get_value(<<"250">>, List),
    0 = proplists:get_value(<<"300">>, List),
    0 = proplists:get_value(<<"350">>, List),
    0 = proplists:get_value(<<"400">>, List),
    1 = proplists:get_value(<<"500">>, List),
    1 = proplists:get_value(<<"750">>, List),
    1 = proplists:get_value(<<"1000">>, List),
    0 = proplists:get_value(<<"1500">>, List),
    1 = proplists:get_value(<<"2000">>, List),
    0 = proplists:get_value(<<"3000">>, List),
    0 = proplists:get_value(<<"4000">>, List),
    1 = proplists:get_value(<<"5000">>, List),
    0 = proplists:get_value(<<"10000">>, List),
    0 = proplists:get_value(<<"20000">>, List),
    0 = proplists:get_value(<<"30000">>, List),
    0 = proplists:get_value(<<"50000">>, List),
    0 = proplists:get_value(<<"99999999999999">>, List).
