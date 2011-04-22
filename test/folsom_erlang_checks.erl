%%%
%%% Copyright 2011, Boundary
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
%%% File:      folsom_erlang_checks.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_erlang_checks).

-include_lib("eunit/include/eunit.hrl").

-export([
         create_metrics/0,
         populate_metrics/0,
         check_metrics/0,
         delete_metrics/0,
         vm_metrics/0
        ]).

-define(DATA, [1, 5, 10, 100, 200, 500, 750, 1000, 2000, 5000]).

create_metrics() ->
    ok = folsom_metrics:new_counter(counter),
    ok = folsom_metrics:new_gauge(gauge),

    ok = folsom_metrics:new_histogram(uniform, uniform, 5000, 1),
    ok = folsom_metrics:new_histogram(exdec, exdec, 5000, 1),
    ok = folsom_metrics:new_histogram(none, none, 5000, 1),

    ok = folsom_metrics:new_history(history),
    ok = folsom_metrics:new_meter(meter),

    7 = length(folsom_metrics:get_metrics()).

populate_metrics() ->
    ok = folsom_metrics:notify({counter, {inc, 1}}),
    ok = folsom_metrics:notify({counter, {dec, 1}}),

    ok = folsom_metrics:notify({gauge, 2}),

    [ok = folsom_metrics:notify({uniform, Value}) || Value <- ?DATA],
    [ok = folsom_metrics:notify({exdec, Value}) || Value <- ?DATA],
    [ok = folsom_metrics:notify({none, Value}) || Value <- ?DATA],

    ok = folsom_metrics:notify({history, "4"}),
    ok = folsom_metrics:notify({meter, 5}).

check_metrics() ->
    0 = folsom_metrics:get_metric_value(counter),

    2 = folsom_metrics:get_metric_value(gauge),

    Histogram1 = folsom_metrics:get_metric_value(uniform),
    histogram_checks(Histogram1),
    Histogram2 = folsom_metrics:get_metric_value(exdec),
    histogram_checks(Histogram2),
    Histogram3 = folsom_metrics:get_metric_value(none),
    histogram_checks(Histogram3),

    1 = length(folsom_metrics:get_metric_value(history)),

    Meter = folsom_metrics:get_metric_value(meter),
    0 > proplists:get_value(one, Meter).

delete_metrics() ->
    ok = folsom_metrics:delete_metric(counter),
    ok = folsom_metrics:delete_metric(gauge),

    ok = folsom_metrics:delete_metric(uniform),
    ok = folsom_metrics:delete_metric(exdec),
    ok = folsom_metrics:delete_metric(none),

    ok = folsom_metrics:delete_metric(history),
    ok = folsom_metrics:delete_metric(meter).

vm_metrics() ->
    List1 = folsom_vm_metrics:get_memory(),
    true = lists:keymember(total, 1, List1),

    List2 = folsom_vm_metrics:get_statistics(),
    true = lists:keymember(context_switches, 1, List2),

    List3 = folsom_vm_metrics:get_system_info(),
    true = lists:keymember(allocated_areas, 1, List3).

%% internal function

histogram_checks(List) ->
    1 = proplists:get_value(min, List),
    5000 = proplists:get_value(max, List),
    956.6 = proplists:get_value(mean, List),
    200 = proplists:get_value(median, List),
    2412421.1555555556 = proplists:get_value(variance, List),
    1553.1970755688267 = proplists:get_value(standard_deviation, List),
    2.3535226547841104 = proplists:get_value(skewness, List),
    2.190080666007587 = proplists:get_value(kurtosis, List),
    List1 = proplists:get_value(percentile, List),
    percentile_check(List1),
    List2 = proplists:get_value(histogram, List),
    histogram_check(List2).

percentile_check(List) ->
    1000 = proplists:get_value(75, List),
    5000 = proplists:get_value(95, List),
    5000 = proplists:get_value(99, List),
    5000 = proplists:get_value(999, List).

histogram_check(List) ->
    1 = proplists:get_value(1, List),
    1 = proplists:get_value(5, List),
    1 = proplists:get_value(10, List),
    0 = proplists:get_value(20, List),
    0 = proplists:get_value(30, List),
    0 = proplists:get_value(40, List),
    0 = proplists:get_value(50, List),
    1 = proplists:get_value(100, List),
    0 = proplists:get_value(150, List),
    1 = proplists:get_value(200, List),
    0 = proplists:get_value(250, List),
    0 = proplists:get_value(300, List),
    0 = proplists:get_value(350, List),
    0 = proplists:get_value(400, List),
    1 = proplists:get_value(500, List),
    1 = proplists:get_value(750, List),
    1 = proplists:get_value(1000, List),
    0 = proplists:get_value(1500, List),
    1 = proplists:get_value(2000, List),
    0 = proplists:get_value(3000, List),
    0 = proplists:get_value(4000, List),
    1 = proplists:get_value(5000, List),
    0 = proplists:get_value(10000, List),
    0 = proplists:get_value(20000, List),
    0 = proplists:get_value(30000, List),
    0 = proplists:get_value(50000, List),
    0 = proplists:get_value(99999999999999, List).
