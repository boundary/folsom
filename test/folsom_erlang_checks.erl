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
         tag_metrics/0,
         check_metrics/0,
         check_group_metrics/0,
         delete_metrics/0,
         vm_metrics/0,
         counter_metric/2,
         cpu_topology/0,
         c_compiler_used/0,
	 create_delete_metrics/0
        ]).

-define(DATA, [0, 1, 5, 10, 100, 200, 500, 750, 1000, 2000, 5000]).
-define(HUGEDATA, lists:seq(1,10000)).

-define(DATA1, [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]).
-define(DATA2, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]).

-include("folsom.hrl").

create_metrics() ->
    ok = folsom_metrics:new_counter(counter),
    ok = folsom_metrics:new_counter(counter2),
    ok = folsom_metrics:new_gauge(<<"gauge">>),

    ok = folsom_metrics:new_histogram(<<"uniform">>, uniform, 5000),
    ok = folsom_metrics:new_histogram(<<"hugedata">>, uniform, 5000),
    ok = folsom_metrics:new_histogram(exdec, exdec),
    ok = folsom_metrics:new_histogram(none, none, 5000),

    ok = folsom_metrics:new_histogram(nonea, none, 5000),

    ok = folsom_metrics:new_histogram(noneb, none, 10),
    ok = folsom_metrics:new_histogram(nonec, none, 5),

    ok = folsom_metrics:new_histogram(slide_sorted_a, slide_sorted, 10),

    ok = folsom_metrics:new_histogram(timed, none, 5000),
    ok = folsom_metrics:new_histogram(timed2, none, 5000),

    ok = folsom_metrics:new_history(<<"history">>),
    ok = folsom_metrics:new_meter(meter),

    ok = folsom_metrics:new_meter_reader(meter_reader),

    ok = folsom_metrics:new_duration(duration),

    ok = folsom_metrics:new_spiral(spiral),
    ok = folsom_metrics:new_spiral(spiral_no_exceptions, no_exceptions),

    ?debugFmt("ensuring meter tick is registered with gen_server~n", []),
    ok = ensure_meter_tick_exists(2),

    ?debugFmt("ensuring multiple timer registrations dont cause issues", []),
    ok = folsom_meter_timer_server:register(meter, folsom_metrics_meter),
    ok = folsom_meter_timer_server:register(meter, folsom_metrics_meter),
    ok = folsom_meter_timer_server:register(meter, folsom_metrics_meter),

    ?debugFmt("~p", [folsom_meter_timer_server:dump()]),
    {state, List} = folsom_meter_timer_server:dump(),
    2 = length(List),

    %% check two servers got started for the spiral metrics
    2 = length(supervisor:which_children(folsom_sample_slide_sup)),

    19 = length(folsom_metrics:get_metrics()),

    ?debugFmt("~n~nmetrics: ~p~n", [folsom_metrics:get_metrics()]).

tag_metrics() ->
    Group = "mygroup",
    ok = folsom_metrics:tag_metric(counter, Group),
    ok = folsom_metrics:tag_metric(counter2, Group),
    ok = folsom_metrics:tag_metric(<<"gauge">>, Group),
    ok = folsom_metrics:tag_metric(meter, Group),
    ok = folsom_metrics:tag_metric(spiral, Group),
    ok = folsom_metrics:tag_metric(spiral_no_exceptions, Group),
    ?debugFmt("~n~ntagged metrics: ~p, ~p, ~p, ~p, ~p and ~p in group ~p~n", [counter,counter2,<<"gauge">>,meter,spiral,spiral_no_exceptions,Group]).

populate_metrics() ->
    ok = folsom_metrics:notify({counter, {inc, 1}}),
    ok = folsom_metrics:notify({counter, {dec, 1}}),

    ok = folsom_metrics:notify({counter2, {inc, 10}}),
    ok = folsom_metrics:notify({counter2, {dec, 7}}),

    meck:new(folsom_ets),
    meck:expect(folsom_ets, notify, fun(_Event) -> meck:exception(error, something_wrong_with_ets) end),
    {'EXIT', {something_wrong_with_ets, _}} = folsom_metrics:safely_notify({unknown_counter, {inc, 1}}),
    meck:unload(folsom_ets),
    ok = folsom_metrics:safely_histogram_timed_update(unknown_histogram, fun() -> ok end),
    ok = folsom_metrics:safely_histogram_timed_update(unknown_histogram, fun(ok) -> ok end, [ok]),
    3.141592653589793 = folsom_metrics:safely_histogram_timed_update(unknown_histogram, math, pi, []),
    UnknownHistogramBegin = folsom_metrics:histogram_timed_begin(unknown_histogram),
    {error, unknown_histogram, nonexistent_metric} = folsom_metrics:safely_histogram_timed_notify(UnknownHistogramBegin),

    ok = folsom_metrics:notify({<<"gauge">>, 2}),

    [ok = folsom_metrics:notify({<<"uniform">>, Value}) || Value <- ?DATA],

    [ok = folsom_metrics:notify({<<"hugedata">>, Value}) || Value <- ?HUGEDATA],

    [ok = folsom_metrics:notify({exdec, Value}) || Value <- lists:seq(1, 100000)],

    [ok = folsom_metrics:notify({none, Value}) || Value <- ?DATA],

    [ok = folsom_metrics:notify({nonea, Value}) || Value <- ?DATA1],

    [ok = folsom_metrics:notify({noneb, Value}) || Value <- ?DATA2],

    [ok = folsom_metrics:notify({nonec, Value}) || Value <- ?DATA2],

    [ok = folsom_metrics:notify({slide_sorted_a, Value}) || Value <- ?DATA2],

    ok = folsom_metrics:notify(tagged_metric, 1, meter, [a, b]),
    ok = folsom_metrics:notify(tagged_metric, 1, meter, [c]),

    {error, _, unsupported_metric_type} = folsom_metrics:notify(tagged_unknown_metric, 1, unknown_metric, [tag]),

    3.141592653589793 = folsom_metrics:histogram_timed_update(timed, math, pi, []),

    Begin = folsom_metrics:histogram_timed_begin(timed2),
    folsom_metrics:histogram_timed_notify(Begin),

    PopulateDuration = fun() ->
                               ok = folsom_metrics:notify_existing_metric(duration, timer_start, duration),
                               timer:sleep(10),
                               ok = folsom_metrics:notify_existing_metric(duration, timer_end, duration) end,

    [PopulateDuration() || _ <- lists:seq(1, 10)],

    ok = folsom_metrics:notify({<<"history">>, "string"}),

    {error, _, nonexistent_metric} = folsom_metrics:notify({historya, "5"}),
    ok = folsom_metrics:notify(historya, <<"binary">>, history),

    ?debugFmt("testing meter ...", []),

    % simulate an interval tick
    folsom_metrics_meter:tick(meter),

    [ok,ok,ok,ok,ok] =
        [ folsom_metrics:notify({meter, Item}) || Item <- [100, 100, 100, 100, 100]],

    % simulate an interval tick
    folsom_metrics_meter:tick(meter),

    ?debugFmt("testing meter reader ...", []),

    % simulate an interval tick
    folsom_metrics_meter_reader:tick(meter_reader),

    [ok,ok,ok,ok,ok] =
        [ folsom_metrics:notify({meter_reader, Item}) || Item <- [1, 10, 100, 1000, 10000]],

    % simulate an interval tick
    folsom_metrics_meter_reader:tick(meter_reader),

    folsom_metrics:notify_existing_metric(spiral, 100, spiral),
    folsom_metrics:notify_existing_metric(spiral_no_exceptions, 200, spiral).

check_metrics() ->
    0 = folsom_metrics:get_metric_value(counter),

    3 = folsom_metrics:get_metric_value(counter2),

    ok = folsom_metrics:notify_existing_metric(counter2, clear, counter),

    0 = folsom_metrics:get_metric_value(counter2),

    2 = folsom_metrics:get_metric_value(<<"gauge">>),

    true = sets:is_subset(sets:from_list([a,b,c]), folsom_metrics:get_tags(tagged_metric)),

    [11,12,13,14,15,6,7,8,9,10] = folsom_metrics:get_metric_value(noneb),

    [11,12,13,14,15] = folsom_metrics:get_metric_value(nonec),

    [6,7,8,9,10,11,12,13,14,15] = folsom_metrics:get_metric_value(slide_sorted_a),

    Histogram1 = folsom_metrics:get_histogram_statistics(<<"uniform">>),
    histogram_checks(Histogram1),

    MetricsSubset = [min, max],

    ok = set_enabled_metrics(MetricsSubset),
    Histogram2 = folsom_metrics:get_histogram_statistics(<<"uniform">>),
    subset_checks(Histogram2, MetricsSubset),
    ok = set_enabled_metrics(?DEFAULT_METRICS),

    HugeHistogram = folsom_metrics:get_histogram_statistics(<<"hugedata">>),
    huge_histogram_checks(HugeHistogram),

    % just check exdec for non-zero values
    Exdec = folsom_metrics:get_histogram_statistics(exdec),

    ?debugFmt("checking exdec sample~n~p~n", [Exdec]),

    ok = case proplists:get_value(median, Exdec) of
        Median when Median > 0 ->
                 ok;
             _ ->
                 error
         end,

    Histogram3 = folsom_metrics:get_histogram_statistics(none),
    histogram_checks(Histogram3),

    CoValues = folsom_metrics:get_histogram_statistics(none, nonea),
    histogram_co_checks(CoValues),

    List = folsom_metrics:get_metric_value(timed),
    ?debugFmt("timed update value: ~p", [List]),

    List2 = folsom_metrics:get_metric_value(timed2),
    ?debugFmt("timed update value begin/end: ~p", [List2]),

    1 = length(folsom_metrics:get_metric_value(<<"history">>)),
    1 = length(folsom_metrics:get_metric_value(historya)),

    ?debugFmt("checking meter~n", []),
    Meter = folsom_metrics:get_metric_value(meter),
    ?debugFmt("~p", [Meter]),
    ok = case proplists:get_value(one, Meter) of
             Value when Value > 1 ->
                 ok;
             _ ->
                 error
         end,
    ok = case proplists:get_value(day, Meter) of
             Value1 when Value1 > 0.005 ->
                 ok;
             _ ->
                 error
         end,

    ?debugFmt("checking meter reader~n", []),
    MeterReader = folsom_metrics:get_metric_value(meter_reader),
    ?debugFmt("~p~n", [MeterReader]),
    ok = case proplists:get_value(one, MeterReader) of
             Value2 when Value2 > 1 ->
                 ok;
             _ ->
                 error
         end,

    %% check duration
    Dur = folsom_metrics:get_metric_value(duration),
    duration_check(Dur),

    ok = set_enabled_metrics(MetricsSubset),
    Dur2 = folsom_metrics:get_metric_value(duration),
    subset_checks(Dur2, MetricsSubset),
    ok = set_enabled_metrics(?DEFAULT_METRICS),

    %% check spiral
    [{count, 100}, {one, 100}] = folsom_metrics:get_metric_value(spiral),

    [{count, 200}, {one, 200}] = folsom_metrics:get_metric_value(spiral_no_exceptions).

check_group_metrics() ->
    Group = "mygroup",
    Metrics = folsom_metrics:get_metrics_value(Group),
    6 = length(Metrics),
    {counter, 0} = lists:keyfind(counter,1,Metrics),
    {counter2, 0} = lists:keyfind(counter2,1,Metrics),
    {<<"gauge">>, 2} = lists:keyfind(<<"gauge">>,1,Metrics),

    {meter, Meter} = lists:keyfind(meter,1,Metrics),
    ok = case proplists:get_value(one, Meter) of
             Value when Value > 1 ->
                 ok;
             _ ->
                 error
         end,
    ok = case proplists:get_value(day, Meter) of
             Value1 when Value1 > 0.005 ->
                 ok;
             _ ->
                 error
         end,

    {spiral, [{count, 100}, {one, 100}]} = lists:keyfind(spiral,1,Metrics),
    {spiral_no_exceptions, [{count, 200}, {one, 200}]} = lists:keyfind(spiral_no_exceptions,1,Metrics),

    Counters = folsom_metrics:get_metrics_value(Group,counter),
    {counter, 0} = lists:keyfind(counter,1,Counters),
    {counter2, 0} = lists:keyfind(counter2,1,Counters),

    ok = folsom_metrics:untag_metric(counter2, Group),
    ok = folsom_metrics:untag_metric(<<"gauge">>, Group),
    ok = folsom_metrics:untag_metric(meter, Group),
    ok = folsom_metrics:untag_metric(spiral, Group),
    ok = folsom_metrics:untag_metric(spiral_no_exceptions, Group),
    ?debugFmt("~n~nuntagged metrics: ~p, ~p, ~p, ~p and ~p in group ~p~n", [counter2,<<"gauge">>,meter,spiral,spiral_no_exceptions,Group]),
    RemainingMetrics = folsom_metrics:get_metrics_value(Group),
    1 = length(RemainingMetrics),
    {counter, 0} = lists:keyfind(counter,1,Metrics).

delete_metrics() ->
    22 = length(ets:tab2list(?FOLSOM_TABLE)),

    ok = folsom_metrics:delete_metric(counter),
    ok = folsom_metrics:delete_metric(counter2),
    ok = folsom_metrics:delete_metric(<<"gauge">>),

    ok = folsom_metrics:delete_metric(<<"hugedata">>),
    ok = folsom_metrics:delete_metric(<<"uniform">>),
    ok = folsom_metrics:delete_metric(exdec),
    ok = folsom_metrics:delete_metric(none),

    ok = folsom_metrics:delete_metric(<<"history">>),
    ok = folsom_metrics:delete_metric(historya),

    ok = folsom_metrics:delete_metric(nonea),
    ok = folsom_metrics:delete_metric(noneb),
    ok = folsom_metrics:delete_metric(nonec),

    ok = folsom_metrics:delete_metric(tagged_metric),

    ok = folsom_metrics:delete_metric(slide_sorted_a),

    ok = folsom_metrics:delete_metric(timed),
    ok = folsom_metrics:delete_metric(timed2),
    ok = folsom_metrics:delete_metric(testcounter),

    ok = ensure_meter_tick_exists(2),

    1 = length(ets:tab2list(?METER_TABLE)),
    ok = folsom_metrics:delete_metric(meter),
    0 = length(ets:tab2list(?METER_TABLE)),

    1 = length(ets:tab2list(?METER_READER_TABLE)),
    ok = folsom_metrics:delete_metric(meter_reader),
    0 = length(ets:tab2list(?METER_READER_TABLE)),

    ok = ensure_meter_tick_exists(0),

    ok = folsom_metrics:delete_metric(duration),
    ok = folsom_metrics:delete_metric(spiral),
    ok = folsom_metrics:delete_metric(spiral_no_exceptions),

    0 = length(ets:tab2list(?FOLSOM_TABLE)).

vm_metrics() ->
    List1 = folsom_vm_metrics:get_memory(),
    true = lists:keymember(total, 1, List1),

    List2 = folsom_vm_metrics:get_statistics(),
    true = lists:keymember(context_switches, 1, List2),

    List3 = folsom_vm_metrics:get_system_info(),
    true = lists:keymember(allocated_areas, 1, List3),
    true = lists:keymember(port_count, 1, List3),

    [{_, [{backtrace, _}| _]} | _] = folsom_vm_metrics:get_process_info(),

    [{_, [{name, _}| _]} | _] = folsom_vm_metrics:get_port_info().


counter_metric(Count, Counter) ->
    ok = folsom_metrics:new_counter(Counter),

    ?debugFmt("running ~p counter inc/dec rounds~n", [Count]),
    for(Count, Counter),

    Result = folsom_metrics:get_metric_value(Counter),
    ?debugFmt("counter result: ~p~n", [Result]),

    0 = Result.

ensure_meter_tick_exists(MeterCnt) ->
    {state, State} = folsom_meter_timer_server:dump(),
    MeterCnt = length(State),
    ok.

%% internal function

histogram_checks(List) ->
    ?debugFmt("checking histogram statistics", []),
    ?debugFmt("~p~n", [List]),
    0 = proplists:get_value(min, List),
    5000 = proplists:get_value(max, List),
    869.6363636363636 = proplists:get_value(arithmetic_mean, List),

    GeoMean = proplists:get_value(geometric_mean, List),
    ok = case GeoMean - 100.17443147308997 of
             GeoDiff when GeoDiff <  0.00000001 ->
                 ok;
             _ ->
                 error
         end,

    Value = proplists:get_value(harmonic_mean, List),
    %?debugFmt("~p~n", [Value]),
    ok = case Value - 8.333122900936845 of
             Diff when Diff < 0.00000001 ->
                 ok;
             _ ->
                 error
         end,

    200 = proplists:get_value(median, List),
    2254368.454545454 = proplists:get_value(variance, List),
    1501.4554454080394 = proplists:get_value(standard_deviation, List),
    1.8399452806806476 = proplists:get_value(skewness, List),
    2.2856772911293204 = proplists:get_value(kurtosis, List),
    List1 = proplists:get_value(percentile, List),
    percentile_check(List1),
    List2 = proplists:get_value(histogram, List),
    histogram_check(List2).

huge_histogram_checks(List) ->
    Skew = erlang:abs(proplists:get_value(skewness, List)),
    A = skewness_is_too_high_for_sample_of_this_size,
    B = this_event_is_very_unprobable,
    C = please_rerun_this_test__if_it_fails_again_your_code_is_bugged,
    {A, B, C, true} = {A, B, C, Skew < 0.2}.

histogram_co_checks(List) ->
    ?debugFmt("checking histogram covariance and etc statistics", []),
    ?debugFmt("~p~n", [List]),
    [
     {covariance,17209.545454545456},
     {tau,1.0},
     {rho,0.760297020598996},
     {r,1.0}
    ] = List.

percentile_check(List) ->
    750 = proplists:get_value(75, List),
    2000 = proplists:get_value(95, List),
    5000 = proplists:get_value(99, List),
    5000 = proplists:get_value(999, List).

histogram_check(List) ->
    [{2400,10},{5000,1},{8000,0}] = List.

counter_inc_dec(Counter) ->
    ok = folsom_metrics:notify({Counter, {inc, 1}}),
    ok = folsom_metrics:notify({Counter, {dec, 1}}).

for(N, Counter) ->
    for(N, 0, Counter).
for(N, Count, _Counter) when N == Count ->
    ok;
for(N, LoopCount, Counter) ->
    counter_inc_dec(Counter),
    for(N, LoopCount + 1, Counter).

cpu_topology() ->
    ?debugFmt("Testing various CPU topologies ...~n", []),
    {ok, [Data]} = file:consult("../test/cpu_topo_data"),
    [run_convert_and_jsonify(Item) || Item <- Data].


run_convert_and_jsonify(Item) ->
    ?debugFmt("Converting ... ~n~p~n", [Item]),
    Result = folsom_vm_metrics:convert_system_info(cpu_topology, Item),
    %?debugFmt("~p~n", [mochijson2:encode(Result)]).
    mochijson2:encode(Result).

c_compiler_used() ->
    Test = [{gnuc, {4,4,5}},
            {gnuc, {4,4}},
            {msc, 1600}],

    Expected = [[{compiler, gnuc}, {version, <<"4.4.5">>}],
                [{compiler, gnuc}, {version, <<"4.4">>}],
                [{compiler, msc}, {version, <<"1600">>}]],

    ?assertEqual(Expected, [folsom_vm_metrics:convert_system_info(c_compiler_used, {Compiler, Version})
                             || {Compiler, Version} <- Test]).


duration_check(Duration) ->
    [?assert(lists:keymember(Key, 1, Duration)) || Key <-
                                                       [count, last, min, max, arithmetic_mean,
                                                        geometric_mean, harmonic_mean, median,
                                                        variance, standard_deviation, skewness,
                                                        kurtosis, percentile, histogram]],
    ?assertEqual(10, proplists:get_value(count, Duration)),
    Last = proplists:get_value(last, Duration),
    ?assert(Last > 10000).

create_delete_metrics() ->
    ?assertMatch(ok, folsom_metrics:new_counter(counter)),
    ?assertMatch(ok, folsom_metrics:notify_existing_metric(counter, {inc, 1}, counter)),
    ?assertMatch(1, folsom_metrics:get_metric_value(counter)),
    ?assertMatch(ok, folsom_metrics:delete_metric(counter)),
    ?assertError(badarg, folsom_metrics:notify_existing_metric(counter, {inc, 1}, counter)),
    ?assertMatch(ok, folsom_metrics:new_counter(counter)),
    ?assertMatch(ok, folsom_metrics:notify_existing_metric(counter, {inc, 1}, counter)),
    ?assertMatch(1, folsom_metrics:get_metric_value(counter)).

set_enabled_metrics(Enabled) ->
    application:set_env(folsom, enabled_metrics, Enabled).

subset_checks(List, Enabled) ->
    ?debugFmt("checking subset statistics", []),
    ?debugFmt("~p, ~p~n", [List, Enabled]),
    Disabled = ?DEFAULT_METRICS -- Enabled,
    true = lists:all(fun(K) ->
                             proplists:get_value(K, List) == undefined
                     end,
                     Disabled),
    true = lists:all(fun(K) ->
                             proplists:get_value(K, List) /= undefined
                     end,
                     Enabled).
