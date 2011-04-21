-module(folsom_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DATA, [1, 5, 10, 100, 200, 500, 750, 1000, 2000, 5000]).

run_test() ->
    folsom:start(),

    create_metrics(),
    populate_metrics(),
    check_metrics(),
    delete_metrics(),

    vm_metrics(),

    folsom:stop().

create_metrics() ->
    ok = folsom_metrics:new_counter(counter),
    ok = folsom_metrics:new_gauge(gauge),
    ok = folsom_metrics:new_histogram(histogram),
    ok = folsom_metrics:new_history(history),
    ok = folsom_metrics:new_meter(meter),

    5 = length(folsom_metrics:get_metrics()).

populate_metrics() ->
    ok = folsom_metrics:notify({counter, {inc, 1}}),
    ok = folsom_metrics:notify({counter, {dec, 1}}),

    ok = folsom_metrics:notify({gauge, 2}),

    [folsom_metrics:notify({histogram, Value}) || Value <- ?DATA],

    ok = folsom_metrics:notify({history, "4"}),
    ok = folsom_metrics:notify({meter, 5}).

check_metrics() ->
    0 = folsom_metrics:get_metric_value(counter),

    2 = folsom_metrics:get_metric_value(gauge),

    Histogram = folsom_metrics:get_metric_value(histogram),
    histogram_checks(Histogram),

    1 = length(folsom_metrics:get_metric_value(history)),

    Meter = folsom_metrics:get_metric_value(meter),
    0 > proplists:get_value(one, Meter).

delete_metrics() ->
    ok = folsom_metrics:delete_metric(counter),
    ok = folsom_metrics:delete_metric(gauge),
    ok = folsom_metrics:delete_metric(histogram),
    ok = folsom_metrics:delete_metric(history),
    ok = folsom_metrics:delete_metric(meter).

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

vm_metrics() ->
    List1 = folsom_vm_metrics:get_memory(),
    true = lists:keymember(total, 1, List1),

    List2 = folsom_vm_metrics:get_statistics(),
    true = lists:keymember(context_switches, 1, List2),

    List3 = folsom_vm_metrics:get_system_info(),
    true = lists:keymember(allocated_areas, 1, List3).
