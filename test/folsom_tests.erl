-module(folsom_tests).

-include_lib("eunit/include/eunit.hrl").

run_test() ->
    folsom:start(),

    create_metrics(),
    populate_metrics(),
    check_metrics(),
    delete_metrics(),

    folsom:stop().

create_metrics() ->
    ok = folsom_metrics:new_counter(counter),
    ok = folsom_metrics:new_gauge(gauge),
    ok = folsom_metrics:new_histogram(histogram),
    ok = folsom_metrics:new_history(historytest),
    ok = folsom_metrics:new_meter(meter),

    5 = length(folsom_metrics:get_metrics()).

populate_metrics() ->
    folsom_metrics:notify({counter, {inc, 1}}),
    folsom_metrics:notify({gauge, 2}),
    folsom_metrics:notify({histogram, 3}),
    folsom_metrics:notify({historytest, "4"}),
    folsom_metrics:notify({meter, 5}).

check_metrics() ->
    1 = folsom_metrics:get_metric_value(counter),

    2 = folsom_metrics:get_metric_value(gauge),

    Histogram = folsom_metrics:get_metric_value(histogram),
    3 = proplists:get_value(min, Histogram),

    1 = length(folsom_metrics:get_metric_value(historytest)),

    Meter = folsom_metrics:get_metric_value(meter),
    0 > proplists:get_value(one, Meter).

delete_metrics() ->
    folsom_metrics:delete_metric(counter),
    folsom_metrics:delete_metric(gauge),
    folsom_metrics:delete_metric(histogram),
    folsom_metrics:delete_metric(historytest),
    folsom_metrics:delete_metric(meter).
