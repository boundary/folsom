-module(folsom_tests).

-include_lib("eunit/include/eunit.hrl").

api_test() ->
    setup(),
    folsom_metrics_tests:run(),
    folsom_events_tests:run(),
    folsom_vm_tests:run(),
    teardown().

setup() ->
    ibrowse:start(),
    folsom:start().

teardown() ->
    ibrowse:stop(),
    folsom:stop().
