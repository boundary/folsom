-module(folsom_tests).

-include_lib("eunit/include/eunit.hrl").

run_test() ->
    folsom:start(),

    folsom_erlang_checks:create_metrics(),
    folsom_erlang_checks:populate_metrics(),
    folsom_erlang_checks:check_metrics(),
    folsom_erlang_checks:vm_metrics(),

    ibrowse:start(),
    folsom_http_checks:run(),
    ibrowse:stop(),

    folsom_erlang_checks:delete_metrics(),

    folsom:stop().


