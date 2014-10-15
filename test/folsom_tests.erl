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
%%% File:      folsom_tests.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_tests).

-include_lib("eunit/include/eunit.hrl").

run_test_() ->
    {setup,
     fun() -> {ok, Apps} = application:ensure_all_started(folsom),
              Apps
     end,
     fun(Apps) -> [application:stop(App) || App <- Apps] end,
     [{"creating metrics",
       fun folsom_erlang_checks:create_metrics/0},
      {"tagging metrics",
       fun folsom_erlang_checks:tag_metrics/0},
      {"populating metrics",
       {timeout, 30, fun folsom_erlang_checks:populate_metrics/0}},
      {"checking metrics",
       fun folsom_erlang_checks:check_metrics/0},
      {"checking counter metric",
       fun () ->
               folsom_erlang_checks:counter_metric(10000, testcounter)
       end},
      {"checking group metrics",
       fun folsom_erlang_checks:check_group_metrics/0},
      {"checking erlang vm metrics",
       fun folsom_erlang_checks:vm_metrics/0},
      {"deleting metrics",
       fun folsom_erlang_checks:delete_metrics/0},
      {"cpu topology test",
       fun folsom_erlang_checks:cpu_topology/0},
      {"c compiler test",
       fun folsom_erlang_checks:c_compiler_used/0},
      {"create and delete tests",
       fun folsom_erlang_checks:create_delete_metrics/0}]}.

configure_test_() ->
    {foreach, fun setup_app/0, fun cleanup_app/1,
     [{"start with configured metrics",
       fun() ->
               ?assertMatch(ok, application:start(folsom)),
               [counter, slide, <<"gauge">>, <<"uniform">>] =
                   lists:sort(folsom_metrics:get_metrics())
       end}]}.

setup_app() ->
    application:unload(folsom),
    Env = [{counter, counter},
           {gauge, <<"gauge">>},
           {histogram, [[<<"uniform">>, uniform, 5000],
                        [slide, slide_uniform, {60, 1028}]]}],
    application:load({application, folsom, [{mod, {folsom, []}}, {env, Env}]}),
    ok.

cleanup_app(ok) ->
    lists:foreach(fun folsom_metrics:delete_metric/1,
                  [counter, slide, <<"gauge">>, <<"uniform">>]),
    application:stop(folsom),
    application:unload(folsom),
    ok.

update_counter_test() ->
    Tid = ets:new(sometable, [public, set]),
    Workers = [spawn_monitor(fun() -> timer:sleep(100-N), folsom_utils:update_counter(Tid, hello, N) end) || N <- lists:seq(1, 100)],
    wait_for_results(Workers),
    ?assertEqual([{hello, 5050}], ets:lookup(Tid, hello)).

wait_for_results([]) ->
    ok;
wait_for_results(Workers) ->
    receive
        {'DOWN', _, _, Pid, Reason} ->
            case lists:keyfind(Pid, 1, Workers) of
                false ->
                    wait_for_results(Workers);
                _ ->
                    case Reason of
                        normal ->
                            wait_for_results(lists:keydelete(Pid, 1, Workers));
                        _ ->
                            erlang:error(Reason)
                    end
            end
    end.
