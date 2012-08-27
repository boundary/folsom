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
     fun () -> folsom:start() end,
     fun (_) -> folsom:stop() end,
     [{"creating metrics",
       fun folsom_erlang_checks:create_metrics/0},
      {"populating metrics",
       {timeout, 30, fun folsom_erlang_checks:populate_metrics/0}},
      {"checking metrics",
       fun folsom_erlang_checks:check_metrics/0},
      {"checking counter metric",
       fun () ->
               folsom_erlang_checks:counter_metric(10000, testcounter)
       end},
      {"checking erlang vm metrics",
       fun folsom_erlang_checks:vm_metrics/0},
      {"deleting metrics",
       fun folsom_erlang_checks:delete_metrics/0},
      {"cpu topology test",
       fun folsom_erlang_checks:cpu_topology/0}]}.

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
