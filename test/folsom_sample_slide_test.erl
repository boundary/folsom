%%%
%%% Copyright 2012 - Basho Technologies, Inc. All Rights Reserved.
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
%%% File:      folsom_sample_slide.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc       eunit test for folsom_sample_slide.erl
%%% @end
%%%------------------------------------------------------------------

-module(folsom_sample_slide_test).

-include_lib("eunit/include/eunit.hrl").
-include("folsom.hrl").

-define(HISTO, test_slide).
-define(WINDOW, 30).
-define(RUNTIME, 90).
-define(READINGS, 10).

slide_test_() ->
    {setup,
     fun () -> folsom:start(),
               meck:new(folsom_utils)
     end,
     fun (_) -> meck:unload(folsom_utils),
                folsom:stop() end,
     [{"Create sliding window",
       fun create/0},
      {"test sliding window",
       {timeout, 30, fun exercise/0}}
     ]}.

create() ->
    ok = folsom_metrics:new_histogram(?HISTO, slide, ?WINDOW),
    #histogram{sample=Slide} = folsom_metrics_histogram:get_value(?HISTO),
    ?assert(is_pid(Slide#slide.server)),
    ?assertEqual(?WINDOW, Slide#slide.window),
    ?assertEqual(0, ets:info(Slide#slide.reservoir, size)).

exercise() ->
    %% don't want a trim to happen
    %% unless we call trim
    %% so kill the trim server process
    #histogram{sample=Slide} = folsom_metrics_histogram:get_value(?HISTO),
    ok = folsom_timer_server_sup:stop_timer(Slide#slide.server),
    Moments = lists:seq(1, ?RUNTIME),
    %% pump in 90 seconds worth of readings
    Moment = lists:foldl(fun(_X, Tick) ->
                                 Tock = tick(Tick),
                                 [folsom_sample_slide:update(Slide, N) ||
                                     N <- lists:duplicate(?READINGS, Tock)],
                                 Tock end,
                         0,
                         Moments),
    %% are all readings in the table?
    check_table(Slide, Moments),
    %% get values only returns last ?WINDOW seconds
    ExpectedValues = lists:sort(lists:flatten([lists:duplicate(?READINGS, N) ||
                                                  N <- lists:seq(?RUNTIME - ?WINDOW, ?RUNTIME)])),
    Values = lists:sort(folsom_sample_slide:get_values(Slide)),
    ?assertEqual(ExpectedValues, Values),
    %% trim the table
    Trimmed = folsom_sample_slide:trim(Slide#slide.reservoir, ?WINDOW),
    ?assertEqual((?RUNTIME - ?WINDOW - 1) * ?READINGS, Trimmed),
    check_table(Slide, lists:seq(?RUNTIME - ?WINDOW, ?RUNTIME)),
    %% increment the clock past the window
    tick(Moment, ?WINDOW * 2),
    %% get values should be empty
    ?assertEqual([], folsom_sample_slide:get_values(Slide)),
    %% trim, and table should be empty
    Trimmed2 = folsom_sample_slide:trim(Slide#slide.reservoir, ?WINDOW),
    ?assertEqual((?RUNTIME * ?READINGS) - ((?RUNTIME - ?WINDOW - 1) * ?READINGS), Trimmed2),
    check_table(Slide, []),
    ok.

tick(Moment0, IncrBy) ->
    Moment = Moment0 + IncrBy,
    meck:expect(folsom_utils, now_epoch, fun() ->
                                                 Moment end),
    Moment.

tick(Moment) ->
    tick(Moment, 1).

check_table(Slide, Moments) ->
    Tab = lists:sort(ets:tab2list(Slide#slide.reservoir)),
    {Ks, Vs} = lists:unzip(Tab),
    ExpectedVs = lists:sort(lists:flatten([lists:duplicate(10, N) || N <- Moments])),
    StrippedKeys = lists:usort([X || {X, _} <- Ks]),
    ?assertEqual(Moments, StrippedKeys),
    ?assertEqual(ExpectedVs, lists:sort(Vs)).
