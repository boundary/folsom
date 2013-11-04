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
%%% File:      folsom_metrics_simple_statistics.erl
%%% @author    Andrey Vasenin <vasenin@aboutecho.com>
%%% @doc Simple minute statistics in one ets table. It works much faster than
%%%      slide uniform histogram and doesn't use so many ets tables
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_simple_statistics).

-export([update/2,
         expire_all/0,
         get_values/1]).

-include("folsom.hrl").

-record(interval, {
          count = 0,
          min = undefined,
          max = undefined,
          sum = 0}).

update(Name, Value) ->
    Current = current_slide(),
    I = #interval{count = 1, min = Value, max = Value, sum = Value},
    ets:insert(?SIMPLE_STATISTICS_TABLE, {{Name, Current}, merge(get_interval(Name, Current), I)}).

get_values(Name) ->
    Current = current_slide(),
    LastMinuteIntervals = [get_interval(Name, I) || I <- lists:seq(Current - ?DEFAULT_STATS_SLIDE_SIZE, Current)],
    #interval{count = Count,
              min = Min,
              max = Max,
              sum = Sum} = lists:foldl(fun merge/2, #interval{}, LastMinuteIntervals),
    L = [{count, Count},
         {min, Min},
         {max, Max},
         {mean, get_mean(Sum, Count)}],
    [ {K,V} || {K,V} <- L, V /= undefined ].

expire_all() ->
    ExpiredSlide = current_slide() - ?DEFAULT_STATS_SLIDE_SIZE,
    ets:select_delete(?SIMPLE_STATISTICS_TABLE, [{{{'_', '$1'}, '_'},
                                                [{is_integer, '$1'}, {'<', '$1', ExpiredSlide}],
                                                ['true'] }]).

% Private functions

current_slide() ->
    folsom_utils:now_epoch() div ?DEFAULT_STATS_INTERVAL.

get_interval(Name, SlideNumber) ->
    case ets:lookup(?SIMPLE_STATISTICS_TABLE, {Name, SlideNumber}) of
        [{_, Interval}] -> Interval;
        _ -> #interval{}
    end.


op(_Op, undefined, undefined) -> undefined;
op(_Op, A, undefined) -> A;
op(_Op, undefined, A) -> A;
op(Op, A, B) -> apply(erlang, Op, [A, B]).

merge(#interval{count = CountA, min = MinA, max = MaxA, sum = SumA},
      #interval{count = CountB, min = MinB, max = MaxB, sum = SumB}) ->
    #interval{count = CountA + CountB,
              min = op(min, MinA, MinB),
              max = op(max, MaxA, MaxB),
              sum = SumA + SumB}.

get_mean(_Sum, 0) -> undefined;
get_mean(Sum, Count) -> Sum / Count.
