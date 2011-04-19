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
%%% @author joe williams <j@boundary.com>
%%% @doc
%%% @end
%%% Created : 22 Mar 2011 by joe williams <j@boundary.com>
%%%------------------------------------------------------------------

-module(folsom_metrics_meter).

-export([new/1,
         new/2,
         tick/1,
         mark/1,
         mark/2,
         one_minute_rate/1,
         five_minute_rate/1,
         fifteen_minute_rate/1,
         mean_rate/1,
         get_value/1
        ]).


-record(meter, {
          one,
          five,
          fifteen,
          count = 0,
          start_time,
          interval
         }).

-include("folsom.hrl").

new(Name) ->
    new(Name, 5).

new(Name, Interval) ->
    OneMin = folsom_ewma:one_minute_ewma(),
    FiveMin = folsom_ewma:five_minute_ewma(),
    FifteenMin = folsom_ewma:fifteen_minute_ewma(),
    timer:send_interval(Interval, {meter_tick, Name}),
    ets:insert(?METER_TABLE,{Name, #meter{one = OneMin, five = FiveMin, fifteen = FifteenMin, interval = Interval, start_time = folsom_utils:now_epoch_micro()}}).

tick(Name) ->
    #meter{one = OneMin, five = FiveMin, fifteen = FifteenMin} = Meter = get(Name),
    OneMin1 = folsom_ewma:tick(OneMin),
    FiveMin1 = folsom_ewma:tick(FiveMin),
    FifteenMin1 = folsom_ewma:tick(FifteenMin),
    ets:insert(?METER_TABLE, {Name, Meter#meter{one = OneMin1, five = FiveMin1, fifteen = FifteenMin1}}).

mark(Name) ->
    mark(Name, 1).

mark(Name, Value) ->
    #meter{count = Count, one = OneMin, five = FiveMin, fifteen = FifteenMin} = Meter = get(Name),
    OneMin1 = folsom_ewma:update(OneMin, Value),
    FiveMin1 = folsom_ewma:update(FiveMin, Value),
    FifteenMin1 = folsom_ewma:update(FifteenMin, Value),
    ets:insert(?METER_TABLE, {Name, Meter#meter{count = Count + Value, one = OneMin1, five = FiveMin1, fifteen = FifteenMin1}}).

one_minute_rate(Name) ->
    #meter{one = OneMin} = get(Name),
    folsom_ewma:rate(OneMin).

five_minute_rate(Name) ->
    #meter{five = FiveMin} = get(Name),
    folsom_ewma:rate(FiveMin).

fifteen_minute_rate(Name) ->
    #meter{fifteen = FifteenMin} = get(Name),
    folsom_ewma:rate(FifteenMin).

mean_rate(Name) ->
    #meter{count = Count, start_time = Start} = get(Name),
    calc_mean_rate(Start, Count).

get_value(Name) ->
    {_, Value} = ets:lookup(?METER_TABLE, Name),
    Value.

% Internal Functions

calc_mean_rate(_, 0) ->
    0.0;
calc_mean_rate(Start, Count) ->
    Elapsed = folsom_utils:now_epoch_micro() - Start,
    Count / Elapsed.
