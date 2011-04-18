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
         mean_rate/1
        ]).


-record(meter, {
          one,
          five,
          fifteen,
          count = 0,
          name,
          start_time
         }).

new(Name) ->
    new(Name, 5).

new(Name, Interval) ->
    OneMin = folsom_ewma:one_minute_ewma(),
    FiveMin = folsom_ewma:five_minute_ewma(),
    FifteenMin = folsom_ewma:fifteen_minute_ewma(),
    timer:send_interval(Interval, {meter_tick, Name}),
    #meter{name = Name, one = OneMin, five = FiveMin, fifteen = FifteenMin, start_time = folsom_utils:now_epoch_micro()}.

tick(#meter{one = OneMin, five = FiveMin, fifteen = FifteenMin} = Meter) ->
    OneMin1 = folsom_ewma:tick(OneMin),
    FiveMin1 = folsom_ewma:tick(FiveMin),
    FifteenMin1 = folsom_ewma:tick(FifteenMin),
    Meter#meter{one = OneMin1, five = FiveMin1, fifteen = FifteenMin1}.

mark(Meter) ->
    mark(Meter, 1).

mark(#meter{count = Count, one = OneMin, five = FiveMin, fifteen = FifteenMin} = Meter, Value) ->
    OneMin1 = folsom_ewma:update(OneMin, Value),
    FiveMin1 = folsom_ewma:update(FiveMin, Value),
    FifteenMin1 = folsom_ewma:update(FifteenMin, Value),
    Meter#meter{count = Count + Value, one = OneMin1, five = FiveMin1, fifteen = FifteenMin1}.

one_minute_rate(#meter{one = OneMin}) ->
    folsom_ewma:rate(OneMin).

five_minute_rate(#meter{five = FiveMin}) ->
    folsom_ewma:rate(FiveMin).

fifteen_minute_rate(#meter{fifteen = FifteenMin}) ->
    folsom_ewma:rate(FifteenMin).

mean_rate(#meter{count = 0}) ->
    0.0;
mean_rate(#meter{start_time = Start, count = Count}) ->
    Elapsed = folsom_utils:now_epoch_micro() - Start,
    Count / Elapsed.
