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

-module(folsom_metrics_histogram).

-export([new/1,
         new/2,
         new/3,
         new/4,
         update/2,
         clear/1,
         time_and_update/4,
         get_value/1,
         get_values/1,
         get_statistics/1
         ]).

-record(histogram, {
          size,
          type = uniform,
          sample
         }).

-include("folsom.hrl").

new(Name) ->
    new(Name, uniform).

new(Name, SampleType) ->
    new(Name, SampleType, ?DEFAULT_SIZE).

new(Name, SampleType, SampleSize) ->
    Sample = folsom_sample_api:new(SampleType, SampleSize),
    Hist = #histogram{size = SampleSize, type = SampleType, sample = Sample},
    ets:insert(?HISTOGRAM_TABLE, {Name, Hist}).

new(Name, SampleType, SampleSize, Alpha) ->
    Sample = folsom_sample_api:new(SampleType, SampleSize, Alpha),
    Hist = #histogram{size = SampleSize, type = SampleType, sample = Sample},
    ets:insert(?HISTOGRAM_TABLE, {Name, Hist}).

update(Name, Value) ->
    Hist = get(Name),
    NewSample = folsom_sample_api:update(Hist#histogram.type, Hist#histogram.sample, Value),
    ets:insert(?HISTOGRAM_TABLE, {Name, Hist#histogram{sample = NewSample}}).

clear(Name) ->
    Hist = get(Name),
    ets:insert(?HISTOGRAM_TABLE, {Name, Hist#histogram{sample = []}}).

time_and_update(Name, Module, Fun, Args) ->
    Start = folsom_utils:now_epoch_micro(),
    erlang:apply(Module, Fun, Args),
    Stop = folsom_utils:now_epoch_micro(),
    update(Name, Stop - Start).

% gets the histogram record from ets
get_value(Name) ->
    [{_, Value}] = ets:lookup(?HISTOGRAM_TABLE, Name),
    Value.

% pulls the sample out of the record gotten from ets
get_values(Name) ->
    Hist = get_value(Name),
    Sample = Hist#histogram.sample,
    Type = Hist#histogram.type,
    get_values(Type, Sample).

get_values(uniform, Sample) ->
    Sample#uniform.reservoir;
get_values(none, Sample) ->
    Sample#none.reservoir;
get_values(exdec, Sample) ->
    Sample#exdec.reservoir.

% calculates stats on a sample
get_statistics(Name) when is_atom(Name)->
    Values = get_values(Name),
    get_statistics(Values);
get_statistics(Values) when is_list(Values) ->
    [
     {min, folsom_statistics:get_min(Values)},
     {max, folsom_statistics:get_max(Values)},
     {mean, folsom_statistics:get_mean(Values)},
     {median, folsom_statistics:get_median(Values)},
     {variance, folsom_statistics:get_variance(Values)},
     {standard_deviation, folsom_statistics:get_standard_deviation(Values)},
     {skewness, folsom_statistics:get_skewness(Values)},
     {kurtosis, folsom_statistics:get_kurtosis(Values)},
     {percentile,
      [
       {75, folsom_statistics:get_percentile(Values, 0.75)},
       {95, folsom_statistics:get_percentile(Values, 0.95)},
       {99, folsom_statistics:get_percentile(Values, 0.99)},
       {999, folsom_statistics:get_percentile(Values, 0.999)}
      ]
     },
     {histogram, folsom_statistics:get_histogram(Values)}
     ].
