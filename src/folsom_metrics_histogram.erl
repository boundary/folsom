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

-exports([new/2,
          new/3,
          update/2,
          clear/1]).

-record(histogram, {
          id,
          size,
          type = uniform,
          sample
         }).


new(Name, SampleType) ->
    new(Name, SampleType, ?DEFAULT_SIZE).

new(Name, SampleType, SampleSize) ->
    Sample = folsom_sample_api:new(Type, Size),
    Hist = #histogram{id = Name, size = SampleSize, type = SampleType, sample = Sample},
    ets:insert(?HISTOGRAM_TABLE, {Name, Hist}).

update(Name, Value) ->
    {_, Hist} = ets:lookup(?HISTOGRAM_TABLE, Name),
    NewSample = folsom_sample_api:update(Hist#histogram.type, Hist#histogram.sample, Value),
    ets:insert(?HISTOGRAM_TABLE, {Name, Hist#histogram{sample = NewSample}}).

clear(Name) ->
    {_, Hist} = ets:lookup(?HISTOGRAM_TABLE, Name),
    ets:insert(?HISTOGRAM_TABLE, {Name, Hist#histogram{sample = []}}).
