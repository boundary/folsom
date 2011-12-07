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
%%% File:      folsom_sample_exdec.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% erlang implementation of a exponentially-decaying random sample
%%% based on a java implementation by coda hale, which can be found at:
%%%
%%% https://github.com/codahale/metrics/blob/development/src/main/java/com/yammer/metrics/core/ExponentiallyDecayingSample.java
%%%
%%% that implementation is based on:
%%%
%%% http://www.research.att.com/people/Cormode_Graham/library/publications/CormodeShkapenyukSrivastavaXu09.pdf
%%% @end
%%%------------------------------------------------------------------

-module(folsom_sample_exdec).

-export([
         new/2,
         update/2,
         get_values/1
        ]).

-define(HOURSECS, 3600).

-include("folsom.hrl").

new(Size, Alpha) ->
    Now = folsom_utils:now_epoch(),
    #exdec{start = Now, next = Now + ?HOURSECS, alpha = Alpha, size = Size}.

update(Sample, Value) ->
    update(Sample, Value, folsom_utils:now_epoch()).

get_values(#exdec{reservoir = Reservoir}) ->
    {_, Values} = lists:unzip(Reservoir),
    Values.

% internal api

update(#exdec{start = Start, alpha = Alpha, size = Size, reservoir = Reservoir, n = N, seed = Seed} = Sample, Value, Tick) when N =< Size ->
    {Rand, New_seed} = random:uniform_s(N, Seed),
    NewList = lists:append(Reservoir, [{priority(Alpha, Tick, Start, Rand), Value}]),
    Sample#exdec{reservoir = NewList, n = N+1, seed = New_seed};
update(#exdec{start = Start, alpha = Alpha, n = N, seed = Seed} = Sample, Value, Tick) ->
    {Rand, New_seed} = random:uniform_s(N, Seed),
    Priority = priority(Alpha, Tick, Start, Rand),
    NewSample = maybe_update(Priority, Value, Sample, New_seed),
    maybe_rescale(NewSample, folsom_utils:now_epoch()).

weight(Alpha, T) ->
    math:exp(Alpha * T).

priority(Alpha, Time, Start, Rand) ->
    weight(Alpha, Time - Start) / Rand.

maybe_update(Priority, Value, #exdec{reservoir = [{First, _}| Tail], n = N} = Sample, Seed) when First < Priority ->
    Sample#exdec{reservoir = lists:append(Tail, [{Priority, Value}]), n = N+1, seed = Seed};
maybe_update(_, _, Sample, _) ->
    Sample.

maybe_rescale(#exdec{next = Next} = Sample, Now) when Now >= Next ->
    rescale(Sample, Now);
maybe_rescale(Sample, _) ->
    Sample.

rescale(#exdec{start = OldStart, next = Next, alpha = Alpha, reservoir = Reservoir} = Sample, Now) when Next == Now ->
    NewNext = Now + ?HOURSECS,
    NewStart = folsom_utils:now_epoch(),
    NewReservoir = [{Key * math:exp(-Alpha * (NewStart - OldStart)), Value} || {Key, Value} <- Reservoir],
    Sample#exdec{start = NewStart, next = NewNext, reservoir = NewReservoir}.
