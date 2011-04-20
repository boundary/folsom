%%%
%%% Copyright 2011, fast_ip
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
%%% File:      folsom_sample_uniform.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% erlang implementation of a uniform random sample
%%% based on a java implementation by coda hale, which can be found at:
%%%
%%% https://github.com/codahale/metrics/blob/development/src/main/java/com/yammer/metrics/core/UniformSample.java
%%%
%%% that implementation is based on:
%%%
%%% http://www.cs.umd.edu/~samir/498/vitter.pdf
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_sample_uniform).

-export([
         new/1,
         update/2,
         get_values/1
        ]).

-define(RAND, 999999999999).

-include("folsom.hrl").


new(Size) ->
    #uniform{size = Size}.

update(#uniform{reservoir = []} = Sample, Value) ->
    Sample#uniform{reservoir = [Value]};
update(#uniform{size = Size, reservoir = Reservoir} = Sample, Value) when length(Reservoir) < Size ->
    Sample#uniform{reservoir = lists:append(Reservoir, [Value])};
update(#uniform{reservoir = Reservoir} = Sample, Value) ->
    NewReservoir = update(Reservoir, Value, rand(length(Reservoir))),
    Sample#uniform{reservoir = NewReservoir}.

get_values(#uniform{reservoir = Reservoir}) ->
    Reservoir.

% internal api

update([_ | Tail], Value, Rand) when Rand == 0 ->
    [Value | Tail];
update(List, Value, Rand) when Rand < length(List) ->
    {List1, List2} = lists:split(Rand, List),
    List3 = lists:append(drop_last(List1), [Value]),
    lists:append(List3, List2).

drop_last([_]) ->
    [];
drop_last([H|T]) ->
    [H | drop_last(T)].

rand(Count) ->
    erlang:abs(random:uniform(?RAND)) rem Count.
