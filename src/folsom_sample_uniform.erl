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
%%% File:      folsom_sample_uniform.erl
%%% @author    joe williams <j@boundary.com>
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

%% update1(#uniform1{reservoir = dict} = Sample, Value) ->
%%     Sample#uniform1{reservoir = [Value]};
update(#uniform{size = Size, reservoir = Reservoir, n=N} = Sample, Value) when N < Size ->
    Sample#uniform{reservoir = dict:store(N, Value, Reservoir), n = N+1};

update(#uniform{reservoir = Reservoir, n = N} = Sample, Value) ->
    NewReservoir = dict:store(rand(N), Value, Reservoir),
    Sample#uniform{reservoir = NewReservoir}.

get_values(#uniform{reservoir = Reservoir}) ->
    [Val || {_,Val} <- dict:to_list(Reservoir)].

rand(Count) ->
    erlang:abs(random:uniform(?RAND)) rem Count.
