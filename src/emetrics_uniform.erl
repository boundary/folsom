%%%-------------------------------------------------------------------
%%% File:      emetrics_uniform.erl
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

-module(emetrics_uniform).

-export([update/2, test/0]).

-define(SIZE, 5).
-define(RAND, 999999999999).

update([], Value) ->
    [Value];
update(List, Value) when length(List) < ?SIZE ->
    lists:append(List, [Value]);
update(List, Value) ->
    update(List, Value, rand(length(List))).

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
