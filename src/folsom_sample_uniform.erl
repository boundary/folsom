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

-export([new/1, update/2, get_values/1, test/0]).

-define(RAND, 999999999999).

-record(uniform, {
    size = 5000,
    reservoir = []
}).

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

test() ->
    List = new(5),
    List1 = update(List, 1),
    List2 = update(List1, 2),
    List3 = update(List2, 3),
    List4 = update(List3, 4),
    List5 = update(List4, 5),
    List6 = update(List5, 6),
    List7 = update(List6, 7),
    List8 = update(List7, 8),
    List9 = update(List8, 9),
    List10 = update(List9, 10),
    io:format("~p~n", [get_values(List10)]).

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
