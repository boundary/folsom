%%%-------------------------------------------------------------------
%%% File:      folsom_sample_none.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% no sampling, just a capped circular buffer
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_sample_none).

-export([new/1, update/2, get_values/1, test/0]).

-record(none, {
    size = 5000,
    reservoir = []
}).

new(Size) ->
    #none{size = Size}.

update(#none{reservoir = []} = Sample, Value) ->
    Sample#none{reservoir = [Value]};
update(#none{size = Size, reservoir = Reservoir} = Sample, Value) when length(Reservoir) < Size ->
    Sample#none{reservoir = lists:append(Reservoir, [Value])};
update(#none{reservoir = Reservoir} = Sample, Value) ->
    NewReservoir = [Value | drop_last(Reservoir)],
    Sample#none{reservoir = NewReservoir}.

get_values(#none{reservoir = Reservoir}) ->
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

drop_last([_]) ->
    [];
drop_last([H|T]) ->
    [H | drop_last(T)].
