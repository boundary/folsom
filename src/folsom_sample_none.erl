%%%-------------------------------------------------------------------
%%% File:      folsom_sample_none.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% no sampling, just a capped circular buffer
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_sample_none).

-export([
         new/1,
         update/2,
         get_values/1
        ]).

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

% internal api

drop_last([_]) ->
    [];
drop_last([H|T]) ->
    [H | drop_last(T)].
