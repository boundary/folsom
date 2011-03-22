%%%-------------------------------------------------------------------
%%% File:      emetrics_exdec.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
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

-module(emetrics_exdec).

-export([new/0, update/2, update/3, get_values/1, test/0]).

-define(SIZE, 5).
-define(ALPHA, 1).
-define(HOURSECS, 3600).
-define(RAND, 999999999999).

new() ->
    Now = tick(),
    [{start, Now}, {next, Now + ?HOURSECS}, {reservoir, []}].

update(List, Value) ->
    update(List, Value, tick()).

update([{start, Start}, {next, Next}, {reservoir, Reservoir}], Value, Tick) when length(Reservoir) < ?SIZE ->
    NewList = lists:append(Reservoir, [{priority(Tick, Start), Value}]),
    [{start, Start}, {next, Next}, {reservoir, NewList}];
update([{start, Start}, {next, Next}, {reservoir, [{FirstTime, _} | Tail]}], Value, Tick) ->
    Priority = priority(Tick, Start),
    NewList = case FirstTime < Priority of
        true ->
            lists:append(Tail, [{Priority, Value}]);
        _ ->
            Tail
    end,

    Now = Tick,
    case Now >= Next of
        true ->
            rescale([{start, Start}, {next, Next}, {reservoir, NewList}], Now);
        _ ->
            [{start, Start}, {next, Next}, {reservoir, NewList}]
    end.

get_values([_, _, {reservoir, Reservoir}]) ->
    {_, Values} = lists:unzip(Reservoir),
    Values.

% internal api

tick() ->
    {Mega, Sec, _} = erlang:now(),
    (Mega * 1000000 + Sec).

weight(T) ->
    math:exp(?ALPHA * T).

priority(Time, Start) ->
    weight(Time - Start) / random:uniform(?RAND).

rescale([{start, OldStart}, {next, Next}, {reservoir, Reservoir}], Now) when Next == Now ->
    NewNext = Now + ?HOURSECS,
    NewStart = tick(),
    NewReservoir = [{Key * math:exp(-?ALPHA * (NewStart - OldStart)), Value} || {Key, Value} <- Reservoir],
    [{start, NewStart}, {next, NewNext}, {reservoir, NewReservoir}].
