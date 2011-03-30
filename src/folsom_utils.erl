%%%-------------------------------------------------------------------
%%% File:      folsom_utils.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% various util functions
%%% @end
%%%------------------------------------------------------------------
-module(folsom_utils).

-export([
         binary_to_atom/1,
         convert_tags/1,
         now_epoch/0,
         now_epoch_micro/0
        ]).

binary_to_atom(Binary) ->
    list_to_atom(binary_to_list(Binary)).

convert_tags(Tags) ->
    [binary_to_atom(Tag) || Tag <- Tags],

now_epoch() ->
    {Mega, Sec, _} = erlang:now(),
    (Mega * 1000000 + Sec).

now_epoch_micro() ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.
