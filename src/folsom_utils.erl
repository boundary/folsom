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
         to_atom/1,
         convert_tags/1,
         now_epoch/0,
         now_epoch_micro/0
        ]).

to_atom(Binary) when is_binary(Binary) ->
    list_to_atom(binary_to_list(Binary));
to_atom(List) when is_list(List) ->
    list_to_atom(List).

convert_tags(Tags) ->
    [to_atom(Tag) || Tag <- Tags].

now_epoch() ->
    {Mega, Sec, _} = erlang:now(),
    (Mega * 1000000 + Sec).

now_epoch_micro() ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.
