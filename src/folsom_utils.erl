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
%%% File:      folsom_utils.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% various util functions
%%% @end
%%%------------------------------------------------------------------
-module(folsom_utils).

-export([
         to_atom/1,
         convert_tags/1,
         now_epoch/0,
         now_epoch_micro/0,
         get_ets_size/1,
         update_counter/3,
         update_counter/4,
         update_element/4
        ]).

to_atom(Binary) when is_binary(Binary) ->
    list_to_atom(binary_to_list(Binary));
to_atom(List) when is_list(List) ->
    list_to_atom(List).

convert_tags(Tags) ->
    [to_atom(Tag) || Tag <- Tags].

now_epoch() ->
    {Mega, Sec, _} = os:timestamp(),
    (Mega * 1000000 + Sec).

now_epoch_micro() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

get_ets_size(Tab) ->
    ets:info(Tab, size).

%% @edoc
%% Same as {@link ets:update_counter/3} but inserts `{Key, Value}' if object
%% is missing in the table.
update_counter(Tid, Key, Value) when is_integer(Value) ->
    %% try to update the counter, will badarg if it doesn't exist
    try ets:update_counter(Tid, Key, Value) of
        Res ->
            Res
    catch
        error:badarg ->
            %% row didn't exist, create it
            %% use insert_now to avoid races
            case ets:insert_new(Tid, {Key, Value}) of
                true ->
                    Value;
                false ->
                    %% someone beat us to it
                    ets:update_counter(Tid, Key, Value)
            end
    end.

%% @edoc
%% Similar to {@link ets:update_counter/3} when called with operations, eg:
%%   ets:update_counter(Tab, Key, UpdateOp | [UpdateOp])
%% but automatically initializes the object if missing from the table. The
%% inserted value is a `Size+1' tuple of the form `{Key, 0, 0, ... 0}'.
update_counter(Tid, Key, Size, Ops) ->
    %% try to update the counter, will badarg if it doesn't exist
    try ets:update_counter(Tid, Key, Ops) of
        Res ->
            Res
    catch
        error:badarg ->
            %% row didn't exist, create it
            %% use insert_now to avoid races
            T = erlang:make_tuple(Size+1, 0, [{1, Key}]),
            ets:insert_new(Tid, T),
            ets:update_counter(Tid, Key, Ops)
    end.

%% @edoc
%% Similar to {@link ets:update_element/3} but automatically initializes
%% the object if missing from the table. The initalized value is a `Size+1'
%% tuple of the form `{Key, Default, Default, .., Default}'.
update_element(Tid, Key, {Size, Default}, Ops) ->
    case ets:update_element(Tid, Key, Ops) of
        true ->
            ok;
        false ->
            %% row didn't exist, create it
            %% use insert_now to avoid races
            T = erlang:make_tuple(Size+1, Default, [{1, Key}]),
            ets:insert_new(Tid, T),
            ets:update_element(Tid, Key, Ops)
    end.
