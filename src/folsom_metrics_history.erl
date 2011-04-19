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
%%% @author joe williams <j@boundary.com>
%%% @doc
%%% @end
%%% Created : 22 Mar 2011 by joe williams <j@boundary.com>
%%%------------------------------------------------------------------

-module(folsom_metrics_history).

-export([new/1,
         update/4,
         get_events/1,
         get_events/2
        ]).

-include("folsom.hrl").

-define(ETSOPTS, [
                  named_table,
                  ordered_set,
                  public
                 ]).

-compile({no_auto_import,[get/1]}).

new(Name) ->
    ets:new(Name, ?ETSOPTS).

update(Name, Size, Tags, Value) ->
    Key = folsom_utils:now_epoch_micro(),
    insert(Name, Key, Size, Tags, Value, ets:info(Name, size)).

get_events(Name) ->
    get_events(Name, ?DEFAULT_LIMIT).

get_events(Name, Count) ->
    get_last_events(Name, Count).

% Internal API

insert(Name, Key, Size, Tags, Value, Count) when is_list(Value) ->
    insert(Name, Key, Size, Tags, list_to_binary(Value), Count);
insert(Name, Key, Size, Tags, Value, Count) when Count < Size ->
    true = ets:insert(Name, {Key, [{tags, Tags}, {event, Value}]});
insert(Name, Key, _, Tags, Value, _) ->
    FirstKey = ets:first(Name),
    true = ets:delete(Name, FirstKey),
    true = ets:insert(Name, {Key, [{tags, Tags}, {event, Value}]}).

get_last_events(Name, Count) ->
    LastKey = ets:last(Name),
    get_prev_event(Name, LastKey, Count, []).

% get_prev_event/4 used by get_last_events/2
get_prev_event(_, '$end_of_table', _, Acc) ->
    Acc;
get_prev_event(Name, Key, Count, Acc) when length(Acc) < Count ->
    Event = ets:lookup(Name, Key),
    get_prev_event(Name, ets:prev(Name, Key), Count, lists:append(Acc, Event));
get_prev_event(_, _, _, Acc) ->
    Acc.

% get_prev_event/5 used by get_tagged_events/3
get_prev_event(_, '$end_of_table', _, _, Acc) ->
    Acc;
get_prev_event(Name, Key, Count, Tag, Acc) when length(Acc) < Count ->
    [{Key, Value}] = ets:lookup(Name, Key),
    Tags = proplists:get_value(tags, Value),
    maybe_append_event(lists:member(Tag, Tags), [{Key, Value}], Name, Key, Count, Tag, Acc);
get_prev_event(_, _, _, _, Acc) ->
    Acc.

maybe_append_event(true, Event, Name, Key, Count, Tag, Acc) ->
    get_prev_event(Name, ets:prev(Name, Key), Count, Tag, lists:append(Acc, Event));
maybe_append_event(false, _, Name, Key, Count, Tag, Acc) ->
    get_prev_event(Name, ets:prev(Name, Key), Count, Tag, Acc).
