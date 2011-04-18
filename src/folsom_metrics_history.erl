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

-export([new/3
         update/3,
         get_events/1,
         get_events/2,
         get_tagged_events/3]).

-include("folsom.hrl")

-record(history, {
          id,
          tags = [],
          size = 5000
         }).

-define(ETSOPTS, [
                  named_table,
                  ordered_set,
                  public
                 ]).

new(Id, Size, Tags) ->
    Id = ets:new(Id, ?ETSOPTS),
    {ok, #events{id = Id, size = Size, tags = Tags}}.

update(Id, Tags, Value) ->
    Key = folsom_utils:now_epoch_micro(),
    insert(Id, Key, Size, Tags, Event, ets:info(Id, size)).

get_info(Id) ->
    ok.

get_events(Id) ->
    get_events(Id, ?DEFAULT_LIMIT).

get_events(Id, Tag) when is_atom(Tag) ->
    get_tagged_events(Id, Tag, ?DEFAULT_LIMIT);
get_events(Id, Count) when is_integer(Count) ->
    get_last_events(Id, Count).

get_tagged_events(Id, Tag, Count) ->
    LastKey = ets:last(Id),
    get_prev_event(Id, LastKey, Count, Tag, []).

% Internal API

insert(Id, Key, Size, Tags, Event, Count) when is_list(Event) ->
    insert(Id, Key, Size, Tags, list_to_binary(Event), Count);
insert(Id, Key, Size, Tags, Event, Count) when Count < Size ->
    true = ets:insert(Id, {Key, [{tags, Tags}, {event, Event}]});
insert(Id, Key, _, Tags, Event, _) ->
    FirstKey = ets:first(Id),
    true = ets:delete(Id, FirstKey),
    true = ets:insert(Id, {Key, [{tags, Tags}, {event, Event}]}).

get_last_events(Id, Count) ->
    LastKey = ets:last(Id),
    get_prev_event(Id, LastKey, Count, []).

% get_prev_event/4 used by get_last_events/2
get_prev_event(_, '$end_of_table', _, Acc) ->
    Acc;
get_prev_event(Id, Key, Count, Acc) when length(Acc) < Count ->
    Event = ets:lookup(Id, Key),
    get_prev_event(Id, ets:prev(Id, Key), Count, lists:append(Acc, Event));
get_prev_event(_, _, _, Acc) ->
    Acc.

% get_prev_event/5 used by get_tagged_events/3
get_prev_event(_, '$end_of_table', _, _, Acc) ->
    Acc;
get_prev_event(Id, Key, Count, Tag, Acc) when length(Acc) < Count ->
    [{Key, Value}] = ets:lookup(Id, Key),
    Tags = proplists:get_value(tags, Value),
    maybe_append_event(lists:member(Tag, Tags), [{Key, Value}], Id, Key, Count, Tag, Acc);
get_prev_event(_, _, _, _, Acc) ->
    Acc.

maybe_append_event(true, Event, Id, Key, Count, Tag, Acc) ->
    get_prev_event(Id, ets:prev(Id, Key), Count, Tag, lists:append(Acc, Event));
maybe_append_event(false, _, Id, Key, Count, Tag, Acc) ->
    get_prev_event(Id, ets:prev(Id, Key), Count, Tag, Acc).
