%%%-------------------------------------------------------------------
%%% @author joe williams <j@fastip.com>
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by joe williams <j@fastip.com>
%%%-------------------------------------------------------------------
-module(folsom_events_event).

-behaviour(gen_event).

-include("folsom.hrl").

%% API
-export([add_handler/3,
         add_sup_handler/3,
         delete_handler/1,
         handler_exists/1,
         notify/1,
         get_handlers/0,
         get_handlers_info/0,
         get_tagged_handlers/1,
         get_info/1,
         get_events/1,
         get_events/2,
         get_events/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(events, {
          id,
          tags = [],
          size = 5000
         }).

-define(ETSOPTS, [named_table,
                  ordered_set]).

%%%===================================================================
%%% API
%%%===================================================================

add_handler(Id, Tags, Size) ->
    gen_event:add_handler(folsom_events_event_manager,
                          {?MODULE, Id}, [Id, Tags, Size]).

add_sup_handler(Id, Tags, Size) ->
    gen_event:add_sup_handler(folsom_events_event_manager,
                              {?MODULE, Id}, [Id, Tags, Size]).

delete_handler(Id) ->
    gen_event:delete_handler(folsom_events_event_manager, {?MODULE, Id}, nil).

handler_exists(Id) ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(folsom_events_event_manager)),
    lists:member(Id, Handlers).

notify(Event) ->
    gen_event:notify(folsom_events_event_manager, Event).

get_handlers() ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(folsom_events_event_manager)),
    Handlers.

get_handlers_info() ->
    folsom_utils:get_handlers_info(?MODULE).

get_tagged_handlers(Tag) ->
    folsom_utils:get_tagged_handlers(?MODULE, Tag).

get_info(Id) ->
    gen_event:call(folsom_events_event_manager, {?MODULE, Id}, info).

get_events(Id) ->
    get_events(Id, ?DEFAULT_LIMIT).

get_events(Id, Tag) when is_atom(Tag) ->
    get_events(Id, Tag, ?DEFAULT_LIMIT);
get_events(Id, Count) when is_integer(Count) ->
    get_events(Id, undefined, Count).

get_events(Id, Tag, Count) ->
    gen_event:call(folsom_events_event_manager, {folsom_events_event, Id}, {events, Tag, Count}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([Id, Tags, Size]) ->
    Id = ets:new(Id, ?ETSOPTS),
    {ok, #events{id = Id, size = Size, tags = Tags}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({Id, Tags, Event}, #events{id = Id1, size = Size} = State) when Id == Id1->
    {Mega, Sec, Micro} = erlang:now(),
    Key = (Mega * 1000000 + Sec) * 1000000 + Micro,
    insert(Id, Key, Size, Tags, Event, ets:info(Id, size)),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(info, #events{id = Id, size = Size, tags = Tags} = State) ->
    {ok, [{Id, [
                {size, Size},
                {tags, Tags}
               ]}], State};
handle_call({events, undefined, Count}, #events{id = Id} = State) ->
    Events = get_last_events(Id, Count),
    {ok, Events, State};
handle_call({events, Tag, Count}, #events{id = Id} = State) ->
    Events = get_tagged_events(Id, Tag, Count),
    {ok, Events, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

get_tagged_events(Id, Tag, Count) ->
    LastKey = ets:last(Id),
    get_prev_event(Id, LastKey, Count, Tag, []).

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
