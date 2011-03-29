%%%-------------------------------------------------------------------
%%% @author joe williams <j@fastip.com>
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by joe williams <j@fastip.com>
%%%-------------------------------------------------------------------
-module(emetrics_event_event).

-behaviour(gen_event).

%% API
-export([add_handler/3,
         add_sup_handler/3,
         delete_handler/1,
         handler_exists/1,
         notify/1,
         get_handlers/0,
         get_info/1,
         get_events/2]).

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

-define(DEFAULT_LIMIT, 10).

%%%===================================================================
%%% API
%%%===================================================================

add_handler(Id, Tags, Size) ->
    gen_event:add_handler(emetrics_event_event_manager,
                          {emetrics_event, Id}, [Id, Tags, Size]).

add_sup_handler(Id, Tags, Size) ->
    gen_event:add_sup_handler(emetrics_event_event_manager,
                              {emetrics_event, Id}, [Id, Tags, Size]).

delete_handler(Id) ->
    gen_event:delete_handler(emetrics_event_event_manager, {emetrics_event, Id}, nil).

handler_exists(Id) ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(emetrics_event_event_manager)),
    lists:member(Id, Handlers).

notify(Event) ->
    gen_event:notify(emetrics_event_event_manager, Event).

get_handlers() ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(emetrics_event_event_manager)),
    Handlers.

get_info(Id) ->
    gen_event:call(emetrics_event_event_manager, {emetrics_event_event, Id}, info).

get_events(Id, Count) ->
    gen_event:call(emetrics_event_event_manger, {emetrics_event_event, Id}, {events, Count}).

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
handle_event({Id, Tags, Event}, #events{size = _Size} = State) ->
    Key = erlang:now(), % just use erlang time for now
    % TODO: limit ets table size by Size
    true = ets:insert(Id, {Key, [{tags, Tags}, {event, Event}]}),
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
    {ok, {Id, Size, Tags, ets:info(Id, size)}, State};
handle_call(events, #events{id = Id} = State) ->
    Events = get_last_events(Id, ?DEFAULT_LIMIT),
    {ok, Events, State};
handle_call({events, Tag}, #events{id = Id} = State) when is_atom(Tag) ->
    Events =  get_tagged_events(Id, Tag, ?DEFAULT_LIMIT),
    {ok, Events, State};
handle_call({events, Count}, #events{id = Id} = State) when is_integer(Count)->
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

get_last_events(_Id, _Count) ->
    % get last X events from ets table
    ok.

get_tagged_events(_Id, _Tag, _Count) ->
    % get last X events from ets table with a tag
    ok.
