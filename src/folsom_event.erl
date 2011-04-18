%%%
%%% Copyright 2011, fast_ip
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
%%% @author joe williams <j@fastip.com>
%%% @doc
%%% _metrics
%%% @end
%%% Created : 22 Mar 2011 by joe williams <j@fastip.com>
%%%-------------------------------------------------------------------
-module(folsom_event).

-behaviour(gen_event).

%% API
-export([add_handler/4,
         add_handler/5,
         add_sup_handler/4,
         add_sup_handler/5,
         delete_handler/1,
         handler_exists/1,
         notify/1,
         get_handlers/0,
         get_handlers_info/0,
         get_tagged_handlers/1,
         get_values/1,
         get_aggregated_values/1,
         get_info/1,
         get_statistics/1,
         get_aggregated_statistics/1
        ]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).


-record(metric, {
          id,
          size,
          tags = [],
          type,
          sample
         }).

-define(EVENTMGR, folsom_event_manager).

%%%===================================================================
%%% API
%%%===================================================================

% generic event handling api

add_handler(Manager, Module, Id, Args) ->
    gen_event:add_handler(Manager, {Module, Id}, Args).

add_sup_handler(Manager, Module, Id, Args) ->
    gen_event:add_sup_handler(Manager, {Module, Id}, Args).

delete_handler(Manager, Module, Id) ->
    gen_event:delete_handler(Manager, {Module, Id}, nil).

handler_exists(Manager, Id) ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(Manager)),
    lists:member(Id, Handlers).

notify(Manager, Event) ->
    gen_event:notify(Manager, Event).

get_handlers_info(Manager, Module) ->
    Handlers = get_handlers(Manager),
    [get_info(Manager, Module, Id) || Id <- Handlers].

get_tagged_handlers(Manager, Module, Tag) ->
    Handlers = get_handlers(Manager),
    List = [get_tags_from_info(Manager, Module, Id) || Id <- Handlers],
    build_tagged_handler_list(List, Tag, []).

get_handlers(Manager) ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(Manager)),
    Handlers.

get_info(Manager, Module, Id) ->
    gen_event:call(Manager, {Module, Id}, info).

% internal functions

get_tags_from_info(Manager, Module, Id) ->
    [{Id, Values}] = get_info(Manager, Module, Id),
    Tags = proplists:get_value(tags, Values),
    {Id, Tags}.

build_tagged_handler_list([], _, Acc) ->
    Acc;
build_tagged_handler_list([{Id, Tags} | Tail], Tag, Acc) ->
    NewAcc = maybe_append_handler(lists:member(Tag, Tags), Id, Acc),
    build_tagged_handler_list(Tail, Tag, NewAcc).

maybe_append_handler(true, Id, Acc) ->
    lists:append([Id], Acc);
maybe_append_handler(false, _, Acc) ->
    Acc.


% _metrics specific api

get_values(Id) ->
    gen_event:call(?EVENTMGR, {?MODULE, Id}, values).

get_aggregated_values(Tag) ->
    Handlers = get_tagged_handlers(Tag),
    List = [get_values(Id) || Id <- Handlers],
    lists:append(List).

get_aggregated_statistics(Tag) ->
    Values = get_aggregated_values(Tag),
    get_statistics(Values).

get_statistics(Id) when is_atom(Id)->
    Values = get_values(Id),
    get_statistics(Values);
get_statistics(Values) when is_list(Values) ->
    [
     {min, folsom_statistics:get_min(Values)},
     {max, folsom_statistics:get_max(Values)},
     {mean, folsom_statistics:get_mean(Values)},
     {median, folsom_statistics:get_median(Values)},
     {variance, folsom_statistics:get_variance(Values)},
     {standard_deviation, folsom_statistics:get_standard_deviation(Values)},
     {skewness, folsom_statistics:get_skewness(Values)},
     {kurtosis, folsom_statistics:get_kurtosis(Values)},
     {percentile,
      [
       {75, folsom_statistics:get_percentile(Values, 0.75)},
       {95, folsom_statistics:get_percentile(Values, 0.95)},
       {99, folsom_statistics:get_percentile(Values, 0.99)},
       {999, folsom_statistics:get_percentile(Values, 0.999)}
      ]
     },
     {histogram, folsom_statistics:get_histogram(Values)}
     ].

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
init([Id, Type, Tags, Size]) ->
    Sample = folsom_sample_api:new(Type, Size),
    {ok, #metric{id = Id, type = Type, tags = Tags, size = Size, sample = Sample}};
init([Id, Type, Tags, Size, Alpha]) ->
    Sample = folsom_sample_api:new(Type, Size, Alpha),
    {ok, #metric{id = Id, type = Type, tags = Tags, size = Size, sample = Sample}}.


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
handle_event({Id, Value}, #metric{id = Id1, type = Type, sample = Sample} = State) when Id == Id1 ->
    NewSample = folsom_sample_api:update(Type, Sample, Value),
    {ok, State#metric{sample = NewSample}};
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
handle_call(info, #metric{id = Id, type = Type, tags = Tags, size = Size} = State) ->
    {ok, [{Id, [{size, Size}, {tags, Tags}, {type, Type}]}], State};
handle_call(values, #metric{type = Type, sample = Sample} = State) ->
    Values = folsom_sample_api:get_values(Type, Sample),
    {ok, Values, State}.

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
