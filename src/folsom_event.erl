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
-export([add_handler/3,
         add_handler/4,
         add_handler/5,
         add_handler/6,
         add_sup_handler/3,
         add_sup_handler/4,
         add_sup_handler/5,
         add_sup_handler/6,
         delete_handler/1,
         handler_exists/1,
         notify/1,
         get_handlers/0,
         get_handlers_info/0,
         get_info/1,
         get_values/1,
         get_histogram_statistics/1
        ]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(EVENTMGR, folsom_event_manager).

-record(metric, {
          name,
          tags = [],
          type,
          history_size
         }).

%%%===================================================================
%%% API
%%%===================================================================

% generic event handling api

add_handler(Type, Name, Tags) ->
    gen_event:add_handler(?EVENTMGR, {?MODULE, Name}, [Type, Name, Tags]).

add_handler(Type, Name, Tags, SampleSize) ->
    gen_event:add_handler(?EVENTMGR, {?MODULE, Name}, [Type, Name, Tags, SampleSize]).

add_handler(Type, Name, Tags, SampleType, SampleSize) ->
    gen_event:add_handler(?EVENTMGR, {?MODULE, Name}, [Type, Name, Tags, SampleType, SampleSize]).

add_handler(Type, Name, Tags, SampleType, SampleSize, Alpha) ->
    gen_event:add_handler(?EVENTMGR, {?MODULE, Name}, [Type, Name, Tags, SampleType, SampleSize, Alpha]).

add_sup_handler(Type, Name, Tags) ->
    gen_event:add_handler(?EVENTMGR, {?MODULE, Name}, [Type, Name, Tags]).

add_sup_handler(Type, Name, Tags, SampleSize) ->
    gen_event:add_handler(?EVENTMGR, {?MODULE, Name}, [Type, Name, Tags, SampleSize]).

add_sup_handler(Type, Name, Tags, SampleType, SampleSize) ->
    gen_event:add_handler(?EVENTMGR, {?MODULE, Name}, [Type, Name, Tags, SampleType, SampleSize]).

add_sup_handler(Type, Name, Tags, SampleType, SampleSize, Alpha) ->
    gen_event:add_handler(?EVENTMGR, {?MODULE, Name}, [Type, Name, Tags, SampleType, SampleSize, Alpha]).

delete_handler(Id) ->
    gen_event:delete_handler(?EVENTMGR, ?MODULE, Id).

handler_exists(Id) ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(?EVENTMGR)),
    lists:member(Id, Handlers).

notify(Event) ->
    gen_event:notify(?EVENTMGR, Event).

get_handlers() ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(?EVENTMGR)),
    Handlers.

get_handlers_info() ->
    Handlers = get_handlers(),
    [get_info(Id) || Id <- Handlers].

get_info(Name) ->
    gen_event:call(?EVENTMGR, {?MODULE, Name}, info).

get_values(Name) ->
    [{_, Info}] = get_info(Name),
    gen_event:call(?EVENTMGR, {?MODULE, Name}, {proplists:get_value(type, Info), Name}).

get_histogram_statistics(Name) ->
    gen_event:call(?EVENTMGR, {?MODULE, Name}, {histogram_statistics, Name}).

% internal functions


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

%% Counter
init([counter, Name, Tags]) ->
    folsom_metrics_counter:new(Name),
    {ok, #metric{name = Name, type = counter, tags = Tags}};
%% Gauge
init([gauge, Name, Tags]) ->
    folsom_metrics_gauge:new(Name),
    {ok, #metric{name = Name, type = gauge, tags = Tags}};
%% Histogram
init([histogram, Name, Tags]) ->
    folsom_metrics_histogram:new(Name),
    {ok, #metric{name = Name, type = histogram, tags = Tags}};
init([histogram, Name, Tags, SampleType]) ->
    folsom_metrics_histogram:new(Name, SampleType),
    {ok, #metric{name = Name, type = histogram, tags = Tags}};
init([histogram, Name, Tags, SampleType, SampleSize]) ->
    folsom_metrics_histogram:new(Name, SampleType, SampleSize),
    {ok, #metric{name = Name, type = histogram, tags = Tags}};
init([histogram, Name, Tags, SampleType, SampleSize, Alpha]) ->
    folsom_metrics_histogram:new(Name, SampleType, SampleSize, Alpha),
    {ok, #metric{name = Name, type = histogram, tags = Tags}};
%% History
init([history, Name, SampleSize, Tags]) ->
    folsom_metrics_history:new(Name, SampleSize),
    {ok, #metric{name = Name, type = history, tags = Tags}};
%% Meter
init([meter, Name, Interval, Tags]) ->
    folsom_metrics_meter:new(Name, Interval),
    {ok, #metric{name = Name, type = meter, tags = Tags}}.

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

%% Counter Increment
handle_event({Name, {inc, Value}}, #metric{name = Name1, type = counter} = State) when Name == Name1 ->
    folsom_metrics_counter:inc(Name, Value),
    {ok, State};
%% Counter Decrement
handle_event({Name, {dec, Value}}, #metric{name = Name1, type = counter} = State) when Name == Name1 ->
    folsom_metrics_counter:dec(Name, Value),
    {ok, State};
%% Gauge
handle_event({Name, Value}, #metric{name = Name1, type = gauge} = State) when Name == Name1 ->
    folsom_metrics_gauge:update(Name, Value),
    {ok, State};
%% Histogram
handle_event({Name, Value}, #metric{name = Name1, type = histogram} = State) when Name == Name1 ->
    folsom_metrics_histogram:update(Name, Value),
    {ok, State};
%% History
handle_event({Name, {Tags, Value}}, #metric{name = Name1, type = history, history_size = HistorySize} = State) when Name == Name1 ->
    folsom_metrics_history:update(Name, HistorySize, Tags, Value),
    {ok, State};
%% Meter
handle_event({Name, Value}, #metric{name = Name1, type = meter} = State) when Name == Name1 ->
    folsom_metrics_meter:mark(Name, Value),
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
handle_call(info, #metric{name = Name, type = Type, tags = Tags} = State) ->
    {ok, [{Name, [{tags, Tags}, {type, Type}]}], State};
%% Counter
handle_call({counter, Name}, State) ->
    Values = folsom_metrics_counter:get_value(Name),
    {ok, Values, State};
%% Gauge
handle_call({gauge, Name}, State) ->
    Values = folsom_metrics_gauge:get_value(Name),
    {ok, Values, State};
%% Histogram
handle_call({histogram_statistics, Name}, State) ->
    Values = folsom_metrics_histogram:get_values(Name),
    Stats = folsom_statistics:get_statistics(Values),
    {ok, Stats, State};
handle_call({histogram, Name}, State) ->
    Values = folsom_metrics_histogram:get_values(Name),
    {ok, Values, State};
%% History
handle_call({history, {Name, Count}}, State) ->
    Values = folsom_metrics_history:get_events(Name, Count),
    {ok, Values, State};
handle_call({history, Name}, State) ->
    Values = folsom_metrics_history:get_events(Name),
    {ok, Values, State};
%% Meter
handle_call({meter, Name}, State) ->
    Values = folsom_metrics_meter:get_value(Name),
    {ok, Values, State};
handle_call({meter_mean_rate, Name}, State) ->
    Values = folsom_metrics_meter:mean_rate(Name),
    {ok, Values, State};
handle_call({meter_one_rate, Name}, State) ->
    Values = folsom_metrics_meter:one_minute_rate(Name),
    {ok, Values, State};
handle_call({meter_five_rate, Name}, State) ->
    Values = folsom_metrics_meter:five_minute_rate(Name),
    {ok, Values, State};
handle_call({meter_fifteen_rate, Name}, State) ->
    Values = folsom_metrics_meter:fifteen_minute_rate(Name),
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
