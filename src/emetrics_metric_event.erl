%%%-------------------------------------------------------------------
%%% @author joe williams <j@fastip.com>
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by joe williams <j@fastip.com>
%%%-------------------------------------------------------------------
-module(emetrics_metric_event).

-behaviour(gen_event).

%% API
-export([add_handler/3,
         add_handler/4,
         add_sup_handler/3,
         add_sup_handler/4,
         delete_handler/1,
         handler_exists/1,
         notify/1,
         get_handlers/0,
         get_values/1,
         get_info/1,
         get_all/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(metric, {
          id,
          size,
          type = uniform,
          sample
         }).

%%%===================================================================
%%% API
%%%===================================================================

add_handler(Id, Type, Size) ->
    gen_event:add_handler(emetrics_event_manager,
                          {emetrics_event, Id}, [Id, Type, Size]).

add_handler(Id, Type, Size, Alpha) ->
    gen_event:add_handler(emetrics_event_manager,
                          {emetrics_event, Id}, [Id, Type, Size, Alpha]).

add_sup_handler(Id, Type, Size) ->
    gen_event:add_sup_handler(emetrics_event_manager,
                              {emetrics_event, Id}, [Id, Type, Size]).

add_sup_handler(Id, Type, Size, Alpha) ->
    gen_event:add_sup_handler(emetrics_event_manager,
                              {emetrics_event, Id}, [Id, Type, Size, Alpha]).

delete_handler(Id) ->
    gen_event:delete_handler(emetrics_event_manager, {emetrics_event, Id}, nil).

handler_exists(Id) ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(emetrics_event_manager)),
    lists:member(Id, Handlers).

notify(Event) ->
    gen_event:notify(emetrics_event_manager, Event).

get_handlers() ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(emetrics_event_manager)),
    Handlers.

get_values(Id) ->
    gen_event:call(emetrics_event_manager, {emetrics_event,Id}, values).

get_info(Id) ->
    gen_event:call(emetrics_event_manager, {emetrics_event, Id}, info).

get_all(Id) ->
    {Id, Type, Size} = get_info(Id),
    [
     {id, Id},
     {type, Type},
     {size, Size},
     {min, emetrics_statistics:get_min(Id)},
     {max, emetrics_statistics:get_max(Id)},
     {mean, emetrics_statistics:get_mean(Id)},
     {median, emetrics_statistics:get_median(Id)},
     {variance, emetrics_statistics:get_variance(Id)},
     {standard_deviation, emetrics_statistics:get_standard_deviation(Id)},
     {skewness, emetrics_statistics:get_skewness(Id)},
     {kurtosis, emetrics_statistics:get_kurtosis(Id)},
     {percentile,
      [
       {75, emetrics_statistics:get_percentile(Id, 0.75)},
       {95, emetrics_statistics:get_percentile(Id, 0.95)},
       {99, emetrics_statistics:get_percentile(Id, 0.99)},
       {999, emetrics_statistics:get_percentile(Id, 0.999)}
      ]
     },
     {histogram, emetrics_statistics:get_histogram(Id)}
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
init([Id, uniform, Size]) ->
    Sample = emetrics_sample_uniform:new(Size),
    {ok, #metric{id = Id, type = uniform, size = Size, sample = Sample}};
init([Id, none, Size]) ->
    Sample = emetrics_sample_none:new(Size),
    {ok, #metric{id = Id, type = none, size = Size, sample = Sample}};
init([Id, exdec, Size, Alpha]) ->
    Sample = emetrics_exdec:new(Alpha, Size),
    {ok, #metric{id = Id, type = exdec, size = Size, sample = Sample}}.


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
handle_event({Id, Value}, #metric{id = Id1, type = uniform, sample = Sample} = State) when Id == Id1 ->
    NewSample = emetrics_sample_uniform:update(Sample, Value),
    {ok, State#metric{
           sample = NewSample}};
handle_event({Id, Value}, #metric{id = Id1, type = exdec, sample = Sample} = State) when Id == Id1->
    NewSample = emetrics_sample_exdec:update(Sample, Value),
    {ok, State#metric{
           sample = NewSample}};
handle_event({Id, Value}, #metric{id = Id1, type = none, sample = Sample} = State) when Id == Id1->
    NewSample = emetrics_sample_none:update(Sample, Value),
    {ok, State#metric{
           sample = NewSample}};
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
handle_call(info, #metric{id = Id, type = Type, size = Size} = State) ->
    {ok, {Id, Type, Size}, State};
handle_call(values, #metric{type = uniform, sample = Sample} = State) ->
    Values = emetrics_sample_uniform:get_values(Sample),
    {ok, Values, State};
handle_call(values, #metric{type = exdec, sample = Sample} = State) ->
    Values = emetrics_sample_exdec:get_values(Sample),
    {ok, Values, State};
handle_call(values, #metric{type = none, sample = Sample} = State) ->
    Values = emetrics_sample_none:get_values(Sample),
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
