%%%-------------------------------------------------------------------
%%% @author joe williams <j@fastip.com>
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by joe williams <j@fastip.com>
%%%-------------------------------------------------------------------
-module(emetrics_event).

-behaviour(gen_event).

%% API
-export([add_handler/3,
         add_handler/4,
         delete_handler/1,
         notify/1,
         get_values/1,
         get_info/1,
         get_max/1,
         get_min/1,
         get_histogram/1,
         get_histogram/2,
         get_variance/1,
         get_mean/1,
         get_median/1,
         get_percentile/2,
         get_all/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(metric, {
          id,
          type = uniform,
          sample
         }).

-define(HIST, [10, 20, 30, 50, 100, 200, 300, 400, 500, 1000, 99999999999999]).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

add_handler(Id, Type, Size) ->
    gen_event:add_sup_handler(emetrics_event_manager,
                              {emetrics_event, Id}, [Id, Type, Size]).

add_handler(Id, Type, Size, Alpha) ->
    gen_event:add_sup_handler(emetrics_event_manager,
                              {emetrics_event, Id}, [Id, Type, Size, Alpha]).

delete_handler(Id) ->
    gen_event:delete_handler(emetrics_event_manager, {emetrics_event, Id}, nil).

notify(Event) ->
    gen_event:notify(emetrics_event_manager, Event).

get_values(Id) ->
    gen_event:call(emetrics_event_manager, {emetrics_event,Id}, values).

get_info(Id) ->
    gen_event:call(emetrics_event_manager, {emetrics_event, Id}, info).

get_max(Id) when is_atom(Id) ->
    get_max(get_values(Id));
get_max([]) ->
    0;
get_max(Values) ->
    [Head | _] = lists:reverse(lists:sort(Values)),
    Head.

get_min(Id) when is_atom(Id)->
    get_min(get_values(Id));
get_min([]) ->
    0;
get_min(Values) ->
    [Head | _] = lists:sort(Values),
    Head.

get_histogram(Id) ->
    get_histogram(Id, ?HIST).

get_histogram(Id, Hist) ->
    Bins = [{Bin, 0} || Bin <- Hist],
    Values = get_values(Id),
    build_hist(Values, Bins).

get_variance(Id) ->
    get_variance(Id, get_values(Id)).

get_variance(_, []) ->
    0;
get_variance(Id, Values) ->
    Mean = get_mean(Id),
    variance(Mean, Values, 0) / length(Values).

get_mean(Id) when is_atom(Id) ->
    get_mean(get_values(Id));
get_mean([]) ->
    0;
get_mean(Values) ->
    Sum = lists:sum(Values),
    Sum / length(Values).

get_median(Id) ->
    get_percentile(Id, 0.5).

get_percentile(Id, Percentile) when is_atom(Id) ->
    get_percentile(lists:sort(get_values(Id)), Percentile);
get_percentile([], _) ->
    0;
get_percentile(Values, Percentile) ->
    Element = round(Percentile * length(Values)),
    lists:nth(Element, Values).

get_all(Id) ->
    {Id, Type} = get_info(Id),
    [
     {id, Id},
     {type, Type},
     {min, get_min(Id)},
     {max, get_max(Id)},
     {mean, get_mean(Id)},
     {median, get_median(Id)},
     {variance, get_variance(Id)},
     {percentile,
      [
      {75, get_percentile(Id, 0.75)},
      {99, get_percentile(Id, 0.99)}
      ]
     },
     {histogram, get_histogram(Id)}
     ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([Id, Type, Size]) ->
    Sample = emetrics_uniform:new(Size),
    {ok, #metric{id = Id, type = Type, sample = Sample}};
init([Id, Type, Size, Alpha]) ->
    Sample = emtrics_exdec:new(Alpha, Size),
    {ok, #metric{id = Id, type = Type, sample = Sample}}.


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
    NewSample = emetrics_uniform:update(Sample, Value),
    {ok, State#metric{
           sample = NewSample}};
handle_event({Id, Value}, #metric{id = Id1, type = exdec, sample = Sample} = State) when Id == Id1->
    NewSample = emetrics_exdec:update(Sample, Value),
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
handle_call(info, #metric{id = Id, type = Type} = State) ->
    {ok, {Id, Type}, State};
handle_call(values, #metric{type = uniform, sample = Sample} = State) ->
    Values = emetrics_uniform:get_values(Sample),
    {ok, Values, State};
handle_call(values, #metric{type = exdec, sample = Sample} = State) ->
    Values = emetrics_exdec:get_values(Sample),
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

variance(Mean, [Head | Tail], Acc) ->
    V = Acc + math:pow(Head - Mean, 2),
    variance(Mean, Tail, V);
variance(_, [], Acc) ->
    Acc.

% all this is too complicated, find better solution
build_hist([Head | Tail], Hist) ->
    {Bin, Count} = proplists:lookup(which_bin(Head, Hist, []), Hist),
    List = proplists:delete(Bin, Hist),
    NewHist = lists:append(List, [{Bin, Count + 1}]),
    build_hist(Tail, NewHist);
build_hist([], Hist) ->
    lists:sort(Hist).

which_bin(Value, [{Bin, _} = B | Tail], Acc) when Value =< Bin ->
    which_bin(Value, Tail, lists:sort(lists:append(Acc, [B])));
which_bin(Value, [_ | Tail], Acc) ->
    which_bin(Value, Tail, Acc);
which_bin(_, [], [{Bin, _} | _]) ->
    Bin.
