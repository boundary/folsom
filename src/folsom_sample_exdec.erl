%%%-------------------------------------------------------------------
%%% @author Joe <joe@der-dieb.local>
%%% @copyright (C) 2012, Joe
%%% @doc
%%%
%%% @end
%%% Created : 26 Apr 2012 by Joe <joe@der-dieb.local>
%%%-------------------------------------------------------------------
-module(folsom_sample_exdec).

-behaviour(gen_server).

%% API
-export([start_link/3]).

-export([
         update/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(HOURSECS, 3600).

-define(ETSOPTS, [ordered_set, {write_concurrency, true}, protected, named_table]).

-include("folsom.hrl").

-record(state, {metric, sample}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Size, Alpha) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Size, Alpha], []).

update(Name, Value) ->
    Timestamp = folsom_utils:now_epoch(),

    % immediately see if we need to rescale
    ok = gen_server:call(Name, {rescale, {Timestamp}}),

    % now lets update the sample if the new value is worthy
    ok = gen_server:call(Name, {update, {Value, Timestamp}}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name, Size, Alpha]) ->
    Now = folsom_utils:now_epoch(),
    ETSTable = ets:new(Name , ?ETSOPTS),
    Sample = #exdec{start = Now, next = Now + ?HOURSECS, alpha = Alpha, size = Size, reservoir = ETSTable},
    {ok, #state{metric = Name, sample = Sample}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({update, {Value, Timestamp}}, _From, #state{sample = Sample} = State) ->
    NewSample = update(Sample, Value, Timestamp),
    {reply, ok, State#state{sample = NewSample}};
handle_call({rescale, {Timestamp}}, _From, #state{sample = #exdec{
                                                    reservoir = Reservoir,
                                                    alpha = Alpha,
                                                    start = Start,
                                                    next = Next
                                                   } = Sample
                                                 } = State) ->
    {NewRes, NewStart, NewNext} = rescale(Reservoir, Timestamp, Next, Start, Alpha),
    {reply, ok, State#state{sample =
                                Sample#exdec{
                                  reservoir = NewRes,
                                  start = NewStart,
                                  next = NewNext,
                                  n = folsom_utils:get_ets_size(NewRes)
                                 }
                           }};
handle_call(dump, _From, State) ->
    {reply, State, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
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

update(#exdec{reservoir = Reservoir, alpha = Alpha, start = Start, n = N, size = Size, seed = Seed} = Sample, Value, Timestamp) when N =< Size ->
    % since N is =< Size we can just add the new value to the sample

    {Rand, New_seed} = random:uniform_s(N + 1, Seed),
    Priority = priority(Alpha, Timestamp, Start, Rand),
    true = ets:insert(Reservoir, {Priority, Value}),

    Sample#exdec{n = folsom_utils:get_ets_size(Reservoir), seed = New_seed};
update(#exdec{reservoir = Reservoir, alpha = Alpha, start = Start, n = N, seed = Seed} = Sample, Value, Timestamp) ->
    % when N is not =< Size we need to check to see if the priority of
    % the new value is greater than the first (smallest) existing priority

    {Rand, NewSeed} = random:uniform_s(N, Seed),
    Priority = priority(Alpha, Timestamp, Start, Rand),
    First = ets:first(Reservoir),

    update_on_priority(Sample, First, Priority, NewSeed, Value).

update_on_priority(#exdec{reservoir = Reservoir} = Sample, First, Priority, NewSeed, Value) when First < Priority ->
    true = case ets:insert_new(Reservoir, {Priority, Value}) of
        true ->
            % priority didnt already exist, so we created it and need to delete the first one
            ets:delete(Reservoir, First);
        false ->
            % priority existed, we dont need to do anything
            true
    end,
    Sample#exdec{n = folsom_utils:get_ets_size(Reservoir), seed = NewSeed};
update_on_priority(#exdec{reservoir = Reservoir} = Sample, _, _, _, _) ->
    Sample#exdec{n = folsom_utils:get_ets_size(Reservoir)}.

% gaurd against a possible bug, T should always be =< ?HOURSECS
% also to prevent overflow issues make sure alpha is always =<
% math:log(1.79769313486231570815e+308) / 3599 = 0.19721664709457737
weight(Alpha, T) when T =< ?HOURSECS, Alpha =< 0.19721664709457737 ->
    math:exp(Alpha * T).

priority(Alpha, Time, Start, Rand) ->
    weight(Alpha, Time - Start) / Rand.

rescale(Reservoir, Now, Next, OldStart, Alpha) when Now >= Next ->
    NewStart = Now + ?HOURSECS,
    NewRes = delete_and_rescale(Reservoir, NewStart, OldStart, Alpha),
    {NewRes, Now, NewStart};
rescale(Reservoir, _, Next, Start, _) ->
    {Reservoir, Start, Next}.

delete_and_rescale(Reservoir, NewStart, OldStart, Alpha) ->
    % get the existing reservoir
    ResList = ets:tab2list(Reservoir),

    % delete the old ets table
    true = ets:delete(Reservoir),

    % create a new ets table to use
    NewRes = ets:new(Reservoir, ?ETSOPTS),

    % populate it with new priorities and the existing values
    [true = ets:insert(Reservoir, {recalc_priority(Priority, Alpha, NewStart, OldStart) ,Value}) || {Priority, Value} <- ResList],

    % return the new ets table
    NewRes.

recalc_priority(Priority, Alpha, Start, OldStart) ->
    Priority * math:exp(-Alpha * (Start - OldStart)).
