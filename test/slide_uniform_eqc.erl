%%%
%%% Copyright 2012 - Basho Technologies, Inc. All Rights Reserved.
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
%%% File:      slide_uniform_eqc.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc       quickcheck test for the folsom_sample_slide.erl
%%% @end
%%%------------------------------------------------------------------

-module(slide_uniform_eqc).

-compile(export_all).

-ifdef(TEST).
-ifdef(EQC).
-include("folsom.hrl").

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NUMTESTS, 200).
-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) ->
                              io:format(user, Str, Args) end, P)).

-define(WINDOW, 60).
-define(SIZE, 5).

-record(state, {moment=1000,
                sample,
                name,
                count=orddict:new(),
                values=[]}).

initial_state() ->
    meck:expect(folsom_utils, now_epoch, fun(_Now) -> 1000 end),
    meck:expect(folsom_utils, now_epoch, fun() -> 1000 end),
    #state{}.

command(S) ->
    oneof(
      [{call, ?MODULE, new_histo, []} || S#state.sample == undefined] ++
          [{call, ?MODULE, tick, [S#state.moment]} || S#state.sample /= undefined] ++
          [{call, ?MODULE, update, [S#state.sample, int()]} || S#state.sample /= undefined] ++
          [{call, ?MODULE, trim, [S#state.sample, ?WINDOW]} || S#state.sample /= undefined] ++
          [{call, ?MODULE, get_values, [S#state.sample]} || S#state.sample /= undefined]
     ).

%% Next state transformation, S is the current state
next_state(S, V, {call, ?MODULE, new_histo, []}) ->
    S#state{name={call, erlang, element, [1, V]}, sample={call, erlang, element, [2, V]}};
next_state(S, V, {call, ?MODULE, tick, [_Moment]}) ->
    S#state{moment=V};
next_state(#state{moment=Moment, values=Values0, sample=Sample, count=Count}=S, NewSample, {call, ?MODULE, update, [_, Val]}) ->
    S#state{values={call, slide_uniform_eqc, new_state_values, [Sample, Moment, Values0, Val, Count]},
            count={call, orddict, update_counter, [Moment, 1, Count]},
            sample=NewSample};
next_state(#state{values=Values, moment=Moment}=S, _V, {call, ?MODULE, trim, _}) ->
    %% trim the model
    S#state{values={call, ?MODULE, trim, [Values, Moment, ?WINDOW]}};
next_state(S,_V,{call, ?MODULE, _, _}) ->
    S.

%% Precondition, checked before command is added to the command sequence
precondition(S, {call, _, new_histo, _}) ->
  S#state.sample == undefined;
precondition(S, _) when S#state.sample == undefined ->
    false;
precondition(_S, {call, _, _, _}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
postcondition(#state{values=Values0, moment=Moment}, {call, ?MODULE, get_values, _}, Res) ->
    Values = [V || {{M, _C}, V} <- Values0, M >= Moment - ?WINDOW],
    case lists:sort(Values) == lists:sort(Res) of
        true ->
            true;
       _ ->
            {"get values", {"model", lists:sort(Values)}, {"sample", lists:sort(Res)}}
    end;
postcondition(#state{values=Values, sample=Sample, moment=Moment}, {call, ?MODULE, trim, _}, _TrimCnt) ->
    %% check that values and the actual table contents are the same after a trim
    Table = [ Elem || {K, _V}=Elem <- ets:tab2list(Sample#slide_uniform.reservoir)
            , is_tuple(K)], %% filter out counter,
    case lists:sort(trim(Values, Moment, ?WINDOW)) == lists:sort(Table) of
        true ->
            true;
        _ ->
            {"after trim", {"model", lists:sort(trim(Values, Moment, ?WINDOW))}, {"sample", lists:sort(Table)}}
    end;
postcondition(_S, {call, ?MODULE, _, _}, _Res) ->
    true.

prop_window_test_() ->
    {setup, fun() -> ok end, fun(_X) -> (catch meck:unload(folsom_utils)), folsom:stop() end,
     fun(_X) ->
                {timeout, 30,
                           ?_assert(eqc:quickcheck(eqc:numtests(?NUMTESTS, ?QC_OUT(prop_window()))))} end}.

prop_window() ->
    folsom:start(),
    (catch meck:new(folsom_utils)),
    (catch meck:expect(folsom_utils, update_counter, fun(Tid, Key, Value) -> meck:passthrough([Tid, Key, Value]) end)),
    (catch meck:expect(folsom_utils, timestamp, fun() -> Res = os:timestamp(), put(timestamp, Res), Res end)),
    ?FORALL(Cmds, commands(?MODULE),
            aggregate(command_names(Cmds),
                      begin
                          {H, S, Res} = run_commands(?MODULE, Cmds),
                          {Actual, Expected, Tab} = case S#state.sample of
                                                   undefined ->
                                                       {S#state.values, [], []};
                                                   Sample ->
                                                       A = folsom_metrics:get_metric_value(S#state.name),
                                                       E = [V || {{M, _C}, V} <- S#state.values, M >= S#state.moment - ?WINDOW],
                                                            T = ets:tab2list(Sample#slide_uniform.reservoir),
                                                            folsom_metrics:delete_metric(S#state.name),
                                                       {A, E, T}
                                    end,
                          ?WHENFAIL(
                             io:format("History: ~p~nState: ~p~nActual: ~p~nExpected: ~p~nRes: ~p~nTab: ~p~n",
                                       [H, S, Actual, Expected, Res, Tab]),
                             conjunction([{total, equals(lists:sort(Actual), lists:sort(Expected))},
                                          {eq, equals(Res, ok)}]))
                      end)).

%% Commands
new_histo() ->
    Ref = make_ref(),
    folsom_metrics:new_histogram(Ref, slide_uniform, {?WINDOW, ?SIZE}),
    #histogram{sample=Slide} = folsom_metrics_histogram:get_value(Ref),
    ok = folsom_sample_slide_server:stop(Slide#slide_uniform.server),
    {Ref, Slide}.

tick(Moment) ->
    IncrBy = trunc(random:uniform(10)),
    meck:expect(folsom_utils, now_epoch, fun() -> Moment + IncrBy end),
    meck:expect(folsom_utils, now_epoch, fun(_Now) -> Moment + IncrBy end),
    Moment+IncrBy.

update(Sample, Val) ->
    folsom_sample_slide_uniform:update(Sample, Val).

trim(Sample, Window) ->
    folsom_sample_slide_uniform:trim(Sample#slide_uniform.reservoir, Window).

get_values(Sample) ->
    folsom_sample_slide_uniform:get_values(Sample).

%% private
trim(L, Moment, Window) ->
    [{K, V} || {{M, _C}=K, V} <- L, M >= Moment - Window].

new_state_values(_Sample, Moment, Values, Val, Count) ->
    %Cnt = length([true || {{M, _C}, _V} <- Values, M == Moment]),
    Cnt =
    case orddict:find(Moment, Count) of
        error ->
            1;
        {ok, V} ->
            V+1
    end,
    case Cnt > ?SIZE of
        true ->
            %% replace
            {Rnd, _} = random:uniform_s(Cnt, get(timestamp)),
            case Rnd =< ?SIZE of
                true ->
                    lists:keyreplace({Moment, Rnd}, 1, Values, {{Moment, Rnd}, Val});
                false ->
                    Values
            end;
        false ->
            %% insert
            Values ++ [{{Moment, Cnt}, Val}]
    end.

-endif.
-endif.
