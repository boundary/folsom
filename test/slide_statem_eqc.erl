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
%%% File:      slide_statem_eqc.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc       quickcheck test for the folsom_sample_slide.erl
%%% @end
%%%------------------------------------------------------------------

-module(slide_statem_eqc).

-compile(export_all).

-ifdef(TEST).
-ifdef(EQC).

-include("folsom.hrl").

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eunit/include/eunit.hrl").



-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) ->
                              io:format(user, Str, Args) end, P)).

-define(WINDOW, 60).

-record(state, {moment=1000,
                sample,
                name,
                values=[]}).

initial_state() ->
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
next_state(#state{moment=Moment, values=Values0}=S, V, {call, ?MODULE, update, [_, _Val]}) ->
    S#state{values=Values0 ++ [{Moment, V}]};
next_state(#state{values=Values, moment=Moment}=S, _V, {call, ?MODULE, trim, _}) ->
    %% trim the model
    S#state{values = trim(Values, Moment, ?WINDOW)};
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
    Values = [V || {K, V} <- Values0, K >= Moment - ?WINDOW],
    case lists:sort(Values) == lists:sort(Res) of
        true ->
            true;
        _ ->
            {"get values", {"model", lists:sort(Values)},
             {"smaple", lists:sort(Res)}}
    end;
postcondition(#state{values=Values, sample=Sample, moment=Moment}, {call, ?MODULE, trim, _}, _TrimCnt) ->
    %% check that values and the actual table contents are the same after a trim
    Table0 = ets:tab2list(Sample#slide.reservoir),
    Table = [{X, Y} || {{X, _}, Y} <- Table0],
    Model = lists:sort(trim(Values, Moment, ?WINDOW)),
    case Model == lists:sort(Table) of
        true ->
            true;
        _ ->
            {"after trim", {"model", Model}, {"sample", lists:sort(Table)}}
    end;
postcondition(_S, {call, ?MODULE, _, _}, _Res) ->
    true.

prop_window_test_() ->
    Seconds = 10,
    {setup,
     fun() -> ok end,
     fun(_X) -> (catch meck:unload(folsom_utils)), folsom:stop() end,
     [{"QuickCheck Test",
       {timeout, Seconds*2, fun() -> true = eqc:quickcheck(eqc:testing_time(Seconds, ?QC_OUT(prop_window()))) end
       }}]}.

prop_window() ->
    folsom:start(),
    (catch meck:new(folsom_utils)),
    ?FORALL(Cmds, commands(?MODULE),
            aggregate(command_names(Cmds),
            begin
                {H, S, Res} = run_commands(?MODULE, Cmds),
                {Actual, Expected} = case S#state.sample of
                                         undefined ->
                                             {S#state.values, []};
                                         Sample ->
                                             A = folsom_metrics:get_metric_value(S#state.name),
                                             E = [V || {K, V} <- S#state.values, K >= S#state.moment - ?WINDOW],
                                             folsom_metrics:delete_metric(S#state.name),
                                             {A, E}
                                    end,
                ?WHENFAIL(
                   io:format("History: ~p~nState: ~p~nActual: ~p~nExpected: ~p~nRes: ~p~n", [H, S, Actual, Expected, Res]),
                   conjunction([{total, equals(lists:sort(Actual), lists:sort(Expected))},
                               {eq, equals(Res, ok)}]))
            end)).

%% Commands
new_histo() ->
    Ref = make_ref(),
    folsom_metrics:new_histogram(Ref, slide, ?WINDOW),
    #histogram{sample=Slide} = folsom_metrics_histogram:get_value(Ref),
    ok = folsom_sample_slide_server:stop(Slide#slide.server),
    {Ref, Slide}.

tick(Moment) ->
    IncrBy = trunc(random:uniform(10)),
    meck:expect(folsom_utils, now_epoch, fun() -> Moment + IncrBy end),
    Moment+IncrBy.

update(Sample, Val) ->
    Sample = folsom_sample_slide:update(Sample, Val),
    Val.

trim(Sample, Window) ->
    folsom_sample_slide:trim(Sample#slide.reservoir, Window).

get_values(Sample) ->
    folsom_sample_slide:get_values(Sample).

%% private
trim(L, Moment, Window) ->
    [{K, V} || {K, V} <- L, K >= Moment - Window].

-endif.
-endif.
