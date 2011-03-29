%%%-------------------------------------------------------------------
%%% File:      folsom_vm_metrics.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% convert erlang system metrics to proplists
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_vm_metrics).

-export([get_system_info/0, get_statistics/0, get_memory/0]).

-include("folsom.hrl").


% api

get_memory() ->
    erlang:memory().

get_statistics() ->
    [{Key, convert_statistics({Key, erlang:statistics(Key)})} || Key <- ?STATISTICS].

get_system_info() ->
    [{Key, convert_system_info({Key, erlang:system_info(Key)})} || Key <- ?SYSTEM_INFO].


% internal functions

%% conversion functions for erlang:statistics(Key)

convert_statistics({context_switches, {ContextSwitches, 0}}) ->
    ContextSwitches;
convert_statistics({exact_reductions, {TotalExactReductions,
                                        ExactReductionsSinceLastCall}}) ->
    [{"total_exact_reductions", TotalExactReductions},
     {"exact_reductions_since_last_call", ExactReductionsSinceLastCall}];
convert_statistics({garbage_collection, {NumberofGCs, WordsReclaimed, 0}}) ->
    [{"number_of_gcs", NumberofGCs}, {"words_reclaimed", WordsReclaimed}];
convert_statistics({io, {Input, Output}}) ->
    [Input, Output];
convert_statistics({reductions, {TotalReductions, ReductionsSinceLastCall}}) ->
    [{"total_reductions", TotalReductions},
     {"reductions_since_last_call", ReductionsSinceLastCall}];
convert_statistics({runtime, {TotalRunTime, TimeSinceLastCall}}) ->
    [{"total_run_time", TotalRunTime}, {"time_since_last_call", TimeSinceLastCall}];
convert_statistics({wall_clock, {TotalWallclockTime, WallclockTimeSinceLastCall}}) ->
    [{"total_wall_clock_time", TotalWallclockTime},
     {"wall_clock_time_since_last_call", WallclockTimeSinceLastCall}];
convert_statistics({_, Value}) ->
    Value.

%% conversion functions for erlang:system_info(Key)

convert_system_info({allocated_areas, List}) ->
    [convert_allocated_areas(Value) || Value <- List];
convert_system_info({allocator, {_,_,_,List}}) ->
    List;
convert_system_info({c_compiler_used, {Compiler, Version}}) ->
    [{compiler, Compiler}, {version, convert_c_compiler_version(Version)}];
convert_system_info({cpu_topology, [{processor, List}]}) ->
    [{processor, convert_cpu_topology(List, [])}];
convert_system_info({dist_ctrl, List}) ->
    [{Value1, list_to_binary(io_lib:format("~p", [Value2]))} || {Value1, Value2} <- List];
convert_system_info({driver_version, Value}) ->
    list_to_binary(Value);
convert_system_info({machine, Value}) ->
    list_to_binary(Value);
convert_system_info({otp_release, Value}) ->
    list_to_binary(Value);
convert_system_info({scheduler_bindings, Value}) ->
    tuple_to_list(Value);
convert_system_info({system_version, Value}) ->
    list_to_binary(Value);
convert_system_info({system_architecture, Value}) ->
    list_to_binary(Value);
convert_system_info({version, Value}) ->
    list_to_binary(Value);
convert_system_info({_, Value}) ->
    Value.

convert_allocated_areas({Key, Value1, Value2}) ->
    {Key, [Value1, Value2]};
convert_allocated_areas({Key, Value}) ->
    {Key, Value}.

convert_c_compiler_version({A, B, C}) ->
    list_to_binary(io_lib:format("~p.~p.~p", [A, B, C]));
convert_c_compiler_version({A, B}) ->
    list_to_binary(io_lib:format("~p.~p", [A, B])).

convert_cpu_topology([{core, Value}| Tail], Acc) when is_tuple(Value) ->
  convert_cpu_topology(Tail, lists:append(Acc, [{core, tuple_to_list(Value)}]));
convert_cpu_topology([{core, Value}| Tail], Acc) when is_list(Value) ->
  convert_cpu_topology(Tail, lists:append(Acc, [{core, convert_cpu_topology(Value, [])}]));
convert_cpu_topology([{thread, Value}| Tail], Acc) ->
  convert_cpu_topology(Tail, lists:append(Acc, [{thread, tuple_to_list(Value)}]));
convert_cpu_topology([], Acc) ->
  Acc.
