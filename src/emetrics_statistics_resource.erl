%%%-------------------------------------------------------------------
%%% File:      emetrics_statistics_resource.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% http end point that converts erlang:statistics/1 to json
%%% @end
%%%------------------------------------------------------------------

-module(emetrics_statistics_resource).

-export([init/1, content_types_provided/2, to_json/2, allowed_methods/2]).

-include("emetrics.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

to_json(ReqData, Context) ->
    {mochijson2:encode(get_statistics()), ReqData, Context}.

get_statistics() ->
    [{Key, convert_statistics({Key, erlang:statistics(Key)})} || Key <- ?STATISTICS].

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
