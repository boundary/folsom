%%%-------------------------------------------------------------------
%%% File:      emetrics_metrics_resource.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% http end point that produces metrics collected from event handlers
%%% @end
%%%------------------------------------------------------------------

-module(emetrics_metrics_resource).

-export([init/1, content_types_provided/2, to_json/2, allowed_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

to_json(ReqData, Context) ->
    Result = case wrq:path_info(id, ReqData) of
                 undefined ->
                     get_handlers();
                 Id ->
                     get_metrics(list_to_atom(Id))
             end,
    {mochijson2:encode(Result), ReqData, Context}.

get_metrics(Id) ->
    emetrics_event:get_all(Id).

get_handlers() ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(emetrics_event_manager)),
    Handlers.
