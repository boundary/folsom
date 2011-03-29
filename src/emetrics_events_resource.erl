%%%-------------------------------------------------------------------
%%% File:      emetrics_metrics_resource.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% http end point that produces metrics collected from event handlers
%%% @end
%%%------------------------------------------------------------------

-module(emetrics_events_resource).

-export([init/1,
         content_types_provided/2,
         content_types_accepted/2,
         to_json/2,
         from_json/2,
         allowed_methods/2,
         resource_exists/2,
         delete_resource/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET', 'PUT', 'DELETE'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    resource_exists(wrq:path_info(id, ReqData), ReqData, Context).

delete_resource(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    emetrics_event_event:delete_handler(list_to_atom(Id)),
    {true, ReqData, Context}.

to_json(ReqData, Context) ->
    Result = get_request(wrq:path_info(id, ReqData), wrq:get_qs_value("count", "1", ReqData)),
    {mochijson2:encode(Result), ReqData, Context}.

from_json(ReqData, Context) ->
    {struct, Body} = mochijson2:decode(wrq:req_body(ReqData)),
    Result = put_request(wrq:path_info(id, ReqData), Body),
    {mochijson2:encode(Result), ReqData, Context}.


% internal fuctions


resource_exists(undefined, ReqData, Context) ->
    {true, ReqData, Context};
resource_exists(Id, ReqData, Context) ->
    {emetrics_event_event:handler_exists(list_to_atom(Id)), ReqData, Context}.

get_request(undefined, _) ->
    emetrics_event_event:get_handlers();
get_request(Id, Count) ->
    emetrics_event_event:get_events(list_to_atom(Id), list_to_integer(Count)).

put_request(undefined, Body) ->
    Id = list_to_atom(binary_to_list(proplists:get_value(<<"id">>, Body))),
    Tags = binary_to_list(proplists:get_value(<<"tags">>, Body)),
    Size = proplists:get_value(<<"size">>, Body),
    add_handler(Id, Tags, Size);
put_request(Id, Body) ->
    Event = proplists:get_value(<<"event">>, Body),
    emetrics_event_event:notify({list_to_atom(Id), Event}).

add_handler(Id, Tags, Size) ->
    emetrics_event_event:add_handler(Id, Tags, Size).
