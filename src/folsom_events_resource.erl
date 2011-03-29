%%%-------------------------------------------------------------------
%%% File:      folsom_metrics_resource.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% http end point that produces metrics collected from event handlers
%%% @end
%%%------------------------------------------------------------------

-module(folsom_events_resource).

-export([init/1,
         content_types_provided/2,
         content_types_accepted/2,
         to_json/2,
         from_json/2,
         allowed_methods/2,
         resource_exists/2,
         delete_resource/2]).

-include("folsom.hrl").
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
    folsom_events_event:delete_handler(list_to_atom(Id)),
    {true, ReqData, Context}.

to_json(ReqData, Context) ->
    Result = get_request(wrq:path_info(id, ReqData),
                         wrq:get_qs_value("limit", integer_to_list(?DEFAULT_LIMIT), ReqData),
                         wrq:get_qs_value("tag", undefined, ReqData),
                         wrq:get_qs_value("info", undefined, ReqData)),
    {mochijson2:encode(Result), ReqData, Context}.

from_json(ReqData, Context) ->
    {struct, Body} = mochijson2:decode(wrq:req_body(ReqData)),
    Result = put_request(wrq:path_info(id, ReqData), Body),
    {mochijson2:encode(Result), ReqData, Context}.


% internal fuctions


resource_exists(undefined, ReqData, Context) ->
    {true, ReqData, Context};
resource_exists(Id, ReqData, Context) ->
    {folsom_events_event:handler_exists(list_to_atom(Id)), ReqData, Context}.

get_request(undefined, _, undefined, undefined) ->
    folsom_events_event:get_handlers();
get_request(undefined, _, undefined, "true") ->
    folsom_events_event:get_handlers_info();
get_request(undefined, _, Tag, _) ->
    folsom_events_event:get_tagged_handlers(list_to_atom(Tag));
get_request(Id, Count, undefined, _) ->
    folsom_events_event:get_events(list_to_atom(Id), list_to_integer(Count));
get_request(Id, Count, Tag, _) ->
    folsom_events_event:get_events(list_to_atom(Id), list_to_atom(Tag), list_to_integer(Count)).

put_request(undefined, Body) ->
    Id = list_to_atom(binary_to_list(proplists:get_value(<<"id">>, Body))),
    Tags = proplists:get_value(<<"tags">>, Body),
    AtomTags = [list_to_atom(binary_to_list(Tag)) || Tag <- Tags],
    Size = proplists:get_value(<<"size">>, Body),
    add_handler(Id, AtomTags, Size);
put_request(Id, Body) ->
    Event = proplists:get_value(<<"event">>, Body),
    Tags = proplists:get_value(<<"tags">>, Body),
    AtomTags = [list_to_atom(binary_to_list(Tag)) || Tag <- Tags],
    folsom_events_event:notify({list_to_atom(Id), AtomTags, Event}).

add_handler(Id, Tags, Size) ->
    folsom_events_event:add_handler(Id, Tags, Size).


