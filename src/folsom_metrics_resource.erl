%%%-------------------------------------------------------------------
%%% File:      folsom_metrics_resource.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% http end point that produces metrics collected from event handlers
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_resource).

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
    folsom_metrics_event:delete_handler(list_to_atom(Id)),
    {true, ReqData, Context}.

to_json(ReqData, Context) ->
    Id1 = wrq:path_info(id, ReqData),
    Raw = wrq:get_qs_value("raw", "undefined", ReqData),
    Id2 = wrq:get_qs_value("covariance", "undefined", ReqData),
    Tag = wrq:get_qs_value("tag", "undefined", ReqData),
    Info = wrq:get_qs_value("info", "undefined", ReqData),
    Result = get_request(Id1, list_to_atom(Raw), list_to_atom(Id2), list_to_atom(Tag), list_to_atom(Info)),
    {mochijson2:encode(Result), ReqData, Context}.

from_json(ReqData, Context) ->
    {struct, Body} = mochijson2:decode(wrq:req_body(ReqData)),
    Result = put_request(wrq:path_info(id, ReqData), Body),
    {mochijson2:encode(Result), ReqData, Context}.


% internal fuctions


resource_exists(undefined, ReqData, Context) ->
    {true, ReqData, Context};
resource_exists(Id, ReqData, Context) ->
    {folsom_metrics_event:handler_exists(list_to_atom(Id)), ReqData, Context}.

get_request(undefined, undefined, undefined, undefined, undefined) ->
    folsom_metrics_event:get_handlers();
get_request(Id, true, undefined, undefined, undefined) ->
    folsom_metrics_event:get_values(list_to_atom(Id));
get_request(undefined, undefined, undefined, undefined, true) ->
    folsom_metrics_event:get_handlers_info();
get_request(undefined, undefined, undefined, Tag, undefined) ->
    folsom_metrics_event:get_tagged_handlers(Tag);
get_request(Id, undefined, undefined, undefined, undefined) ->
    folsom_metrics_event:get_statistics(list_to_atom(Id));
get_request(Id1, undefined, Id2, undefined, undefined) ->
    folsom_statistics:get_covariance(list_to_atom(Id1), Id2).

put_request(undefined, Body) ->
    Id = folsom_utils:binary_to_atom(proplists:get_value(<<"id">>, Body)),
    Type = folsom_utils:binary_to_atom(proplists:get_value(<<"type">>, Body, <<"uniform">>)),
    Size = proplists:get_value(<<"size">>, Body, ?DEFAULT_SIZE),
    Tags = proplists:get_value(<<"tags">>, Body, []),
    AtomTags = folsom_utils:convert_tags(Tags),
    add_handler(Type, Id, AtomTags, Size, Body);
put_request(Id, Body) ->
    Value = proplists:get_value(<<"value">>, Body),
    folsom_metrics_event:notify({list_to_atom(Id), Value}).

add_handler(exdec, Id, Tags, Size, Body) ->
    Alpha = proplists:get_value(<<"alpha">>, Body, 1),
    folsom_metrics_event:add_handler(Id, exdec, Tags, Size, Alpha);
add_handler(uniform, Id, Tags, Size, _) ->
    folsom_metrics_event:add_handler(Id, uniform, Tags, Size);
add_handler(none, Id, Tags, Size, _) ->
    folsom_metrics_event:add_handler(Id, none, Tags, Size);
add_handler(_, Id, Tags, Size, _) ->
    folsom_metrics_event:add_handler(Id, uniform, Tags, Size).
