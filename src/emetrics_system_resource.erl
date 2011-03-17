-module(emetrics_system_resource).

-export([init/1, content_types_provided/2, to_json/2, allowed_methods/2]).

-include("emetrics.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

to_json(ReqData, Context) ->
    {mochijson2:encode(get_system_info()), ReqData, Context}.

get_system_info() ->
    [{Key, convert_system_info({Key, erlang:system_info(Key)})} || Key <- ?SYSTEM_INFO].

convert_system_info({driver_version, Value}) ->
    list_to_binary(Value);
convert_system_info({machine, Value}) ->
    list_to_binary(Value);
convert_system_info({otp_release, Value}) ->
    list_to_binary(Value);
convert_system_info({system_version, Value}) ->
    list_to_binary(Value);
convert_system_info({system_architecture, Value}) ->
    list_to_binary(Value);
convert_system_info({version, Value}) ->
    list_to_binary(Value);
convert_system_info({_, Value}) ->
    Value.
