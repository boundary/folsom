%%%
%%% Copyright 2011, Boundary
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
%%% File:      folsom_metrics_resource.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% http end point that produces metrics collected from event handlers
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_resource).

-export([init/1,
         content_types_provided/2,
         to_json/2,
         allowed_methods/2,
         resource_exists/2
         ]).

-include("folsom.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    resource_exists(wrq:path_info(id, ReqData), ReqData, Context).

to_json(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    Result = get_request(Id),
    {mochijson2:encode(Result), ReqData, Context}.

% internal fuctions

resource_exists(undefined, ReqData, Context) ->
    {true, ReqData, Context};
resource_exists(Id, ReqData, Context) ->
    {folsom_metrics:metric_exists(list_to_atom(Id)), ReqData, Context}.

get_request(undefined) ->
    folsom_metrics:get_metrics();
get_request(Id) ->
    folsom_metrics:get_metric_value(list_to_atom(Id)).
