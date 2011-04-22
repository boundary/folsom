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
%%% File:      http_helpers.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% http helper functions
%%% @end
%%%------------------------------------------------------------------

-module(http_helpers).

-export([http_get/1]).

-include_lib("eunit/include/eunit.hrl").
-define(CONTENTTYPE, {"Content-Type", "application/json"}).

% http helper functions

check_get_response_code({"200", _, Body}) ->
    Body.

http_get(Url) ->
    {ok, RC, ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [], get),
    check_get_response_code({RC, ResponseHeaders, ResponseBody}).
