%%%
%%% Copyright 2011, fast_ip
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
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% http helper functions
%%% @end
%%%------------------------------------------------------------------

-module(http_helpers).

-export([http_get/1, http_put/2, http_delete/1]).

-include_lib("eunit/include/eunit.hrl").
-define(CONTENTTYPE, {"Content-Type", "application/json"}).

% http helper functions

check_get_response_code({"200", _, Body}) ->
    Body.

check_put_response_code({"204", _, _}) ->
    ok.

check_delete_response_code({"204", _, _}) ->
    ok.

http_get(Url) ->
    ?debugFmt("~nGET ~p~n",[Url]),
    {ok, RC, ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [], get),
    ?debugFmt("~nBody: ~p~n",[ResponseBody]),
    check_get_response_code({RC, ResponseHeaders, ResponseBody}).

http_put(Url, RequestBody) ->
    ?debugFmt("~nPUT ~p~nBody: ~p~n",[Url, RequestBody]),
    {ok, RC, ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [?CONTENTTYPE], put, RequestBody),
    check_put_response_code({RC, ResponseHeaders, ResponseBody}).

http_delete(Url) ->
    ?debugFmt("~nDELETE ~p~n",[Url]),
    {ok, RC, ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [], delete),
    check_delete_response_code({RC, ResponseHeaders, ResponseBody}).
