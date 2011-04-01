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
