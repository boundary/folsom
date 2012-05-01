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
%%% File:      folsom.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom).
-export([start/0, stop/0]).
-export([start/2, stop/1]).
-behaviour(application).
-define(APP, ?MODULE).

start() ->
    application:start(?APP).

stop() ->
    application:stop(?APP).

start(_Type, _Args) ->
    {ok, Pid} = folsom_sup:start_link(),
    lists:foreach(
      fun ({K, New}) ->
              case application:get_env(?APP, K) of
                  {ok, Name} when is_atom(Name) ->
                      New(Name);
                  {ok, Names} when is_list(Names) ->
                      lists:foreach(New, Names);
                  undefined ->
                      ok
              end
      end,
      [{counter, fun folsom_metrics:new_counter/1},
       {gauge, fun folsom_metrics:new_gauge/1},
       {histogram, fun folsom_metrics:new_histogram/1},
       {history, fun folsom_metrics:new_history/1},
       {meter, fun folsom_metrics:new_meter/1},
       {meter_reader, fun folsom_metrics:new_meter_reader/1}]),
    {ok, Pid}.

stop(_State) ->
    ok.
