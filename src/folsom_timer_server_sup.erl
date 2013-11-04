%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011, Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% File:      folsom_timer_server_sup.erl
%%% @author    Russell Brown <russelldb@basho.com>,
%%%            Andrey Vasenin <vasenin@aboutecho.com>
%%% @doc
%%% Supervisor for folsom's timers. It starts simple_one_to_one timer's
%%% child specs
%%% @end
%%%-----------------------------------------------------------------
-module(folsom_timer_server_sup).
-behaviour(supervisor).

-export([start_link/0, init/1 ]).

-export([start_timer/4, stop_timer/1]).

start_link () ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

start_timer(Time, Module, Function, Args) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [Time, Module, Function, Args]),
    Pid.

stop_timer(Pid) ->
    folsom_timer_server:stop(Pid).

init ([]) ->
    {ok,{{simple_one_for_one, 3, 180},
         [
          {undefined, {folsom_timer_server, start_link, []},
           transient, brutal_kill, worker, [folsom_sample_slide_server]}
         ]}}.

