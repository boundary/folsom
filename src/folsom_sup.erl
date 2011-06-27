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
%%% File:      folsom_sup.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

%% @doc Supervisor for the folsom application.

-module(folsom_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include("folsom.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    setup_ets_tables(),

    EventMgr = {folsom_event_manager,
                {gen_event, start_link, [{local, folsom_event_manager}]},
                permanent,
                brutal_kill,
                worker,
                dynamic},

    Processes = [EventMgr],
    
    {ok, { {one_for_one, 10, 10}, Processes} }.

setup_ets_tables() ->
    ets:new(?COUNTER_TABLE, [set, named_table, public]),
    ets:new(?GAUGE_TABLE, [set, named_table, public]),
    ets:new(?HISTOGRAM_TABLE, [set, named_table, public]),
    ets:new(?METER_TABLE, [set, named_table, public]),
    ets:new(?HISTORY_TABLE, [set, named_table, public]).
