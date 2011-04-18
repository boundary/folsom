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
%%% File:      folsom_sup.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% @end
%%%------------------------------------------------------------------

%% @doc Supervisor for the folsom application.

-module(folsom_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include("folsom.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    setup_ets_tables(),

    Ip = case os:getenv("FOLSOM_IP") of false -> "127.0.0.1"; Any -> Any end,
    Port = case os:getenv("FOLSOM_PORT") of false -> "5565"; Any1 -> Any1 end,
    LogDir = case os:getenv("FOLSOM_LOG_DIR") of false -> "priv/log"; Any2 -> Any2 end,
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, LogDir},
                 {dispatch, Dispatch}
                ],

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent,
           5000,
           worker,
           dynamic},

    MetricsEventMgr = {folsom_metrics_event_manager,
                {gen_event, start_link, [{local, folsom_metrics_event_manager}]},
                permanent,
                brutal_kill,
                worker,
                dynamic},

    EventsEventMgr = {folsom_events_event_manager,
                {gen_event, start_link, [{local, folsom_events_event_manager}]},
                permanent,
                brutal_kill,
                worker,
                dynamic},

    Processes = [Web, MetricsEventMgr, EventsEventMgr],
    {ok, { {one_for_one, 10, 10}, Processes} }.

setup_ets_tables() ->
    ets:new(?COUNTER_TABLE, [set, named_table, public]),
    ets:new(?GAUGE_TABLE, [set, named_table, public]),
    ets:new(?HISTOGRAM_TABLE, [set, named_table, public]),
    ets:new(?METER_TABLE, [set, named_table, public]),
    ets:new(?TIMER_TABLE, [set, named_table, public]).
