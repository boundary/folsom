%%%-------------------------------------------------------------------
%%% File:      emetrics_sup.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% @end
%%%------------------------------------------------------------------

%% @doc Supervisor for the emetrics application.

-module(emetrics_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

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
    Ip = case os:getenv("EMETRICS_IP") of false -> "0.0.0.0"; Any -> Any end,
    Port = case os:getenv("EMETRICS_PORT") of false -> "5555"; Any1 -> Any1 end,
    LogDir = case os:getenv("EMETRICS_LOG_DIR") of false -> "priv/log"; Any2 -> Any2 end,
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

    EventMgr = {emetrics_event_manager,
                {gen_event, start_link, [{local, emetrics_event_manager}]},
                permanent,
                brutal_kill,
                worker,
                dynamic},

    Processes = [Web, EventMgr],
    {ok, { {one_for_one, 10, 10}, Processes} }.
