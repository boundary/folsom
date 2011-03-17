-module(emetrics).
-author('Andy Gross <andy@basho.com>').
-author('Justin Sheehy <justin@@basho.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    emetrics_sup:start_link().

%% @spec start() -> ok
%% @doc Start the emetrics server.
start() ->
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(emetrics).

%% @spec stop() -> ok
%% @doc Stop the emetrics server.
stop() ->
    Res = application:stop(emetrics),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(inets),
    application:stop(crypto),
    Res.
