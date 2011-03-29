%%%-------------------------------------------------------------------
%%% File:      folsom.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom).
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
    folsom_sup:start_link().

%% @spec start() -> ok
%% @doc Start the folsom server.
start() ->
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(folsom).

%% @spec stop() -> ok
%% @doc Stop the folsom server.
stop() ->
    Res = application:stop(folsom),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(inets),
    application:stop(crypto),
    Res.
