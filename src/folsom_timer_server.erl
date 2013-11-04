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
%%% File:      folsom_timer_server.erl
%%% @author    Russell Brown <russelldb@basho.com>,
%%%            Andrey Vasenin <vasenin@aboutecho.com>
%%% @doc
%%% Evaluates apply(Module, Function, Arguments) repeatedly at intervals of Time.
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_timer_server).

-behaviour(gen_server).

%% API
-export([start_link/4, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {time, module, func, args, timer_pid}).

start_link(Time, Module, Func, Args) ->
    gen_server:start_link(?MODULE, [Time, Module, Func, Args], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init([Time, Module, Func, Args]) ->
    Pid = erlang:send_after(Time, self(), tick),
    {ok, #state{time = Time, timer_pid = Pid, module = Module, func = Func, args = Args}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State=#state{time = Time, module = Module, func = Func, args = Args, timer_pid = OldTimer}) ->
    erlang:cancel_timer(OldTimer),
    apply(Module, Func, Args),
    Pid = erlang:send_after(Time, self(), tick),
    {noreply, State#state{timer_pid = Pid}};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{timer_pid = Pid}) ->
    erlang:cancel_timer(Pid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
