%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Basho Technologies, Inc.
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
%%% File:      folsom_sample_slide_server.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc
%%% Serialization point for folsom_sample_slide. Handles
%%% pruning of older smaples. One started per histogram.
%%% See folsom.hrl, folsom_sample_slide, folsom_sample_slide_sup
%%% @end
%%%-----------------------------------------------------------------
-module(folsom_sample_slide_server).

-behaviour(gen_server).

%% API
-export([start_link/3, stop/1, resize/2]).

-record(state, {sample_mod,  reservoir, window}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(SampleMod, Reservoir, Window) ->
    gen_server:start_link(?MODULE, [SampleMod, Reservoir, Window], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init([SampleMod, Reservoir, Window]) ->
    {ok, #state{sample_mod = SampleMod, reservoir = Reservoir, window = Window}, timeout(Window)}.

resize(Pid, NewWindow) ->
    gen_server:call(Pid, {resize, NewWindow}).

handle_call({resize, NewWindow}, _From, State) ->
    NewState = State#state{window=NewWindow},
    Reply = ok,
    {reply, Reply, NewState, timeout(NewWindow)};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State=#state{sample_mod = SampleMod, reservoir = Reservoir, window = Window}) ->
    try
	SampleMod:trim(Reservoir, Window),
	{noreply, State, timeout(Window)}
    catch error:badarg ->
	    %% The ets table must have gone away; stop normally instead of bringing
	    %% the supervisor down with us.
	    {stop, normal, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

timeout(Window) ->
    timer:seconds(Window) div 2.
