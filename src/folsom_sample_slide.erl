%%%
%%% Copyright 2012, Basho Technologies, Inc.  All Rights Reserved.
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
%%% File:      folsom_sample_slide.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc
%%% Sliding window sample. Last Window seconds readings are recorded.
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_sample_slide).

-export([
         new/1,
         update/2,
         get_values/1,
         moment/0,
         trim/2
        ]).

-include("folsom.hrl").

new(Size) ->
    Sample = #slide{window = Size},
    Pid = folsom_sample_slide_sup:start_slide_server(Sample#slide.reservoir, Sample#slide.window),
    Sample#slide{server=Pid}.

update(#slide{reservoir = Reservoir} = Sample, Value) ->
    Moment = moment(),
    ets:insert(Reservoir, {Moment, Value}),
    Sample.

get_values(#slide{window = Window, reservoir = Reservoir}) ->
    Oldest = moment() - Window,
    ets:select(Reservoir, [{{'$1','$2'},[{'>=', '$1', Oldest}],['$2']}]).

moment() ->
    folsom_utils:now_epoch().

trim(Reservoir, Window) ->
    Oldest = moment() - Window,
    ets:select_delete(Reservoir, [{{'$1','_'},[{'<', '$1', Oldest}],['true']}]).
