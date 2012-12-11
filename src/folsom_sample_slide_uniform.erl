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

-module(folsom_sample_slide_uniform).

-export([
         new/1,
         update/2,
         get_values/1,
         moment/0,
         trim/2
        ]).

-include("folsom.hrl").

new({Window, SampleSize}) ->
    Sample = #slide_uniform{window = Window, size = SampleSize},
    ets:insert(Sample#slide_uniform.reservoir, {size, SampleSize}),
    Pid = folsom_sample_slide_sup:start_slide_server(?MODULE, Sample#slide_uniform.reservoir, Sample#slide_uniform.window),
    Sample#slide_uniform{server=Pid}.

update(#slide_uniform{reservoir = Reservoir, size = Size} = Sample0, Value) ->
    Now = os:timestamp(),
    Moment = folsom_utils:now_epoch(Now),
    {Rnd, _} = random:uniform_s(Size, Now),
    folsom_utils:update_element(Reservoir, Moment, {Size+1, undefined}, {Rnd+1, Value}),
    Sample0.

get_values(#slide_uniform{window = Window, reservoir = Reservoir, size=Size}) ->
    Oldest = moment() - Window,
    Moments=ets:select(Reservoir, [{match(Size+1),[{'>=', '$1', Oldest}],['$_']}]),
    Values=lists:flatmap(fun(T) ->
                                 %% Start at element 2 to skip key (the moment)
                                 get_samples(2, tuple_size(T), T, [])
                         end, Moments),
    Values.

%% Convert the tuple of samples into a list, removing undefined values. I'm
%% assuming this is faster than tuple_to_list + filter to remove undefined,
%% but we should probably benchmark if we care.
%%
%% Note: Technically, we have Size+1 samples when full and should drop the
%% extra sample if we want to match the semantics of the prior implementation
%% that did not insert a sample when random() == Size.  Of course, was that
%% just an implementation detail? Isn't providing more samples a good thing?
get_samples(Same, Same, _, Acc) ->
    Acc;
get_samples(N, Size, T, Acc) ->
    Acc2 = case element(N, T) of
               undefined ->
                   Acc;
               X ->
                   [X|Acc]
           end,
    get_samples(N+1, Size, T, Acc2).

moment() ->
    folsom_utils:now_epoch().

trim(Reservoir, Window) ->
    case ets:lookup(Reservoir, size) of
        [{size, Size}] ->
            Oldest = moment() - Window,
            ets:select_delete(Reservoir, [{match(Size+1),[{'<', '$1', Oldest}],['true']}]);
        _ ->
            ok
    end.

match(Size) ->
    erlang:make_tuple(Size+1, '_', [{1, '$1'}]).
