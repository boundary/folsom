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
%%% File:      folsom_sample.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_sample).

-export([
         new/1,
         new/2,
         new/3,
         new/4,
         update/3,
         get_values/2
        ]).

-include("folsom.hrl").

-define(RESTART, permanent).
-define(SHUTDOWN, 2000).

%% API

new(Type) ->
    new(Type, ?DEFAULT_SIZE, ?DEFAULT_ALPHA).

new(Type, Size) ->
    new(Type, Size, ?DEFAULT_ALPHA).

new(uniform, Size, _) ->
    folsom_sample_uniform:new(Size);
new(none, Size, _) ->
    folsom_sample_none:new(Size).

new(Name, exdec, Size, Alpha) ->
    ChildSpec = {Name,
                 {folsom_sample_exdec, start_link, [Name, Size, Alpha]},
                 ?RESTART, ?SHUTDOWN, worker, [folsom_sample_exdec]
                },
    supervisor:start_child(folsom_sample_sup, ChildSpec).

update(uniform, Sample, Value) ->
    folsom_sample_uniform:update(Sample, Value);
update(none, Sample, Value) ->
    folsom_sample_none:update(Sample, Value);
update(Name, exdec, Value) ->
    folsom_sample_exdec:update(Name, Value).

get_values(uniform, Sample) ->
    folsom_sample_uniform:get_values(Sample);
get_values(none, Sample) ->
    folsom_sample_none:get_values(Sample);
get_values(Name, exdec) ->
    {_, Values} = lists:unzip(ets:tab2list(Name)),
    Values.
