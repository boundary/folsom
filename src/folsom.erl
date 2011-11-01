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
%%% File:      folsom.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom).
-export([start/0]).

-include("folsom.hrl").

start() ->
    Tables = [
              {?FOLSOM_TABLE, [set, named_table, public, {read_concurrency, true}]},
              {?COUNTER_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?GAUGE_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?HISTOGRAM_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?METER_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?HISTORY_TABLE, [set, named_table, public, {write_concurrency, true}]}
             ],
    [maybe_create_table(ets:info(Name), Name, Opts) || {Name, Opts} <- Tables],
    ok.

maybe_create_table(undefined, Name, Opts) ->
    ets:new(Name, Opts);
maybe_create_table(_, _, _) ->
    ok.
