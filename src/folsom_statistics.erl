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
%%% File:      folsom_statistics.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% statistics functions for calucating based on id and a list of values
%%% @end
%%%------------------------------------------------------------------

-module(folsom_statistics).

-export([get_max/1,
         get_min/1,
         get_histogram/1,
         get_variance/1,
         get_standard_deviation/1,
         get_covariance/2,
         get_kurtosis/1,
         get_skewness/1,
         get_mean/1,
         get_median/1,
         get_percentile/2,
         get_statistics/1]).

-define(HIST, [1, 5, 10, 20, 30, 40, 50, 100, 150,
               200, 250, 300, 350, 400, 450, 500,
               750, 1000, 1500, 2000, 3000, 4000,
               5000, 10000, 20000, 30000, 40000,
               50000, 99999999999999]).

-define(STATS_MIN, 5).


get_max([]) ->
    0;
get_max(Values) ->
    [Head | _] = lists:reverse(lists:sort(Values)),
    Head.

get_min([]) ->
    0;
get_min(Values) ->
    [Head | _] = lists:sort(Values),
    Head.

get_histogram(Values) ->
    Bins = [{Bin, 0} || Bin <- ?HIST],
    build_hist(Values, Bins).

% two pass variance
get_variance(Values) when length(Values) < ?STATS_MIN ->
    0;
get_variance(Values) ->
    Mean = get_mean(Values),
    List = [(Value - Mean) * (Value - Mean) || Value <- Values],
    Sum = lists:sum(List),
    Sum / (length(Values) - 1).

get_standard_deviation(Values) when length(Values) < ?STATS_MIN ->
    0;
get_standard_deviation(Values) ->
    math:sqrt(get_variance(Values)).

% two pass covariance
get_covariance(Values, _) when length(Values) < ?STATS_MIN ->
    0;
get_covariance(_, Values) when length(Values) < ?STATS_MIN ->
    0;
get_covariance(Values1, Values2) ->
    Mean1 = get_mean(Values1),
    Mean2 = get_mean(Values2),
    Zip = lists:zip(Values1, Values2),
    List = [((X1 - Mean1) * (X2 - Mean2))  / length(Values1) || {X1, X2} <- Zip],
    lists:sum(List).

get_kurtosis(Values) when length(Values) < ?STATS_MIN ->
    0;
get_kurtosis(Values) ->
    Mean = get_mean(Values),
    StdDev = get_standard_deviation(Values),
    Count = length(Values),
    get_kurtosis(Values, Mean, StdDev, Count).

get_skewness(Values) when length(Values) < ?STATS_MIN ->
    0;
get_skewness(Values) ->
    Mean = get_mean(Values),
    StdDev = get_standard_deviation(Values),
    Count = length(Values),
    get_skewness(Values, Mean, StdDev, Count).

get_mean(Values) when length(Values) < ?STATS_MIN ->
    0;
get_mean(Values) ->
    Sum = lists:sum(Values),
    Sum / length(Values).

get_median(Values) when length(Values) < ?STATS_MIN ->
    0;
get_median(Values) when is_list(Values) ->
    get_percentile(Values, 0.5).

get_percentile(Values, _) when length(Values) < ?STATS_MIN ->
    0;
get_percentile(Values, Percentile) when is_list(Values) ->
    SortedValues = lists:sort(Values),
    Element = round(Percentile * length(SortedValues)),
    lists:nth(Element, SortedValues).

% calculates stats on a sample
get_statistics(Values) ->
    [
     {min, get_min(Values)},
     {max, get_max(Values)},
     {mean, get_mean(Values)},
     {median, get_median(Values)},
     {variance, get_variance(Values)},
     {standard_deviation, get_standard_deviation(Values)},
     {skewness, get_skewness(Values)},
     {kurtosis, get_kurtosis(Values)},
     {percentile,
      [
       {75, get_percentile(Values, 0.75)},
       {95, get_percentile(Values, 0.95)},
       {99, get_percentile(Values, 0.99)},
       {999, get_percentile(Values, 0.999)}
      ]
     },
     {histogram, get_histogram(Values)}
     ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

% these histogram functions are too complicated, find better solution
build_hist([Head | Tail], Hist) ->
    {Bin, Count} = proplists:lookup(which_bin(Head, Hist, []), Hist),
    List = proplists:delete(Bin, Hist),
    NewHist = lists:append(List, [{Bin, Count + 1}]),
    build_hist(Tail, NewHist);
build_hist([], Hist) ->
    lists:sort(Hist).

which_bin(Value, [{Bin, _} = B | Tail], Acc) when Value =< Bin ->
    which_bin(Value, Tail, lists:sort(lists:append(Acc, [B])));
which_bin(Value, [_ | Tail], Acc) ->
    which_bin(Value, Tail, Acc);
which_bin(_, [], [{Bin, _} | _]) ->
    Bin.

% my best estimation of excess kurtosis thus far
% http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm
get_kurtosis([], _, _, _) ->
    0;
get_kurtosis(Values, Mean, StdDev, Count) ->
    List1 = [math:pow(Value - Mean, 4) || Value <- Values],
    (lists:sum(List1) / ((Count - 1) * math:pow(StdDev, 4)) ) - 3.

% results match excel calculation in my testing
get_skewness([], _, _, _) ->
    0;
get_skewness(Values, Mean, StdDev, Count) ->
    List = [math:pow((Value - Mean) / StdDev, 3) * Count || Value <- Values],
    lists:sum(List) / ( (Count - 1) * (Count - 2) ).
