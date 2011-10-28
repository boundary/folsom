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
%%% File:      folsom_statistics.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% statistics functions for calucating based on id and a list of values
%%% @end
%%%------------------------------------------------------------------

-module(folsom_statistics).

-export([get_max/1,
         get_min/1,
         get_rate/3,
         get_rate/4,
         get_histogram/1,
         get_variance/1,
         get_standard_deviation/1,
         get_covariance/2,
         get_kurtosis/1,
         get_skewness/1,
         get_median/1,
         get_percentile/2,
         get_statistics/1,
         get_statistics/2]).

-define(HIST, [1, 5, 10, 20, 30, 40, 50, 100, 150,
               200, 250, 300, 350, 400, 450, 500,
               750, 1000, 1500, 2000, 3000, 4000,
               5000, 10000, 20000, 30000, 40000,
               50000, 99999999999999]).

-define(STATS_MIN, 5).

-compile([native,export_all]).


get_max([]) ->
    0.0;
get_max(Values) ->
    lists:max(Values).

get_min([]) ->
    0.0;
get_min(Values) ->
    lists:min(Values).

get_rate(Value1, Value2, Interval) ->
    Delta = Value1 - Value2,
    Delta / Interval.

% time values here are based on epoch i.e. an integer
get_rate(Value1, Value2, Time1, Time2) ->
    Interval = Time2 - Time1,
    get_rate(Value1, Value2, Interval).

get_histogram(Values) ->
    Dict = lists:foldl(fun (Value, Dict) ->
			       update_bin(Value, ?HIST, Dict)
		       end,
		       dict:from_list([{Bin, 0} || Bin <- ?HIST]),
		       Values),
    lists:sort(dict:to_list(Dict)).

% two pass variance
% results match those given by the 'var' function in R
get_variance(Values) when length(Values) < ?STATS_MIN ->
    0.0;
get_variance(Values) ->
    Mean = folsom_statistics_scutil:arithmetic_mean(Values),
    List = [(Value - Mean) * (Value - Mean) || Value <- Values],
    Sum = lists:sum(List),
    Sum / (length(Values) - 1).

% results match those given by the 'sd' function in R
get_standard_deviation(Values) when length(Values) < ?STATS_MIN ->
    0.0;
get_standard_deviation(Values) ->
    math:sqrt(get_variance(Values)).

% two pass covariance (http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Covariance)
% matches results given by excel's 'covar' function
get_covariance(Values, _) when length(Values) < ?STATS_MIN ->
    0.0;
get_covariance(_, Values) when length(Values) < ?STATS_MIN ->
    0.0;
get_covariance(Values1, Values2) ->
    Mean1 = folsom_statistics_scutil:arithmetic_mean(Values1),
    Mean2 = folsom_statistics_scutil:arithmetic_mean(Values2),
    Zip = lists:zip(Values1, Values2),
    Samples = length(Values1),
    lists:foldl(fun ({X1,X2}, Sum) ->
    			Sum + ((X1 - Mean1) * (X2 - Mean2))  / Samples
    		end,
    		0, Zip).

get_kendall_correlation(Values, _) when length(Values) < ?STATS_MIN ->
    0.0;
get_kendall_correlation(_, Values) when length(Values) < ?STATS_MIN ->
    0.0;
get_kendall_correlation(Values1, Values2) when length(Values1) /= length(Values2) ->
    0.0;
get_kendall_correlation(Values1, Values2) ->
    folsom_statistics_scutil:kendall_correlation(Values1, Values2).

get_spearman_correlation(Values, _) when length(Values) < ?STATS_MIN ->
    0.0;
get_spearman_correlation(_, Values) when length(Values) < ?STATS_MIN ->
    0.0;
get_spearman_correlation(Values1, Values2) when length(Values1) /= length(Values2) ->
    0.0;
get_spearman_correlation(Values1, Values2) ->
    folsom_statistics_scutil:spearman_correlation(Values1, Values2).

get_pearson_correlation(Values, _) when length(Values) < ?STATS_MIN ->
    0.0;
get_pearson_correlation(_, Values) when length(Values) < ?STATS_MIN ->
    0.0;
get_pearson_correlation(Values1, Values2) when length(Values1) /= length(Values2) ->
    0.0;
get_pearson_correlation(Values1, Values2) ->
    folsom_statistics_scutil:pearson_correlation(Values1, Values2).

% http://en.wikipedia.org/wiki/Kurtosis
%
% results should match this R function:
% kurtosis <- function(x) {
%     m4 <- mean((x - mean(x))^4)
%     kurt <- m4 / (sd(x)^4) - 3
%     kurt
% }
get_kurtosis(Values) when length(Values) < ?STATS_MIN ->
    0;
get_kurtosis(Values) ->
    Mean = folsom_statistics_scutil:arithmetic_mean(Values),
    M4 = folsom_statistics_scutil:arithmetic_mean([math:pow(X - Mean, 4) || X <- Values]),
    M4 / (math:pow(get_standard_deviation(Values), 4)) - 3.

% http://en.wikipedia.org/wiki/Skewness
%
% skewness results should match this R function:
% skewness <- function(x) {
%    m3 <- mean((x - mean(x))^3)
%    skew <- m3 / (sd(x)^3)
%    skew
% }
get_skewness(Values) when length(Values) < ?STATS_MIN ->
    0;
get_skewness(Values) ->
    Mean = folsom_statistics_scutil:arithmetic_mean(Values),
    M3 = folsom_statistics_scutil:arithmetic_mean([math:pow(X - Mean, 3) || X <- Values]),
    M3 / (math:pow(get_standard_deviation(Values), 3)).

get_median(Values) when length(Values) < ?STATS_MIN ->
    0.0;
get_median(Values) when is_list(Values) ->
    get_percentile(Values, 0.5).

get_percentile(Values, _) when length(Values) < ?STATS_MIN ->
    0.0;
get_percentile(Values, Percentile) when is_list(Values) ->
    SortedValues = lists:sort(Values),
    Element = round(Percentile * length(SortedValues)),
    lists:nth(Element, SortedValues).

% calculates stats on a sample
get_statistics(Values) ->
    [
     {min, get_min(Values)},
     {max, get_max(Values)},
     {arithmetic_mean, folsom_statistics_scutil:arithmetic_mean(Values)},
     {geometric_mean, folsom_statistics_scutil:geometric_mean(Values)},
     {harmonic_mean, folsom_statistics_scutil:harmonic_mean(Values)},
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

get_statistics(Values1, Values2) ->
    [
     {covariance, get_covariance(Values1, Values2)},
     {tau, get_kendall_correlation(Values1, Values2)},
     {rho, get_pearson_correlation(Values1, Values2)},
     {r, get_spearman_correlation(Values1, Values2)}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_bin(Value, [Bin|_Bins], Dict) when Value =< Bin ->
    dict:update_counter(Bin, 1, Dict);
update_bin(Values, [_Bin|Bins], Dict) ->
    update_bin(Values, Bins, Dict).
