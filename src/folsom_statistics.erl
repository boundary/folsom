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

-compile([export_all]).

-export([
         get_statistics/1,
         get_statistics/2
        ]).

-define(HIST_BINS, 10).

-define(STATS_MIN, 5).

-record(scan_result, {n=0, sumX=0, sumXX=0, sumInv=0, sumLog, max, min}).
-record(scan_result2, {x2=0, x3=0, x4=0}).

-compile([native]).

get_statistics(Values) ->
    Scan_res = scan_values(Values),
    Scan_res2 = scan_values2(Values, Scan_res),
    Variance = variance(Scan_res, Scan_res2),
    SortedValues = lists:sort(Values),
    [
     {min, Scan_res#scan_result.min},
     {max, Scan_res#scan_result.max},
     {arithmetic_mean, arithmetic_mean(Scan_res)},
     {geometric_mean, geometric_mean(Scan_res)},
     {harmonic_mean, harmonic_mean(Scan_res)},
     {median, percentile(SortedValues, Scan_res, 0.5)},
     {variance, Variance},
     {standard_deviation, std_deviation(Scan_res, Scan_res2)},
     {skewness, skewness(Scan_res, Scan_res2)},
     {kurtosis, kurtosis(Scan_res, Scan_res2)},
     {percentile,
      [
       {75, percentile(SortedValues, Scan_res, 0.75)},
       {95, percentile(SortedValues, Scan_res, 0.95)},
       {99, percentile(SortedValues, Scan_res, 0.99)},
       {999, percentile(SortedValues, Scan_res, 0.999)}
      ]
     },
     {histogram, get_histogram(Values, Scan_res)}
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

scan_values([X|Values]) ->
    scan_values(Values, #scan_result{n=1, sumX=X, sumXX=X*X,
             sumLog=math_log(X),
             max=X, min=X, sumInv=inverse(X)}).

scan_values([X|Values],
      #scan_result{n=N, sumX=SumX, sumXX=SumXX, sumLog=SumLog,
                   max=Max, min=Min, sumInv=SumInv}=Acc) ->
    scan_values(Values,
                Acc#scan_result{n=N+1, sumX=SumX+X, sumXX=SumXX+X*X,
                                sumLog=SumLog+math_log(X),
                                max=max(X,Max), min=min(X,Min),
                                sumInv=SumInv+inverse(X)});
scan_values([], Acc) ->
    Acc.

scan_values2(Values, #scan_result{n=N, sumX=SumX}) ->
    scan_values2(Values, SumX/N, #scan_result2{}).

scan_values2([X|Values], Mean, #scan_result2{x2=X2, x3=X3, x4=X4}=Acc) ->
    Diff = X-Mean,
    Diff2 = Diff*Diff,
    Diff3 = Diff2*Diff,
    Diff4 = Diff2*Diff2,
    scan_values2(Values, Mean, Acc#scan_result2{x2=X2+Diff2, x3=X3+Diff3,
            x4=X4+Diff4});
scan_values2([], _, Acc) ->
    Acc.


arithmetic_mean(#scan_result{n=N, sumX=Sum}) ->
    Sum/N.

geometric_mean(#scan_result{n=N, sumLog=SumLog}) ->
    math:exp(SumLog/N).

harmonic_mean(#scan_result{n=N, sumInv=Sum}) ->
    N/Sum.

percentile(SortedValues, #scan_result{n=N}, Percentile)
  when is_list(SortedValues) ->
    Element = round(Percentile * N),
    lists:nth(Element, SortedValues).

%% Two pass variance
%% Results match those given by the 'var' function in R
variance(#scan_result{n=N}, #scan_result2{x2=X2}) ->
    X2/(N-1).

std_deviation(Scan_res, Scan_res2) ->
    math:sqrt(variance(Scan_res, Scan_res2)).

%% http://en.wikipedia.org/wiki/Skewness
%%
%% skewness results should match this R function:
%% skewness <- function(x) {
%%    m3 <- mean((x - mean(x))^3)
%%    skew <- m3 / (sd(x)^3)
%%    skew
%% }
skewness(#scan_result{n=N}=Scan_res, #scan_result2{x3=X3}=Scan_res2) ->
    (X3/N)/(math:pow(std_deviation(Scan_res,Scan_res2), 3)).

%% http://en.wikipedia.org/wiki/Kurtosis
%%
%% results should match this R function:
%% kurtosis <- function(x) {
%%     m4 <- mean((x - mean(x))^4)
%%     kurt <- m4 / (sd(x)^4) - 3
%%     kurt
%% }
kurtosis(#scan_result{n=N}=Scan_res, #scan_result2{x4=X4}=Scan_res2) ->
    (X4/N)/(math:pow(std_deviation(Scan_res,Scan_res2), 4)) - 3.

get_histogram(Values, Scan_res) ->
    Bins = get_hist_bins(Scan_res#scan_result.min, Scan_res#scan_result.max),
    Dict = lists:foldl(fun (Value, Dict) ->
             update_bin(Value, Bins, Dict)
           end,
           dict:from_list([{Bin, 0} || Bin <- Bins]),
           Values),
    lists:sort(dict:to_list(Dict)).

update_bin(Value, [Bin|_Bins], Dict) when Value =< Bin ->
    dict:update_counter(Bin, 1, Dict);
update_bin(Values, [_Bin|Bins], Dict) ->
    update_bin(Values, Bins, Dict).

%% two pass covariance
%% (http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Covariance)
%% matches results given by excel's 'covar' function
get_covariance(Values, _) when length(Values) < ?STATS_MIN ->
    0.0;
get_covariance(_, Values) when length(Values) < ?STATS_MIN ->
    0.0;
get_covariance(Values1, Values2) when length(Values1) /= length(Values2) ->
    0.0;
get_covariance(Values1, Values2) ->
    {SumX, SumY, N} = foldl2(fun (X, Y, {SumX, SumY, N}) ->
             {SumX+X, SumY+Y, N+1}
           end, {0,0,0}, Values1, Values2),
    MeanX = SumX/N,
    MeanY = SumY/N,
    Sum = foldl2(fun (X, Y, Sum) ->
       Sum + ((X - MeanX) * (Y - MeanY))
     end,
     0, Values1, Values2),
    Sum/N.

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
    TR1 = ranks_of(Values1),
    TR2 = ranks_of(Values2),
    Numerator   = 6 * foldl2(fun (X, Y, Acc) ->
             Diff = X-Y,
             Acc + Diff*Diff
           end, 0, TR1,TR2),
    N = length(Values1),
    Denominator = math:pow(N,3)-N,
    1-(Numerator/Denominator).

ranks_of(Values) when is_list(Values) ->
    [Fst|Rest] = revsort(Values),
    TRs = ranks_of(Rest, [], 2, Fst, 1),
    Dict = gb_trees:from_orddict(TRs),
    L = lists:foldl(fun (Val, Acc) ->
                            Rank = gb_trees:get(Val, Dict),
                            [Rank|Acc]
                    end, [], Values),
    lists:reverse(L).

ranks_of([E|Es],Acc, N, E, S) ->
    ranks_of(Es, Acc, N+1, E, S);
ranks_of([E|Es], Acc, N, P, S) ->
    ranks_of(Es,[{P,(S+N-1)/2}|Acc], N+1, E, N);
ranks_of([],  Acc, N, P, S) ->
    [{P,(S+N-1)/2}|Acc].


get_pearson_correlation(Values, _) when length(Values) < ?STATS_MIN ->
    0.0;
get_pearson_correlation(_, Values) when length(Values) < ?STATS_MIN ->
    0.0;
get_pearson_correlation(Values1, Values2) when length(Values1) /= length(Values2) ->
    0.0;
get_pearson_correlation(Values1, Values2) ->
    {SumX, SumY, SumXX, SumYY, SumXY, N} =
  foldl2(fun (X,Y,{SX, SY, SXX, SYY, SXY, N}) ->
          {SX+X, SY+Y, SXX+X*X, SYY+Y*Y, SXY+X*Y, N+1}
        end, {0,0,0,0,0,0}, Values1, Values2),
    Numer = (N*SumXY) - (SumX * SumY),
    case math:sqrt(((N*SumXX)-(SumX*SumX)) * ((N*SumYY)-(SumY*SumY))) of
  0.0 ->
      0.0; %% Is this really the correct thing to do here?
  Denom ->
      Numer/Denom
    end.

revsort(L) ->
    lists:reverse(lists:sort(L)).

%% Foldl over two lists
foldl2(F, Acc, [I1|L1], [I2|L2]) when is_function(F,3) ->
    foldl2(F, F(I1, I2, Acc), L1, L2);
foldl2(_F, Acc, [], []) ->
    Acc.

%% wrapper for math:log/1 to avoid dividing by zero
math_log(0) ->
    1;
math_log(X) ->
    math:log(X).

%% wrapper for calculating inverse to avoid dividing by zero
inverse(0) ->
    0;
inverse(X) ->
    1/X.

get_hist_bins(Min, Max) ->
    Width = round((Max - Min) / ?HIST_BINS),
    get_bin_list(Width, ?HIST_BINS, []).

get_bin_list(Width, Bins, Acc) when Bins > length(Acc) ->
    Bin = ((length(Acc) + 1) * Width ),
    get_bin_list(Width, Bins, [round_bin(Bin) | Acc]);
get_bin_list(_, _, Acc) ->
    lists:usort(Acc).

round_bin(Bin) ->
    Base = erlang:trunc(math:pow(10, round(math:log10(Bin) - 1))),
    round_bin(Bin, Base).

round_bin(Bin, Base) when Bin rem Base == 0 ->
    Bin;
round_bin(Bin, Base) ->
    Bin + Base - (Bin rem Base).
