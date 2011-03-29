%%%-------------------------------------------------------------------
%%% File:      folsom_statistics.erl
%%% @author    joe williams <j@fastip.com>
%%% @copyright 2011 fast_ip
%%% @doc
%%% statistics functions
%%% @end
%%%------------------------------------------------------------------

-module(folsom_statistics).

-export([get_max/1,
         get_min/1,
         get_histogram/1,
         get_histogram/2,
         get_variance/1,
         get_standard_deviation/1,
         get_covariance/2,
         get_kurtosis/1,
         get_skewness/1,
         get_mean/1,
         get_median/1,
         get_percentile/2]).

-define(HIST, [10, 20, 30, 50, 100, 200, 300, 400, 500, 1000, 99999999999999]).

get_max(Id) when is_atom(Id) ->
    get_max(folsom_metrics_event:get_values(Id));
get_max([]) ->
    0;
get_max(Values) ->
    [Head | _] = lists:reverse(lists:sort(Values)),
    Head.

get_min(Id) when is_atom(Id)->
    get_min(folsom_metrics_event:get_values(Id));
get_min([]) ->
    0;
get_min(Values) ->
    [Head | _] = lists:sort(Values),
    Head.

get_histogram(Id) ->
    get_histogram(Id, ?HIST).

get_histogram(Id, Hist) ->
    Bins = [{Bin, 0} || Bin <- Hist],
    Values = folsom_metrics_event:get_values(Id),
    build_hist(Values, Bins).

get_variance(Id) ->
    get_variance(Id, folsom_metrics_event:get_values(Id)).

% two pass variance
get_variance(_, []) ->
    0;
get_variance(_, [_]) ->
    0;
get_variance(Id, Values) ->
    Mean = get_mean(Id),
    List = [(Value - Mean) * (Value - Mean) || Value <- Values],
    Sum = lists:sum(List),
    Sum / (length(Values) - 1).

get_standard_deviation(Id) ->
    math:sqrt(get_variance(Id)).

% two pass covariance
get_covariance(Id1, Id2) ->
    Values1 = folsom_metrics_event:get_values(Id1),
    Values2 = folsom_metrics_event:get_values(Id2),
    Mean1 = get_mean(Id1),
    Mean2 = get_mean(Id2),
    get_covariance(Values1, Values2, Mean1, Mean2).

get_covariance([], _, _, _) ->
    0;
get_covariance(_, [], _, _) ->
    0;
get_covariance(Values1, Values2, Mean1, Mean2) ->
    Zip = lists:zip(Values1, Values2),
    List = [((X1 - Mean1) * (X2 - Mean2))  / length(Values1) || {X1, X2} <- Zip],
    lists:sum(List).

get_kurtosis(Id) ->
    Values = folsom_metrics_event:get_values(Id),
    Mean = get_mean(Id),
    StdDev = get_standard_deviation(Id),
    Count = length(Values),
    get_kurtosis(Values, Mean, StdDev, Count).

get_skewness(Id) ->
    Values = folsom_metrics_event:get_values(Id),
    Mean = get_mean(Id),
    StdDev = get_standard_deviation(Id),
    Count = length(Values),
    get_skewness(Values, Mean, StdDev, Count).

get_mean(Id) when is_atom(Id) ->
    get_mean(folsom_metrics_event:get_values(Id));
get_mean([]) ->
    0;
get_mean(Values) ->
    Sum = lists:sum(Values),
    Sum / length(Values).

get_median(Id) ->
    get_percentile(Id, 0.5).

get_percentile(Id, Percentile) when is_atom(Id) ->
    get_percentile(lists:sort(folsom_metrics_event:get_values(Id)), Percentile);
get_percentile([], _) ->
    0;
get_percentile(Values, Percentile) ->
    Element = round(Percentile * length(Values)),
    lists:nth(Element, Values).

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
