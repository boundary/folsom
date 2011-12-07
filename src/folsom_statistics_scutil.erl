%% taken from http://crunchyd.com/scutil/
%% All code here is MIT Licensed
%%  http://scutil.com/license.html

-module(folsom_statistics_scutil).

-export([
         arithmetic_mean/1,
         geometric_mean/1,
         harmonic_mean/1,
         kendall_correlation/2,
         spearman_correlation/2,
         pearson_correlation/2
        ]).

-compile([native]).

% http://en.wikipedia.org/wiki/Arithmetic_mean
arithmetic_mean([]) ->
    0.0;
arithmetic_mean(List) when is_list(List) ->
    lists:sum(List) / length(List).

% http://en.wikipedia.org/wiki/Geometric_mean
geometric_mean([]) ->
    0.0;
geometric_mean(List) when is_list(List) ->
    math:exp(arithmetic_mean([ math_log(X) || X<-List ])).

% http://en.wikipedia.org/wiki/Harmonic_mean
harmonic_mean([]) ->
    0.0;
harmonic_mean(List) when is_list(List) ->
  length(List) / lists:sum([ inverse(X) || X<-List ]).


% seems to match the value returned by the 'cor' (method="kendal") R function
% http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient
kendall_correlation(List1, List2) when is_list(List1), is_list(List2) ->
    {RA,_} = lists:unzip(tied_ordered_ranking(List1)),
    {RB,_} = lists:unzip(tied_ordered_ranking(List2)),

    Ordering = lists:keysort(1, lists:zip(RA,RB)),
    {_,OrdB} = lists:unzip(Ordering),

    N = length(List1),
    P = lists:sum(kendall_right_of(OrdB, [])),

    -(( (4*P) / (N * (N - 1))) - 1).

% seems to match the value returned by the 'cor' (method="spearman") R function
% http://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient
spearman_correlation(List1, List2) when is_list(List1), is_list(List2) ->
    {TR1,_} = lists:unzip(simple_ranking(List1)),
    {TR2,_} = lists:unzip(simple_ranking(List2)),

    Numerator   = 6 * lists:sum([ square(D1-D2) || {D1,D2} <- lists:zip(TR1,TR2) ]),
    Denominator = math:pow(length(List1),3)-length(List1),

    1-(Numerator/Denominator).

% seems to match the value returned by the 'cor' (method="pearson") R function
% http://en.wikipedia.org/wiki/Pearson_correlation_coefficient
pearson_correlation(List1, List2) when is_list(List1), is_list(List2) ->
    SumXY = lists:sum([A*B || {A,B} <- lists:zip(List1,List2) ]), % the sum of the products of each matched pair

    SumX  = lists:sum(List1),
    SumY  = lists:sum(List2),

    SumXX = lists:sum([L*L || L<-List1]), % the sums of the squared items
    SumYY = lists:sum([L*L || L<-List2]),

    N     = length(List1),

    case math:sqrt(   ( (N*SumXX)-(SumX*SumX) )   *   ( (N*SumYY)-(SumY*SumY) )   ) of
        0 ->
            0.0; % some nasty value sets otherwise cause divide by zero
        0.0 ->
            0.0; % eg [ [1,1,1,1,1], [1,1,2,1,2] ]
        Denom ->
          Numer = (N*SumXY) - (SumX * SumY),
          (Numer/Denom)
    end.

%%%===================================================================
%%% Internal functions
%%%==================================================================

simple_ranking(List) when is_list(List) ->
    lists:zip(lists:seq(1,length(List)),lists:reverse(lists:sort(List))).

tied_ranking(List) ->
    tied_rank_worker(simple_ranking(List), [], no_prev_value).

tied_ordered_ranking(List) when is_list(List) ->
    tied_ordered_ranking(List, tied_ranking(List), []).

tied_ordered_ranking([], [], Work) ->
    lists:reverse(Work);

tied_ordered_ranking([Front|Rem], Ranks, Work) ->
    {value,Item}  = lists:keysearch(Front,2,Ranks),
    {IRank,Front} = Item,
    tied_ordered_ranking(Rem, Ranks--[Item], [{IRank,Front}]++Work).

square(Value) ->
    math:pow(Value, 2).

kendall_right_of([], Work) ->
    lists:reverse(Work);
kendall_right_of([F|R], Work) ->
    kendall_right_of(R, [kendall_right_of_item(F,R)]++Work).

kendall_right_of_item(B, Rem) ->
    length([R || R <- Rem, R < B]).

tied_add_prev(Work, {FoundAt, NewValue}) ->
    lists:duplicate( length(FoundAt), {lists:sum(FoundAt)/length(FoundAt), NewValue} ) ++ Work.

tied_rank_worker([], Work, PrevValue) ->
    lists:reverse(tied_add_prev(Work, PrevValue));

tied_rank_worker([Item|Remainder], Work, PrevValue) ->
    case PrevValue of
        no_prev_value ->
            {BaseRank,BaseVal} = Item,
            tied_rank_worker(Remainder, Work, {[BaseRank],BaseVal});
        {FoundAt,OldVal} ->
            case Item of
                {Id,OldVal} ->
                    tied_rank_worker(Remainder, Work, {[Id]++FoundAt,OldVal});
                {Id,NewVal} ->
                    tied_rank_worker(Remainder, tied_add_prev(Work, PrevValue), {[Id],NewVal})

            end
    end.

%% wrapper for math:log/1 to avoid dividing by zero
math_log(0) ->
    0;
math_log(X) ->
    math:log(X).

%% wrapper for calculating inverse to avoid dividing by zero
inverse(0) ->
    0;
inverse(X) ->
    1/X.
