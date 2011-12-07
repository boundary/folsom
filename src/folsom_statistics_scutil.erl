%% taken from http://crunchyd.com/scutil/
%% All code here is MIT Licensed
%%  http://scutil.com/license.html

-module(folsom_statistics_scutil).

-export([
         kendall_correlation/2
        ]).
-compile([export_all]).
-compile([native]).

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
