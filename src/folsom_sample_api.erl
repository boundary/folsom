%%%-------------------------------------------------------------------
%%% @author joe williams <j@fastip.com>
%%% @doc
%%% unified sample api
%%% @end
%%% Created : 22 Mar 2011 by joe williams <j@fastip.com>
%%%-------------------------------------------------------------------

-module(folsom_sample_api).

-export([
         new/1,
         new/2,
         new/3,
         update/3,
         get_values/2
        ]).

-include("folsom.hrl").

new(Type) ->
    new(Type, ?DEFAULT_SIZE, ?DEFAULT_ALPHA).

new(Type, Size) ->
    new(Type, Size, ?DEFAULT_ALPHA).

new(uniform, Size, _) ->
    folsom_sample_uniform:new(Size);
new(none, Size, _) ->
    folsom_sample_none:new(Size);
new(exdec, Size, Alpha) ->
    folsom_sample_exdec:new(Size, Alpha).

update(uniform, Sample, Value) ->
    folsom_sample_uniform:update(Sample, Value);
update(none, Sample, Value) ->
    folsom_sample_none:update(Sample, Value);
update(exdec, Sample, Value) ->
    folsom_sample_exdec:update(Sample, Value).

get_values(uniform, Sample) ->
    folsom_sample_uniform:get_values(Sample);
get_values(none, Sample) ->
    folsom_sample_none:get_values(Sample);
get_values(exdec, Sample) ->
    folsom_sample_exdec:get_values(Sample).
