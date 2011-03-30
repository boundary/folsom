%%%-------------------------------------------------------------------
%%% @author joe williams <j@fastip.com>
%%% @doc
%%% unified api for most event handler stuff
%%% @end
%%% Created : 22 Mar 2011 by joe williams <j@fastip.com>
%%%-------------------------------------------------------------------

-module(folsom_internal_api).

-export([
         add_handler/4,
         add_sup_handler/4,
         delete_handler/3,
         handler_exists/2,
         notify/2,
         get_handlers_info/2,
         get_tagged_handlers/3,
         get_handlers/1,
         get_info/3
        ]).

add_handler(Manager, Module, Id, Args) ->
    gen_event:add_handler(Manager, {Module, Id}, Args).

add_sup_handler(Manager, Module, Id, Args) ->
    gen_event:add_sup_handler(Manager, {Module, Id}, Args).

delete_handler(Manager, Module, Id) ->
    gen_event:delete_handler(Manager, {Module, Id}, nil).

handler_exists(Manager, Id) ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(Manager)),
    lists:member(Id, Handlers).

notify(Manager, Event) ->
    gen_event:notify(Manager, Event).

get_handlers_info(Manager, Module) ->
    Handlers = get_handlers(Manager),
    [get_info(Manager, Module, Id) || Id <- Handlers].

get_tagged_handlers(Manager, Module, Tag) ->
    Handlers = get_handlers(Manager),
    List = [get_tags_from_info(Manager, Module, Id) || Id <- Handlers],
    build_tagged_handler_list(List, Tag, []).

get_handlers(Manager) ->
    {_, Handlers} = lists:unzip(gen_event:which_handlers(Manager)),
    Handlers.

get_info(Manager, Module, Id) ->
    gen_event:call(Manager, {Module, Id}, info).

% internal functions

get_tags_from_info(Manager, Module, Id) ->
    [{Id, Values}] = get_info(Manager, Module, Id),
    Tags = proplists:get_value(tags, Values),
    {Id, Tags}.

build_tagged_handler_list([], _, Acc) ->
    Acc;
build_tagged_handler_list([{Id, Tags} | Tail], Tag, Acc) ->
    NewAcc = maybe_append_handler(lists:member(Tag, Tags), Id, Acc),
    build_tagged_handler_list(Tail, Tag, NewAcc).

maybe_append_handler(true, Id, Acc) ->
    lists:append([Id], Acc);
maybe_append_handler(false, _, Acc) ->
    Acc.
