-module(folsom_utils).

-export([
         get_handlers_info/1,
         get_tagged_handlers/2
        ]).

get_handlers_info(Module) ->
    Handlers = Module:get_handlers(),
    [Module:get_info(Handler) || Handler <- Handlers].

get_tagged_handlers(Module, Tag) ->
    Handlers = Module:get_handlers(),
    List = [get_tags_from_info(Module, Handler) || Handler <- Handlers],
    build_tagged_handler_list(List, Tag, []).

get_tags_from_info(Module, Handler) ->
    [{Id, Values}] = Module:get_info(Handler),
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