-module(mit_db).

-import(proplists, [get_value/2]).

-export([merge/4, merge/5]).

merge(Table, NewList, OldList, NewFun) ->
    merge(Table, NewList, OldList, NewFun, true).

merge(Table, NewList, OldList, NewFun, IsDelete) ->
    NewIdxList = [Idx || {Idx, _} <- NewList],
    OldIdxList = [Idx || {Idx, _} <- OldList],
    
	{IdxAdded, IdxUpdated, IdxDeleted} = extlib:list_compare(NewIdxList, OldIdxList),

    %added
    Records = [NewFun(Idx, get_value(Idx, NewList)) || Idx <- IdxAdded],
    [epgsql:insert(main, Table, Record) || Record <- Records],

    %updated
    Updates = lists:map(fun(Idx) -> 
        New = get_value(Idx, NewList),
        Old = get_value(Idx, OldList),
        {get_value(id, Old), New -- Old}
    end, IdxUpdated),
    [epgsql:update(main, Table, Changed, {id, Id})
        || {Id, Changed} <- Updates, Changed =/= []],

    %deleted
    if
    IsDelete ->
        delete_by_ids(Table, [id(get_value(Idx, OldList)) || Idx <- IdxDeleted]);
    true ->
        ingore
    end.
    
delete_by_ids(_, []) ->
    ignore;

delete_by_ids(Table, Ids) ->
    epgsql:delete(main, Table, {'in', id, Ids}).

id(R) ->
    get_value(id, R).

