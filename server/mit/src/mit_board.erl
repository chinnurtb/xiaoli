-module(mit_board).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([insert/2]).

insert(#node{dn=Dn, id=NodeId, categoryid=CatId, vendorid=VendorId}, Boards) ->
    NewIdxList = [Idx || {Idx, _} <- Boards],
    %load old ports
    {ok, Records} = epgsql:select(main, boards, {node_id, NodeId}),
    OldBoards = [{get_value(boardidx, R), R} || R <- Records],
    OldIdxList = [Idx || {Idx, _} <- OldBoards],

    Now = {datetime, {date(), time()}},
	{Added, Updated, Deleted} = extlib:list_compare(NewIdxList, OldIdxList),
    %Added
    lists:foreach(fun(Idx) -> 
        Record = [{dn, boardn(Dn, Idx)},
                 {node_id, NodeId},
                 {vendor_id, VendorId},
                 {category_id, CatId}, 
                 {boardidx, Idx},
                 {created_at, Now}
                 | get_value(Idx, Boards)],
        case epgsql:insert(main, boards, Record) of
        {error, Err} -> 
            ?ERROR("~p", [Err]),
            ?ERROR("~p", [Record]);
        _ -> ok
        end
    end, Added),

    %Updated
    lists:foreach(fun(Idx) -> 
        NewBoard = get_value(Idx, Boards),
        OldBoard = get_value(Idx, OldBoards),
        Id = get_value(id, OldBoard),
        Changed = NewBoard -- OldBoard,
        case Changed of
        [] -> 
            ignore;
        _ ->
            ?INFO("Port Changed: ~p", [Changed]),
            case epgsql:update(main, boards, Changed, {id, Id}) of
            {error, Err} -> ?ERROR("~p", [Err]);
            _ -> ok
            end
        end
    end, Updated),

    %Deleted
    DeletedIds = [get_value(id, get_value(Idx, OldBoards)) || Idx <- Deleted], 
    case DeletedIds of
    [] -> 
        ignore;
    _ ->
        ?INFO("deleted boards ~p from ~s", [DeletedIds, Dn]),
        case epgsql:delete(main, boards, {'in', id, DeletedIds}) of
        {error, Err} -> ?ERROR("~p", [Err]);
        _ -> ok
        end
    end.

boardn(Dn, Idx) ->
    iolist_to_binary([Dn, ",board=", integer_to_list(Idx)]).    


