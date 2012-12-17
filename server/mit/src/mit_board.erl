-module(mit_board).

-include("mit.hrl").

-import(proplists, [get_value/2]).

-export([insert/2]).

insert(_, []) ->
    ingore;

insert(#mit_node{dn=Dn, id=NodeId, categoryid=CatId, vendorid=VendorId}, Boards) ->
    {ok, Records} = epgsql:select(main, boards, {node_id, NodeId}),
    OldBoards = [{get_value(boardidx, R), R} || R <- Records],
    DateTime = {date(), time()},
    NewFun = fun(Idx, Board) ->
        [{dn, boardn(Dn, Idx)},
        {node_id, NodeId},
        {vendor_id, VendorId},
        {category_id, CatId}, 
        {boardidx, Idx},
        {created_at, {datetime, DateTime}} | Board]
    end,
    mit_db:merge(boards, Boards, OldBoards, NewFun).

boardn(Dn, Idx) ->
    iolist_to_binary([Dn, ",board=", integer_to_list(Idx)]).    

