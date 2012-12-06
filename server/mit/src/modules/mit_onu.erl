-module(mit_onu).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([load/0, load/1, update/2, add/1]).

-define(TAB, node_onus).

-define(PORT_TAB, port_onus).

load() ->
    mit_node:load(onu).

load(Dn) ->
    mit_node:load(Dn).

update(Dn, Attrs) ->
    mit_node:update(?TAB, Dn, Attrs).

add({_, _Dn, []}) ->
    ignore;

add({nodes, Dn, Onus}) ->
    {ok, Node} = mit:lookup(Dn),
    insert(Node, Onus);

add({ports, Dn, Ports}) ->
    {ok, Node} = mit:lookup(Dn),
    mit_port:insert(?PORT_TAB, Node, Ports).

insert(#node{id=OltId, areaid=AreaId, vendorid=VdId}, Onus) ->
    NewIdxList = [get_value(onuidx, Onu) || Onu <- Onus],
    {ok, Records} = epgsql:select(main, node_onus, {oltid, OltId}),
    OldOnus = [{get_value(onuidx, R), R} || R <- Records],
    OldIdxList = [Idx || {Idx, _} <- OldOnus],

    Now = {datetime, {date(), time()}},
	{Added, Updated, _Deleted} = extlib:list_compare(NewIdxList, OldIdxList),

    %Added
    lists:foreach(fun(Idx) -> 
        Record = [{oltid, OltId},
                {controller_id, OltId}, 
                {category_id, 21},
                {vendor_id, VdId},
                {area_id, AreaId},
                {created_at, Now} | get_value(Idx, Onus)],
            Res = epgsql:insert(main, node_onus, Record),
            case Res of
            {error, Err} -> 
                ?ERROR("~p", [Err]),
                ?ERROR("~p", [Record]);
            _ ->
                ok
            end
    end, Added),

    %Updated
    lists:foreach(fun(Idx) -> 
        NewOnu = get_value(Idx, Onus),
        OldOnu = get_value(Idx, OldOnus),
        Id = get_value(id, OldOnu),
        Changed = NewOnu -- OldOnu,
        case Changed of
        [] ->
            ignore;
        _ ->
            ?INFO("Port Changed: ~p", [Changed]),
            case epgsql:update(main, node_onus, Changed, {id, Id}) of
            {error, Err} -> ?ERROR("~p", [Err]);
            _ -> ok
            end
        end
    end, Updated).

