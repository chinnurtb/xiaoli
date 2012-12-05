-module(mit_port).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([insert/3]).

-define(FIELDS, [
    name,
    alias,
    ifindex,
    biztype,
    iftype,
    ifspeed,
    ifphysaddr,
    ifoperstatus,
    ifmtu,
    ifdescr,
    ifadminstatus
]).

%ports table
% dn character varying(100) NOT NULL,
% name character varying(100),
% alias character varying(100),
% biz_type integer, -- 1:FE 2:GE 3:PON 4:POTS 5:DSL 6:E1 7:SDH 8:RF 9:VI 10:AGGREGATION
% node_id integer,
% category_id integer NOT NULL,
% vendor_id integer,
% board_id integer,
% ifindex integer,
% ifdescr character varying(100),
% iftype integer,
% ifphysaddr character varying(50),
% ifadminstatus integer,
% ifoperstatus integer,
% ifspeed integer,
% ifmtu integer,
% iflastchange timestamp without time zone,
% uplink_port integer,
% slot_no integer,
% port_no integer,
% downassuredbw integer,
% downmaximumbw integer,
% upassuredbw integer,
% upmaximumbw integer,
% temperature integer,
% received_power integer,
% led_power integer, -- 发光功率
% e_current integer, -- 电流
% voltage integer,
% telno character varying(50), -- 电压
% duplex integer,

insert(Tab, #node{dn=Dn, id=NodeId, categoryid=CatId, vendorid=VendorId}, Ports) ->
    NewIdxList = [IfIndex || {IfIndex, _} <- Ports],
    %load old ports
    {ok, Records} = epgsql:select(main, Tab, ?FIELDS, {node_id, NodeId}),
    OldPorts = [{get_value(ifindex, R), R} || R <- Records],
    OldIdxList = [IfIdx || {IfIdx, _} <- OldPorts],

    Now = {datetime, {date(), time()}},
	{Added, Updated, Deleted} = extlib:list_compare(NewIdxList, OldIdxList),
    %Added
    lists:foreach(fun(Idx) -> 
        Record = [{ifindex, Idx},
                 {dn, ifdn(Dn, Idx)},
                 {node_id, NodeId},
                 {vendor_id, VendorId},
                 {category_id, CatId}, 
                 {created_at, Now}
                 | get_value(Idx, Ports)],
        case epgsql:insert(main, Tab, Record) of
        {error, Err} -> 
            ?ERROR("~p", [Err]),
            ?ERROR("~p", [Record]);
        _ -> ok
        end
    end, Added),

    %Updated
    lists:foreach(fun(Idx) -> 
        NewPort = get_value(Idx, Ports),
        OldPort = get_value(Idx, OldPorts),
        Id = get_value(id, OldPort),
        Changed = NewPort -- OldPort,
        case Changed of
        [] -> 
            ignore;
        _ ->
            ?INFO("Port Changed: ~p", [Changed]),
            case epgsql:update(main, Tab, Changed, {id, Id}) of
            {error, Err} -> ?ERROR("~p", [Err]);
            _ -> ok
            end
        end
    end, Updated),

    %Deleted
    DeletedIds = [get_value(id, get_value(Idx, OldPorts)) || Idx <- Deleted], 
    case DeletedIds of
    [] -> 
        ignore;
    _ ->
        ?INFO("deleted ports ~p from ~s", [DeletedIds, Dn]),
        case epgsql:delete(main, Tab, {'in', id, DeletedIds}) of
        {error, Err} -> ?ERROR("~p", [Err]);
        _ -> ok
        end
    end.

ifdn(Dn, IfIndex) ->
    iolist_to_binary([Dn, ",ifindex=", integer_to_list(IfIndex)]).

