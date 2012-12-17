-module(mit_port).

-include("mit.hrl").

-import(proplists, [get_value/2]).

-export([insert/3]).

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

insert(Tab, #mit_node{dn=Dn, id=NodeId, categoryid=CatId, vendorid=VendorId}, Ports) ->
    {ok, Records} = epgsql:select(main, Tab, {node_id, NodeId}),
    OldPorts = [{get_value(ifindex, R), R} || R <- Records],
    DateTime = {date(), time()},
    NewFun = fun(Idx, NewPort) ->
        [{ifindex, Idx},
         {dn, ifdn(Dn, Idx)},
         {node_id, NodeId},
         {vendor_id, VendorId},
         {category_id, CatId}, 
         {created_at, {datetime, DateTime}}
         | NewPort]
    end,
    mit_db:merge(Tab, Ports, OldPorts, NewFun).

ifdn(Dn, IfIndex) ->
    iolist_to_binary([Dn, ",ifindex=", integer_to_list(IfIndex)]).

