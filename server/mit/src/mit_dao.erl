%%%----------------------------------------------------------------------
%%% File    : mit_dao.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : MIT change callback.
%%% Created : 10 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_dao).

-include("mit.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([all_nodes/0,
        one_node/1,
        all_areas/0,
        one_area/1]).

-define(AREA_SQL,
    "select t.id, t.rdn, t.name, t.alias, t.area_type as type, "
    "t2.rdn as parent "
    "from areas t "
    "left join areas t2 on t.parent_id = t2.id ").

-define(NODE_SQL,
    "select t.id, t.rdn, t.name, t.alias, t.addr as ip, t.area_id, "
    "t.sysoid, t.snmp_comm, t.snmp_wcomm, t.oid_idx, t.timeperiod_id, "
    "t1.name as category, "
    "t2.name as vendor, "
    "t3.name as model, "
    "t4.rdn as area, "
    "t4.cityid, "
    "t5.name as city "
    "from nodes t "
    "left join categories t1 on t1.id = t.category_id "
    "left join vendors t2 on t2.id = t.vendor_id "
    "left join models t3 on t3.id = t.model_id "
    "left join areas t4 on t4.id = t.area_id "
    "left join areas t5 on t5.id = t4.cityid ").

all_areas() ->
    {ok, Areas} = epgsql:squery(main, ?AREA_SQL++";"),
    {ok, [area_record(A) || A <- Areas]}.

one_area(Dn) when is_binary(Dn) or is_list(Dn) ->
    select_area(iolist_to_binary(["where t.rdn = '", Dn ,"';"]));

one_area(Id) when is_integer(Id) ->
    select_area(iolist_to_binary(["where t.id = ", integer_to_list(Id), ";"])).

select_area(Where) ->
    case epgsql:squery(main, list_to_binary([?AREA_SQL, Where])) of
    {ok, [A]} -> {ok, area_record(A)};
    {ok, []} -> {error, notfound};
    {error, Err} -> {error, Err}
    end.

all_nodes() ->
    {ok, Nodes} = epgsql:squery(main, ?NODE_SQL++";"),
    {ok, [node_record(N) || N <- Nodes]}.

one_node(Dn) when is_binary(Dn) or is_list(Dn) ->
    select_node(iolist_to_binary(["where t.rdn = '", Dn ,"';"]));

one_node(Id) when is_integer(Id) ->
    select_node(iolist_to_binary(["where t.id = ", integer_to_list(Id), ";"])).

select_node(Where) ->
    case epgsql:squery(main, iolist_to_binary([?NODE_SQL, Where])) of
    {ok, [N]} -> {ok, node_record(N)};
    {ok, []} -> {error, notfound};
    {error, Err} -> {error, Err}
    end.
    
area_record(A) ->
    %TODO: RDN as DN 
    #area{dn = get_value(rdn, A),
        id = get_value(id, A),
        cityid = get_value(cityid, A),
        parent = get_value(parent, A),
        name = get_value(name, A),
        alias = get_value(alias, A),
        type = get_value(type, A)}.

node_record(N) ->
    %TODO: rdn as dn
    #node{dn = get_value(rdn, N),
        id = get_value(id, N),
        ip = get_value(ip, N),
        category= get_value(category, N),
        parent = get_value(parent, N),
        city = get_value(city, N),
        cityid = get_value(cityid, N),
        name = get_value(name, N),
        alias = get_value(alias, N),
        tpid = get_value(timeperiod_id, N),
        attrs = N}.

