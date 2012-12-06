%%%----------------------------------------------------------------------
%%% File    : mit_node.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : MIT node
%%% Created : 10 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_node).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([cat/1, load/1, update/3]).

-define(NODE_SQL,
    "select t.id, t.dn, t.name, t.alias, t.addr as ip, t.area_id as areaid, "
    "t.sysoid, t.snmp_comm as community, t.snmp_wcomm as write_community, "
    "t.oididx, t.timeperiod_id, "
    "t1.name as category, t1.id as categoryid, "
    "t2.name as vendor, t2.id as vendorid, "
    "t3.name as model, t3.id as modelid, "
    "t4.dn as area, "
    "t4.cityid, "
    "t5.name as city "
    "from nodes t "
    "left join categories t1 on t1.id = t.category_id "
    "left join vendors t2 on t2.id = t.vendor_id "
    "left join models t3 on t3.id = t.model_id "
    "left join areas t4 on t4.id = t.area_id "
    "left join areas t5 on t5.id = t4.cityid").

cat(DN) when is_binary(DN) ->
    [H|_] = lists:reverse(string:tokens(binary_to_list(DN), ",")),
    [Cat|_] = string:tokens(H, "="),
    list_to_atom(Cat).

load(Cat) when is_atom(Cat) ->
    Where = iolist_to_binary([" where t1.name = '", atom_to_list(Cat), "';"]),
    result(epgsql:squery(main, iolist_to_binary([?NODE_SQL, Where])));

load(Id) when is_integer(Id) ->
    Where = iolist_to_binary([" where t.id = ", integer_to_list(Id), ";"]),
    result(epgsql:squery(main, iolist_to_binary([?NODE_SQL, Where])));

load(DN) when is_list(DN) or is_binary(DN) ->
    Where = iolist_to_binary([" where t.dn = '", DN, "';"]),
    result(epgsql:squery(main, iolist_to_binary([?NODE_SQL, Where]))).

result({ok, Records}) ->
    {ok, [record(R) || R <- Records]};

result({error, Error}) ->
    {error, Error}.
    
record(N) ->
    #node{dn = get_value(dn, N),
        id = get_value(id, N),
        ip = get_value(ip, N),
        category = atom(get_value(category, N)),
        categoryid = get_value(categoryid, N),
        vendor = get_value(vendor, N),
        vendorid = get_value(vendorid, N),
        model = get_value(model, N),
        modelid = get_value(modelid, N),
        parent = get_value(parent, N),
        city = get_value(city, N),
        cityid = get_value(cityid, N),
        area = get_value(area, N),
        areaid = get_value(areaid, N),
        name = get_value(name, N),
        alias = get_value(alias, N),
        tpid = get_value(timeperiod_id, N),
        sysoid = get_value(sysoid, N),
        community = get_value(community, N),
        write_community = get_value(write_community, N),
        oididx = get_value(oididx, N)}.

update(Tab, Dn, Attrs) ->
    ?INFO("update: ~s, attrs: ~n~p", [Dn, Attrs]),
    case epgsql:update(main, Tab, Attrs, {dn, Dn}) of
    {error, Err} ->
        ?ERROR("update error: ~p", [Err]);
    _ ->
        ok
    end.

atom(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B)).

