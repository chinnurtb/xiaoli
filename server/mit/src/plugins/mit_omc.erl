%%%----------------------------------------------------------------------
%%% File    : mit_omc.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit omc management
%%% Created : 18 Arg 2011
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2011, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_omc).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-mit_boot_load({omc, load, "loading omc...", site}).

-export([load/0, entry/1, read/1, update/2]).

-define(SQL, "select `t1`.`id` AS `id`,`t1`.`omc_dn` AS `dn`,`t1`.`omc_cn` AS `text`,`t1`.`ip` AS `ip`,"
        "_utf8'/top/ossIpDevice/ossWirelessOperationsMaintenanceCenter' AS `objectClass`,`t1`.`omc_en` AS `cn`,"
        "`omctype`.`code_name` AS `type`,`t1`.`vendor` AS `vendor`,`t1`.`managed_state` AS `managed_state`,"
        "`t1`.`discovery_state` AS `discovery_state`,`t1`.`snmp_r` AS `snmpCommunity`,"
        "`t1`.`snmp_w` AS `snmpWriteCommunity`,"
        "`t1`.`sysoid` AS `sysoid`,"
        "`t1`.`manager` AS `manager`,"
        "(case when (`t1`.`omc_state` in (0,1,2,3)) then `t1`.`omc_state` else 0 end) AS `oper_state`,`t1`.`extra` AS `extra`,"
        "NULL AS `ap_fit`,NULL AS `sw_dn`,`t1`.`ip` AS `slave_omc_ip`,`t1`.`snmp_r` AS `slave_snmp_r` "
        "from `mit_omcs` `t1` "
        "left join `dic_codes` `omctype` on `t1`.`omc_type` = `omctype`.`id` "
        "left join `avail_devices` `avail` on `t1`.`omc_dn` = `avail`.`dn`").

load() ->
    {ok, Omcs} = emysql:sqlquery([?SQL, ";"]),
    Entries = [entry(Omc) || Omc <-  Omcs],
	?INFO("cache ~p omcs.", [length(Entries)]),
	StoreFun = fun(#entry{dn = Dn, ip = Ip} = Entry) -> 
		mnesia:write(Entry),
		mnesia:write(#ip2dn{ip=Ip, dn=Dn})
	end,
	mnesia:sync_dirty(fun lists:foreach/2, [StoreFun, Entries]).

entry(Omc) ->
	mit:entry(omc, Omc).

read({dn, Dn}) -> 
    emysql:sqlquery([?SQL, "where t1.omc_dn = '", Dn, "';"]).

update(Dn, Attrs) ->
    case mit:lookup(Dn) of
    {ok, #entry{uid = {omc, Id}}} ->
		Updated = {updated_at, {datetime, calendar:local_time()}},
        emysql:update(mit_omcs, [Updated|filter(Attrs)], {id, Id});
	{ok, Entry} ->
		?ERROR("entry is not omc: ~p", [Entry]);
    {false, _} ->
        ?ERROR("cannot find omc; ~p", [Dn])
    end.

update(aclist, OmcDn, AcInfos) ->
	case mit:lookup(OmcDn) of
	{ok, #entry{uid = {omc, Omc_id}}} ->
		lists:foreach(fun(AcInfo) ->
			{ip,AcIp} = lists:keyfind(ip, 1,AcInfo),
			{ip,AcIp} = lists:keyfind(ip, 1,AcInfo),
			case mit:lookup({ip, AcIp}) of 
			{ok, #entry{dn = AcDn}} ->
				AcInfo1 = lists:keydelete(ip, 1, AcInfo),
				mit_ac:update(AcDn, [{omc_id,Omc_id}] ++ AcInfo1);
			{false, {ip, AcIp}} ->
				?ERROR("not find AcIp: ~p~n", [AcIp])
			end
		end, AcInfos);
	{ok, #entry{uid = Uid}} ->
		?ERROR("entry is not omcac: ~p~n~p", [Uid, OmcDn]);
	{false, _} ->
		?ERROR(" ~p not found", [OmcDn])
	end;

update(omintfs, Dn, Intfs) ->
	case mit:lookup(Dn) of
	{ok, _Entry} ->
		lists:foreach(fun({AcIp, Intf}) ->
			{ifIndex, IfIndex} = lists:keyfind(ifIndex, 1,Intf),
			case mit:lookup({ip, AcIp}) of 
			{ok, #entry{dn = AcDn}} ->
				IfDn = mit_dn:ifdn(IfIndex, AcDn),
				case mit_intf:lookup(IfDn) of
					{ok, _} ->
						mit_intf:update(IfDn, Intf);
					{false, _} ->
						mit_intf:insert(IfDn, Intf)
				end;
			{false, {ip, AcIp}} ->
				?ERROR("not find AcIp: ~p~n", [AcIp])
			end
			end, Intfs);
	{false, _} ->
		?ERROR(" ~p not found", [Dn])
	end.

filter(Attrs) ->
	filter(Attrs, []).
filter([{ap_cn, _}|T], Acc) ->
	filter(T, Acc);
filter([{omc_cn, _}|T], Acc) ->
	filter(T, Acc);
filter([{sysdescr, _}|T], Acc) ->
	filter(T, Acc);
filter([{type, _}|T], Acc) ->
	filter(T, Acc);
filter([Attr|T], Acc) ->
	filter(T, [Attr|Acc]).

