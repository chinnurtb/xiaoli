%%%----------------------------------------------------------------------
%%% File    : mit_sw.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit switches 
%%% Created : 18 Arg 2011
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_sw).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-mit_boot_load({sw, load, "loading switch...", site}).

-export([entry/1, load/0, read/1, update/2]).

%FIXME: should use this feature
%-record(state, {is_cn_discovered = false}).

-define(SQL, "select `t1`.`id` AS `id`,`t1`.`sw_en` AS `cn`,`t1`.`sw_cn` AS `text`,`t1`.`ip` AS `ip`,"
        "`t1`.`sysoid` AS `sysoid`,"
        "`t1`.`manager` AS `manager`,"
        "_utf8'/top/ossIpDevice/ossIpSwitch' AS `objectClass`,`t1`.`sw_dn` AS `dn`,`swtype`.`code_name` AS `type`,"
        "`devicemanu`.`code_name` AS `vendor`,`t1`.`managed_state` AS `managed_state`,"
        "`t1`.`discovery_state` AS `discovery_state`,`t1`.`snmp_r` AS `snmpCommunity`,`t1`.`snmp_w` AS `snmpWriteCommunity`,"
        "(case when (`t1`.`sw_state` in (0,1,2,3)) then `t1`.`sw_state` else 0 end) AS `oper_state`,"
        "`t1`.`extra` AS `extra`,NULL AS `ap_fit`,NULL AS `sw_dn`,NULL AS `slave_ac_ip`,NULL AS `slave_snmp_r`" 
        "from `mit_switchs` `t1`  "
        "left join `dic_codes` `devicemanu` on `t1`.`device_manu` = `devicemanu`.`id` "
        "left join `dic_codes` `swtype` on `t1`.`sw_type` = `swtype`.`id` ").

load() ->
	emysql:update(mit_switchs, [{manager, undefined}]),
	emysql:update(mit_switchs, [{discovery_state, 1}], {discovery_state, 2}),
    {ok, Switches} = emysql:sqlquery([?SQL, ";"]),
	Entries = [entry(Sw) || Sw <- Switches],
	?INFO("cache ~p switchs.", [length(Entries)]),
	StoreFun = fun(Entry) ->
		#entry{dn = Dn, ip = Ip} = Entry,
		mnesia:write(Entry),
		mnesia:write(#ip2dn{ip=Ip, dn=Dn})
	end,
	mnesia:sync_dirty(fun lists:foreach/2, [StoreFun, Entries]).

entry(Sw) ->
	mit:entry(sw, Sw).

read({dn, Dn}) -> 
    emysql:sqlquery([?SQL, "where t1.sw_dn = '", Dn, "';"]).

update(Dn, Attrs) ->
    case mit:lookup(Dn) of
    {ok, #entry{uid = {sw, Id}}} ->
		Attrs1 = lists:keydelete(ap_cn, 1, Attrs),
		Attrs2 = mit_dict:transform(sw, Attrs1),
        emysql:update(mit_switchs, Attrs2, {id, Id});
	{ok, Entry} ->
		?ERROR("entry is not sw: ~p", [Entry]);
    false ->
        ?ERROR("cannot find ac ~p", [Dn])
    end.

