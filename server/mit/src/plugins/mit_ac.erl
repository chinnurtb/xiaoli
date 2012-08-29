%%%----------------------------------------------------------------------
%%% File    : mit_ac.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit ac management
%%% Created : 18 Arg 2011
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2011, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_ac).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-mit_boot_load({ac, load, "loading ac...", site}).

-export([load/0, entry/1, read/1, update/2]). 

-define(SQL, "select `t1`.`id` AS `id`, `t1`.`omc_id` AS `omc_id`, `t1`.`ac_en` AS `cn`,`t1`.`ac_cn` AS `text`,"
		"`t1`.`ip` AS `ip`,_utf8'/top/ossIpDevice/ossWirelessAccessController' AS `objectClass`,`t1`.`ac_dn` AS `dn`,"
        "`actype`.`code_name` AS `type`,`devicemanu`.`code_name` AS `vendor`,`t1`.`managed_state` AS `managed_state`,"
        "`t1`.`discovery_state` AS `discovery_state`,`t1`.`snmp_r` AS `snmpCommunity`,"
        "`t1`.`snmp_w` AS `snmpWriteCommunity`,"
        "`t1`.`sysoid` AS `sysoid`,"
        "`t1`.`software_ver` AS `software_ver`,"
        "`t1`.`manager` AS `manager`,"
        "(case when (`t1`.`ac_state` in (0,1,2,3)) then `t1`.`ac_state` else 0 end) AS `oper_state`,`t1`.`extra` AS `extra`,"
        "NULL AS `ap_fit`,NULL AS `sw_dn`,`mit_acs`.`ip` AS `slave_ac_ip`,`mit_acs`.`snmp_r` AS `slave_snmp_r` "
        "from `mit_acs` `t1` "
        "left join `dic_codes` `devicemanu` on `t1`.`device_manu` = `devicemanu`.`id` "
        "left join `dic_codes` `actype` on `t1`.`ac_type` = `actype`.`id` "
        "left join `mit_acs` on `t1`.`slave_id` = `mit_acs`.`id` "
		"left join `mit_omcs` on `t1`.`omc_id` = `mit_omcs`.`id` ").

entry(Ac) ->
	mit:entry(ac, Ac).

load() ->
	emysql:update(mit_acs, [{manager, undefined}]),
	emysql:update(mit_acs, [{discovery_state, 1}], {discovery_state, 2}),
   {ok, Acs} = emysql:sqlquery([?SQL, ";"]),
	Entries = [entry(Ac) || Ac <- Acs],
	StoreFun = fun(Entry) ->
		#entry{dn = Dn, ip = Ip} = Entry,
		mnesia:write(Entry),
		mnesia:write(#ip2dn{ip=Ip, dn=Dn})
	end,
	mnesia:sync_dirty(fun lists:foreach/2, [StoreFun, Entries]).

read({dn, Dn}) ->
    emysql:sqlquery([?SQL, "where t1.ac_dn = '", Dn, "';"]);

read({ip, Ip}) ->
    emysql:sqlquery([?SQL, "where t1.ip = '", Ip, "';"]).

update(Dn, Attrs) ->
    case mit:lookup(Dn) of
    {ok, #entry{uid = {ac, Id}}} ->
		Attrs1 = 
		lists:foldl(fun(Key, List) -> 
			lists:keydelete(Key, 1, List)
		end, Attrs, [ap_cn, ac_cn]),
		Attrs2 = mit_dict:transform(ac, Attrs1),
        UpdatedAt = {updated_at, {datetime, calendar:local_time()}},
        case emysql:update(mit_acs, [UpdatedAt|Attrs2], {id, Id}) of
        {error, Error} -> ?ERROR("~p", [Error]);
        _ -> ok
        end;
	{ok, Entry} ->
		?ERROR("entry is not ac: ~p", [Entry]);
    {false, _} ->
        ?ERROR("cannot find ac; ~p", [Dn])
    end.

