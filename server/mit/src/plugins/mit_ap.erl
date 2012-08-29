%%%----------------------------------------------------------------------
%%% File    : mit_ap.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit ap
%%% Created : 21 Feb 2008
%%% Updated : 18 Arg 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_ap).

-author('ery.lee@gmail.com').

-import(proplists, [get_value/2, get_value/3]).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-mit_boot_load({ap, load, "loading ap", ac}).

-export([load/0, entry/1, read/1, update/3]). 

-define(FIELDS, [id,
				 ac_id,
				 ap_cn,
				 ap_dn,
				 ap_en,
				 device_manu,
				 ap_type,
				 mac,
				 ip,
				 mask,
				 managed_state,
				 extra,
				 serial_no,
				 ap_software_rer]).

-define(SQL, "select `t1`.`id` AS `id`, `t1`.`ac_id` AS `ac_id`, `t1`.`ap_en` AS `cn`,`t1`.`ap_cn` AS `text`,(case `t1`.`ap_fit` when 1 then `mit_acs`.`ip` "
        "else `t1`.`ip` end) AS `ip`,_utf8'/top/ossIpDevice/ossWirelessAccessPoint' AS `objectClass`,"
        "`t1`.`ap_dn` AS `dn`,`aptype`.`code_name` AS `type`,`devicemanu`.`code_name` AS `vendor`,"
        "`t1`.`managed_state` AS `managed_state`,`t1`.`discovery_state` AS `discovery_state`,"
        "`t1`.`manager` AS `manager`,"
		 "(case `t1`.`ap_fit` when 1 then `mit_acs`.`snmp_r` else `t1`.`snmp_r` end) AS `snmpCommunity`, "
        "(case `t1`.`ap_fit` when 1 then `mit_acs`.`snmp_w` else `t1`.`snmp_w` end) AS `snmpWriteCommunity`,"
        "(case `t1`.`ap_fit` when 1 then `mit_acs`.`sysoid` else `t1`.`sysoid` end) AS `sysoid`, "
        "(case when (`t1`.`ap_state` in (0,1,2,3)) then `t1`.`ap_state` else 0 end) AS `oper_state`,`t1`.`extra` AS `extra`, "
        "`t1`.`ap_fit` AS `ap_fit`,`mit_links`.`end_z` AS `sw_dn`,`slaveac`.`ip` AS `slave_ac_ip`,`slaveac`.`snmp_r` AS `slave_snmp_r` "
        "from `mit_aps` `t1` left join `mit_links` on `t1`.`ap_dn` = `mit_links`.`end_a` "
        "left join `dic_codes` `devicemanu` on `t1`.`device_manu` = `devicemanu`.`id` " 
        "left join `dic_codes` `aptype` on `t1`.`ap_type` = `aptype`.`id` "
        "left join `mit_acs` on `t1`.`ac_id` = `mit_acs`.`id` "
        "left join `mit_acs` `slaveac` on`mit_acs`.`slave_id` = `slaveac`.`id` ").

load() ->
	emysql:update(mit_aps, [{manager, undefined}]),
   emysql:update(mit_aps, [{discovery_state, 1}], {discovery_state, 9}),
   {ok, Aps} = emysql:sqlquery([?SQL, ";"]),
	?INFO("cache ~p aps", [length(Aps)]),
	Store = fun(Ap) -> mnesia:write(entry(Ap)) end,
	mnesia:sync_dirty(fun lists:foreach/2, [Store, Aps]).

entry(Ap) ->
	{value, ApFit} = dataset:get_value(ap_fit, Ap),
	mit:entry(category(ApFit), Ap).

%fitAP:1，fatAP:2		
category(1) ->
	fitap;

category(2) ->
	fatap.

read({dn, Dn}) ->
    emysql:sqlquery([?SQL, "where t1.ap_dn = '", Dn, "';"]).

update(fatap, Dn, Attrs) ->
    Attrs1 = lists:keydelete(ap_cn, 1, Attrs),
    UpdatedAt = {datetime, {date(), time()}},
    case emysql:update(mit_aps, [{updated_at, UpdatedAt}|Attrs1], {ap_dn, Dn}) of
    {error, Reason}-> 
		?ERROR("fatap attrs: ~p", [Attrs]),
       ?ERROR("failed to update ~p: ~p", [Dn, Reason]);
	_ -> ok
    end;

update(fitap, Dn, Attrs) ->
	Attrs1 = 
	case is_fitap_cn_discovered() of
	true -> Attrs;
	false -> lists:keydelete(ap_cn, 1, Attrs)
	end,
	case emysql:update(mit_aps, Attrs1, {ap_dn, Dn}) of
    {error, Reason}->
		?ERROR("fitap attrs: ~p", [Attrs]),
       ?ERROR("failed to update ~p: ~p", [Dn, Reason]);
	_ -> ok
	end;

update(fitaps, AcDn, []) ->
	?ERROR("empty fitaps for ~p", [AcDn]);

%should compare with db.
update(fitaps, AcDn, FitAps) ->
    case mit:lookup(AcDn) of
    {ok, #entry{uid = {ac, AcId}} = AcEntry} ->
		%fitaps read from ac.
		ApsOfAc = transform(AcEntry, FitAps),
		EnInAc = [En || {En, _} <- ApsOfAc],

		%fitaps in database.
		ApsInDb = find_aps_in_db(AcId, EnInAc),
		EnInDb = [En || {En, _} <- ApsInDb],

		%list compare
		Added = EnInAc -- EnInDb,
		Deleted = EnInDb -- EnInAc,
		Updated = EnInDb -- Deleted,

		%maybe_offline(AcEntry, Deleted),
		add(AcEntry, Added, ApsOfAc),
		update(AcId, Updated, ApsInDb, ApsOfAc);
	{ok, Entry} ->
		?CRITICAL("entry is not ac: ~p", [Entry]);
    {false, _} ->
        ?ERROR("cannot find ac ~p", [AcDn])
    end.

find_aps_in_db(AcId, EnInAc) ->
	%%find aps of the same ac
    {ok, ApsInDb} = emysql:select({mit_aps, ?FIELDS, {ac_id, AcId}}),
    EnInDb = [get_value(ap_en, Ap) || Ap <- ApsInDb],
	%find aps that not belongs to this ac
	%FIXME later: 新增AC发现，会导致大批量的比对
	LeftEnList = EnInAc -- EnInDb,
	LeftAps = 
	case LeftEnList of
	[] -> 
		[];
	_ ->
		Select = {mit_aps, ?FIELDS, {'in', ap_en, LeftEnList}},
		{ok, Aps} = emysql:select(Select, length(LeftEnList)),
		Aps
	end,
    [{get_value(ap_en, Ap), Ap} || Ap <- LeftAps ++ ApsInDb].

add(AcEntry, Added, FitAps) ->
	#entry{dn = AcDn, uid = {ac, AcId}} = AcEntry,
	CreatedAt = UpdatedAt = {datetime, {date(), time()}},
	Store = fun(ApEn) ->
        FitAp = proplists:get_value(ApEn, FitAps),
        State = dataset:get_value(managed_state, FitAp, 1),
        case emysql:select(mit_fit_aps, [id], {ap_en, ApEn}) of %find in mit_fit_aps
        {ok, [_OldFiAp|_]} -> %exist
            if
            State > 0 -> % online
				Record = [{ac_id, AcId},{updated_at, UpdatedAt}|FitAp],
                case emysql:update(mit_fit_aps, Record, {ap_en, ApEn}) of
				{error, Reason} -> ?ERROR("mysql error: ~p", [Reason]);
				_ -> ok
				end;
            true ->
                ignore
            end;
        {ok, []} -> %not exist
			ApDn = list_to_binary(["cn=", ApEn, ",", mit_dn:bdn(AcDn)]),
			Record = [{ap_dn, ApDn}, {ac_id, AcId}, 
					  {created_at, CreatedAt},
					  {updated_at, UpdatedAt} | FitAp],
            case emysql:insert(mit_fit_aps, Record) of
			{error, Reason} -> ?ERROR("mysql error: ~p", [Reason]);
			_ -> ok
			end;
        {error, Reason} ->
            ?ERROR("mysql error: ~p ", [Reason])
        end
	end,
    lists:foreach(Store, Added).


update(AcId, Updated, ApsInDb, FitAps) ->
    lists:foreach(fun(ApEn) -> 
        {value, OldAp} = dataset:get_value(ApEn, ApsInDb),
        {value, OldAcId} = dataset:get_value(ac_id, OldAp),
        {value, NewAp} = dataset:get_value(ApEn, FitAps),
		{value, NewState} = dataset:get_value(managed_state, NewAp, 1),
		case NewState of
		1 -> %online
            UpdatedAt = {datetime, {date(), time()}},
			NewAp1 = rmattrs([ap_dn, managed_state], NewAp),
			UpdatedAttrs = [{ac_id, AcId}|NewAp1] -- OldAp,
            Attrs = find_update_attrs(UpdatedAttrs),
            Attrs1 = 
            case is_fitap_cn_discovered() of
            true -> Attrs;
            false -> dataset:key_delete(ap_cn, Attrs)
            end,
			if
			length(Attrs1) > 0 ->
				Res = emysql:update(mit_aps, [{updated_at, UpdatedAt}|Attrs1], {ap_en, ApEn}),
				?INFO("ap updated: ~p, ~p", [Res, Attrs1]),
				case OldAcId == AcId of
				false -> % ac changed.
					mit_journal:log(fitap, ApEn, update, [{ac_id, OldAcId, AcId}]);
				true -> 
					ignore
				end;
			true ->
				ok
			end;
		_ -> %offline
			%don't update attributes of offline ap
			ignore
		end
	end, Updated).

find_update_attrs(FitAp) ->
    find_update_attrs(FitAp, []).

find_update_attrs([{ap_cn, ApCn}|T], Acc) ->
    find_update_attrs(T, [{ap_cn, ApCn}|Acc]);
find_update_attrs([{serial_no, Serial}|T], Acc) ->
    find_update_attrs(T, [{serial_no, Serial}|Acc]);
find_update_attrs([{extra, Extra}|T], Acc) ->
    find_update_attrs(T, [{extra, Extra}|Acc]);
find_update_attrs([{ip, Ip}|T], Acc) ->
    find_update_attrs(T, [{ip, Ip}|Acc]);
find_update_attrs([{mac, Mac}|T], Acc) ->
    find_update_attrs(T, [{mac, Mac}|Acc]);
find_update_attrs([{mask, Mask}|T], Acc) ->
    find_update_attrs(T, [{mask, Mask}|Acc]);
find_update_attrs([{ap_software_rer, SoftVer}|T], Acc) ->
    find_update_attrs(T, [{ap_software_rer, SoftVer}|Acc]);
find_update_attrs([{device_manu, Manu}|T], Acc) ->
    find_update_attrs(T, [{device_manu, Manu}|Acc]);
find_update_attrs([{ap_type, ApType}|T], Acc) ->
    find_update_attrs(T, [{ap_type, ApType}|Acc]);
find_update_attrs([_|T], Acc) ->
    find_update_attrs(T, Acc);
find_update_attrs([], Acc) ->
    Acc.

rmattrs(Attrs, Record) ->
	lists:foldl(fun(Attr, R) -> 
		lists:keydelete(Attr, 1, R)
	end, Record, Attrs).

transform(#entry{attrs = Ac}, FitAps)->
	Vendor = proplists:get_value(vendor, Ac, ""),
	[{En, mit_dict:transform(ap, [{vendor, Vendor}|Ap])}
		|| {En, Ap} <- FitAps].
	
is_fitap_cn_discovered() ->
    case application:get_env(is_fitap_cn_discovered) of
    {ok, Val} -> Val;
    undefined -> false
    end.

