%%%----------------------------------------------------------------------
%%% File    : mib_mapper.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mib record mapping
%%% Created : 27 Jun 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mib_mapper).

-include_lib("elog/include/elog.hrl").

-import(lists, [keyreplace/4]).

-import(extbif, [binary_join/2]).

-import(proplists, [get_value/2, get_value/3]).

-import(mib_formatter, [mac/1, ip/1, s/1, i/1]).

-export([null_mapping/1,
	noindex_mapping/1,
	apstatus_mapping_fun/1,
        apavail_mapping_fun/1,
        ap_mapping_fun/1,
	omc_ac_mapping_fun/0,
	ac_mapping_fun/0,
        ac_perf_mapping/1,
	bras_mapping_fun/0,
	fatap_mapping/1,
        intf_mapping_fun/1, 
        intf_mapping/1, 
        pool_perf_mapping/1,
        portal_mapping/1,
        portal_reduce/1,
        radius_mapping/1,
        radius_reduce/1,
        ippool_mapping/1,
        ippool_reduce/1,
        radio_mapping_fun/1,
	ssid_mapping_fun/1,
	sta_mapping/1,
	sta_reduce/1,
	user_mapping/1,
        mapping/2,
	omc_ac_perf_mapping/1,
	online/2,
	ifdn/2
	]).

-define(OMC_AC_MAPPING, [
	{'$tableIndex', ip},
    {acName, ac_cn, s},
    {maxApLimit, maxApLimit},
    {softVersion, software_ver, s}
    ]).

-define(AC_MAPPING, [
    {acName, ac_cn, s},
    {maxApLimit, maxApLimit, s},
    {softVersion, software_ver, s},
    {sysModel, type, s},
    {asIPAddress, asIPAddress, s},
    {portalServerURL, portalServerURL, s},
    {acMac, mac, mac}]).

-define(AP_MAPPING, [
    {'$tableIndex', extra, stroid},           
    {apMac, [ap_en, mac], mac},
    {apState, managed_state},
    {apName, ap_cn, s},
    {apType, type, fitype},
    {apSerialNo, serial_no, s},
    {apSoftVersion, ap_software_rer, s},
    {apIp, ip, ip},
    {apMask, mask, ip}]).

-define(AVAIL_MAPPING, [
    {'$tableIndex', extra, stroid},           
    {apMac, [ap_en], mac},
    {apState, managed_state}]).

-define(BRAS_MAPPING, [
	{barsName, ac_cn, s},
%	{barsRestart, barsRestart},
%	{barsRunTime, barsRunTime},
	{barsSysDes, sysdescr, s},
%	{barsType, barsType},
%	{brasSerialNo,brasSerialNo},
%	{softDevVendor, softDevVendor},
%	{softName, softName},
	{softVersion, software_ver, s}
%	{sysTime, sysTime},
%	{vendor, vendor}
]).
%mib, online value
-define(ONLINES, [
    {h3c_25506, 5},
    {h3c, 5},
    {huawei, 8},
    {huawei_ma5200, 8},
    {huawei_s9306, 8},
    {huawei_ws6603, 8},
    {cisco, 1},
    {autelan, 5},
    {newpostcom, 5},
    {azalea, "Online"},
    {bell, 6},
    {boomsense, 1},
    {comba_ac2400, 1},
    {comba_ac2400_f1024, 5},
    {datang_mx400, 1},
    {datang_mx400ii, 5},
    {dfxl, 6},
    {guoren, 6},
    {guoren_sgr, 6},
    {hongxin_ac2400, 5},
    {hongxin_fhap2400, 1},
    {htec, 6},
    {htec_sx, 6},
    {jiesai_at3600, 1},
    {jiesai_at7605, 6},
    {mhah, 5},
    {moto, 1},
    {ruckus, 5},
    {ruckus_ZD6000, 6},
    {ruckus_ZD3000, 1},
    {sunnada, 6},
    {dafu, 6},
    {surekam, 5},
    {zoomtech, 6},
    {zte_w901, 1},
    {wifioss_zte_w901, 1},
    {zte_w908, 6},
    {wifioss_zte_W908, 6},
    {netgear, 5},
    {netgear_wfs128, 5},
    {bell_14823, 1},
	{bell_ac2400, 5},
    {zcom, 1},
    {ruijie, 1}]).

-define(FATAP_MAPPING, [
    {apMac, [ap_en, mac], mac},
    {apName, ap_cn, s},
    {apType, type, s},
    {apSerialNo, serial_no, s},
    {apSoftVersion, ap_software_rer, s},
%    {apIp, ip, ip},
    {apMask, mask, ip}]).

-define(RADIO_MAPPING, [
    {radioAdminStatus, radioAdminStatus, i},
    {radioOperStatus, radioOperStatus, i},
    {radioIndex, radioIndex, i},
    {radioPlus, radioPlus},
    {radioPeriod, radioPeriod, i},
    {radioDtim, radioDtim, i},
    {radioRts, radioRts, i},
    {radioSlice, radioSlice, i},
    {radioModel, radioModel, i},
    {radioStatus, radioStatus, i},
    {radioChannel, radioChannel, i},
    {radioPower, radioPower, i}
]).

-define(SSID_MAPPING, [
    {authenMode, authenMode},
    {dot11Auth, dot11Auth},
    {maxSimultUsers ,maxSimultUsers},
    {security, security},
    {securityCiphers, securityCiphers},
    {ssidEnabled, ssidEnabled},
    {ssidHidden, ssidHidden},
    {ssidName, cn},
    {staIsolate, staIsolate},
    {vlanId, vlanId}
]).

-define(STA_MAPPING, [
	{staIp, sta_ip, ip},
	{staMac, sta_mac, mac},
	{staRxBytes, sta_rxbytes, i},
	{staTxBytes, sta_txbytes, i},
	{staSsid, sta_ssid, s},
	{staNoiseRate, sta_noise_rate, i},
	{staRxFrame, sta_rxframe},
	{staTxFrame, sta_txframe},
	{staRssi, sta_rssi, i},
	{staChannel, sta_channel, s},
	{staVlan, sta_vlan},
	{'$timestamp', '$timestamp'}
]).

%radio model: mib -> mapping
-define(RADIO_MODS, [
    {huawei, [
        {1, "auto"},
        {2, "dot11b"},
        {3, "dot11a"},
        {4, "dot11g"},
        {5, "dot11n"},
        {6, "dot11bg"},
        {7, "dot11an"},
        {8, "dot11bgn"},
        {9, "dot11gn"}
    ]},
    {huawei_ma5200, [%TODO: GUESS
        {1, "auto"},
        {2, "dot11b"},
        {3, "dot11a"},
        {4, "dot11g"},
        {5, "dot11n"},
        {6, "dot11bg"},
        {7, "dot11an"},
        {8, "dot11bgn"},
        {9, "dot11gn"}
    ]},
    {h3c, [ %TODO: GUESS
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {h3c_25506, [ %TODO: GUESS
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"},
	{32, "dot11an"}
    ]},
    {h3c25506_fatap, [ %TODO: GUESS
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {7, "dot11abg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11b/gn"},
        {22, "dot11b/g/gn"},
        {33, "dot11an/a"},
        {55, "dot11a/b/g/an/gn"}
    ]},
     {h3c2011_fatap, [ %TODO: GUESS
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
   {cisco, [
        {1, "dot11b"},
        {2, "dot11a"},
        {3, "unknown"},
        {4, "uwb"},
        {5, "dot11g"},
        {6, "dot11n24"},
        {7, "dot11n5"}
    ]},
    {autelan, [
        {1, "dot11bg"},
        {2, "dot11g"},
        {3, "dot11b"},
        {4, "dot11a"}
    ]},
    {newpostcom, [
        {1, "dot11bg"},
        {2, "dot11g"},
        {3, "dot11b"},
        {4, "dot11a"}
    ]},
    {postcom_AP2000I_fatap, [
        {1, "dot11bg"},
        {2, "dot11g"},
        {3, "dot11b"},
        {4, "dot11a"}
    ]},

    {azalea, [
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {bell, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {bell_ac2400, [
        {1, "dot11bg"},
        {2, "dot11g"},
        {3, "dot11b"},
        {4, "dot11a"}
    ]},
    {boomsense, [
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"}
    ]},
    {comba_ac2400, [
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {comba_ac2400_f1024, [
        {1, "dot11bg"},
        {2, "dot11g"},
        {3, "dot11b"},
        {4, "dot11a"}
    ]},
    {datang_mx400, [
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {datang_mx400ii, [
        {1, "dot11bg"},
        {2, "dot11g"},
        {3, "dot11b"},
        {4, "dot11a"}
    ]},
    {dfxl, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {guoren, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {guoren_sgr, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {3, "dot11aAndb"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
   {guoren_18603_fatap, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]}, 
   {guoren_18603V3_fatap, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {guoren_1567ZA_fatap, [
        {1, "dotlla"},
        {2, "dot11b"},
        {3, "dot11bAndg"},
	{33, "dot11g"}
    ]},
    {guoren_1567EBI_fatap, [
        {1, "dotlla"},
        {2, "dot11b"},
        {3, "dot11bAndg"},
	{33, "dot11g"}
    ]},
    {hongxin_ac2400, [ %TODO: FIXME LATER
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11b/g"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11b/gn"},
        {22, "dot11b/g/gn"},
        {31, "dot11a/b/g/an/gn"}
    ]},
    {hongxin_fhap2400, [
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {htec, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {htec_sx, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {jiesai_at3600, [
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {jiesai_at7605, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {mhah, [
        {1, "dot11bg"},
        {2, "dot11g"},
        {3, "dot11b"},
        {4, "dot11a"}
    ]},
    {moto, [
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {ruckus, [
        {1, "dot11bg"},
        {2, "dot11g"},
        {3, "dot11b"},
        {4, "dot11a"}
    ]},
    {ruckus_ZD6000, [
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"}
   ]},
    {ruckus_ZD3000, [
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"}
   ]},
    {ruckus_ZF_fatap, [
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {8, "dot11an"},
        {16, "dot11gn"}
   ]},
    {ruckus_A8_fatap, [
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {8, "dot11an"},
        {16, "dot11gn"}
   ]},
    {sunnada, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {dafu, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {surekam, [
        {1, "dot11bg"},
        {2, "dot11g"},
        {3, "dot11b"},
        {4, "dot11a"}
    ]},
    {zoomtech, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {zte_w901, [
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {wifioss_zte_w901, [
        {0, "auto"},
        {1, "dot11a"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {zte_w908, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {wifioss_zte_W908, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]},
    {netgear_wg4526_fatap, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {3, "dot11g"}
    ]},
    {netgear_wg100_fatap, [
        {0, "dot11b"},
        {1, "dot11bAndg"}
    ]},
    {newpostcom_AP2000I_fatap, 
	[{2,"dot11g"}]},
    {sunnada_fatap, [
        {0, "auto"},
        {1, "dotlla"},
        {2, "dot11b"},
        {4, "dot11g"},
        {6, "dot11bg"},
        {8, "dot11an"},
        {16, "dot11gn"},
        {18, "dot11bgn"}
    ]}
]).




%used by check_fitap_avail
apstatus_mapping_fun(Mib) ->
    fun(Record) ->
        ApState = get_value(apState, Record),
        Online = online(Mib, ApState),
        [{managed_state, Online}]
    end.

apavail_mapping_fun(bell_14823) ->
    fun(Record) -> 
	{value, Mac} = dataset:get_value('$tableIndex', Record),
	ApMac = mac(Mac),
        Avail = mapping(Record, ?AVAIL_MAPPING),
        {value, Extra} = dataset:get_value(extra, Avail),
	{value, State} = dataset:get_value(managed_state, Avail),
        State1 = online(bell_14823, State),
        {ApMac, [{managed_state, State1}, {extra, Extra}]}
    end;

apavail_mapping_fun(Mib) ->
    fun(Record) -> 
        Avail = mapping(Record, ?AVAIL_MAPPING),
        {value, ApEn} = dataset:get_value(ap_en, Avail),
        {value, Extra} = dataset:get_value(extra, Avail),
	{value, State} = dataset:get_value(managed_state, Avail),
        State1 = online(Mib, State),
        {ApEn, [{managed_state, State1}, {extra, Extra}]}
    end.

ap_mapping_fun(bell_14823) ->
    fun(Record) -> 
	{value, TableIdx} = dataset:get_value('$tableIndex', Record),
	ApMac = mac(TableIdx),
        AP = [{ap_en, ApMac}, {mac, ApMac} | mapping(Record, ?AP_MAPPING)],
        {value, ApEn} = dataset:get_value(ap_en, AP),
		{value, State} = dataset:get_value(managed_state, AP),
        State1 = online(bell_14823, State),
        {ApEn, keyreplace(managed_state, 1, AP, {managed_state, State1})}
    end;


ap_mapping_fun(cisco) ->
    fun(Record) -> 
		{value, TableIdx} = dataset:get_value('$tableIndex', Record),
		ApMac = mac(TableIdx),
        AP = [{ap_en, ApMac}, {mac, ApMac} | mapping(Record, ?AP_MAPPING)],
        {value, ApEn} = dataset:get_value(ap_en, AP),
		{value, State} = dataset:get_value(managed_state, AP),
        State1 = online(cisco, State),
        {ApEn, keyreplace(managed_state, 1, AP, {managed_state, State1})}
    end;

ap_mapping_fun(Mib) ->
    fun(Record) -> 
        AP = mapping(Record, ?AP_MAPPING),
        {value, ApEn} = dataset:get_value(ap_en, AP),
	{value, State} = dataset:get_value(managed_state, AP),
        State1 = online(Mib, State),
        {ApEn, keyreplace(managed_state, 1, AP, {managed_state, State1})}
    end.

omc_ac_mapping_fun() ->
    fun(Record) -> mapping(Record, ?OMC_AC_MAPPING) end.
	
ac_mapping_fun() ->
    fun(Record) -> mapping(Record, ?AC_MAPPING) end.

bras_mapping_fun() ->
	fun(Record) -> mapping(Record, ?BRAS_MAPPING) end.
 
fatap_mapping(netgear_wg4526_fatap) ->
	fun(Record) ->
	   {value, ApMac} = dataset:get_value(apMac, Record),
	   ApMac1 = mac(string:join(extlib:slice(2, ApMac),":")), 
	   FatAp0 = mapping(Record, ?FATAP_MAPPING),
	   FatAp = keyreplace(mac, 1, FatAp0, {mac, ApMac1}),
	   keyreplace(ap_en, 1, FatAp, {ap_en, ApMac1})
	end;

fatap_mapping(_Mib) ->
	fun(Record) ->
    	   mapping(Record, ?FATAP_MAPPING)
	end.

null_mapping(Record) ->
    Record.

noindex_mapping(Record) ->
	noindex_mapping(Record, []).

noindex_mapping([], Acc) ->
	Acc;

noindex_mapping([{'$tableIndex', _}|Record], Acc) ->
	noindex_mapping(Record, Acc);

noindex_mapping([H|Record], Acc) ->
	noindex_mapping(Record, [H|Acc]).

intf_mapping_fun(Mib) -> 
	fun(Record) -> 
		{IfIndex, Intf} = intf_mapping(Record),
		IfSpeed = ifspeed(Mib, get_value(ifSpeed, Intf)),
		Intf1 = keyreplace(ifSpeed, 1, Intf, {ifSpeed, IfSpeed}),
		{IfIndex, Intf1}
	end.

ifspeed(hongxin_fhap2400, V) ->
	V * 1000000;
ifspeed(jiesai_at3600, V) ->
	V * 1000000;
ifspeed(zte_w901, V) ->
	V * 1000000;
ifspeed(_, V) ->
	V.

intf_mapping(Record) ->
    intf_mapping(Record, []).

intf_mapping([], Intf) ->
    IfIndex = get_value(ifIndex, Intf, 1),
    {IfIndex, Intf};
    
intf_mapping([{'$tableIndex', TabIdx} | Record], Intf) ->
   IfIndex1 =
    case TabIdx of
    [BroadIdx, IfIndex] -> (BroadIdx bsl 8) + IfIndex;
    [IfIndex] -> IfIndex
    end,
    intf_mapping(Record, [{ifIndex, IfIndex1}|Intf]);

intf_mapping([{ifIndex, _} | Record], Intf) ->
    intf_mapping(Record, Intf);

intf_mapping([{ifPhysAddress, PhysAddr} | Record], Intf) ->
    intf_mapping(Record, [{ifPhysAddress, mac(PhysAddr)}|Intf]);

intf_mapping([Attr | Record], Intf) ->
    intf_mapping(Record, [Attr|Intf]).
	
omc_ac_perf_mapping(Record) ->
    Record1 = lists:keydelete('$tableIndex', 1, Record),	
	omcac_perf_mapping(Record1).

omcac_perf_mapping(Record) ->
     Record1 = [{Name, i(Value)} || {Name, Value} <- Record],
     case dataset:get_value(memRTUsage, Record1) of
     {value, _MemRTUsage} -> 
        Record1;
     {false, _} ->
        {value, Total} = dataset:get_value(totalMemory, Record1, 1),
        {value, Free}  = dataset:get_value(freeMemory, Record1, 1),
        if
        Total == 0 ->
            Record1;
        true ->
            [{memRTUsage, ((Total - Free) * 100) div Total} | Record1]
        end
     end.

ac_perf_mapping(Record) ->
     Record1 = [{Name, i(Value)} || {Name, Value} <- Record],
	Record2 =
     case dataset:get_value(flashMemRTUsage, Record1) of
     {value, _FlashUsage} -> 
        Record1;
     {false, _} ->
        {value, Total} = dataset:get_value(flashMemTotal,Record1, 1),
        {value, Free}  = dataset:get_value(flashMemFree, Record1, 1),
        if
        Total == 0 ->
            Record1;
        true ->
            [{flashMemRTUsage, ((Total - Free) * 100) div Total} | Record1]
        end
     end,
	case dataset:get_value(dHCPIpPoolUsage, Record2) of
	{value, _Usage} -> Record2;
	{false, _} -> [{dHCPIpPoolUsage, 0} | Record2]
	end.

pool_perf_mapping(Record) ->
    {value, IdxOid} = dataset:get_value('$tableIndex', Record),
    {value, Usage} = dataset:get_value(ipPoolUsage, Record),
    {mib_oid:to_str(IdxOid), Usage}.

radio_mapping_fun(Mib) ->
    fun(Record) ->
        [Index] = get_value('$tableIndex', Record,[1]),
        Radio = mapping(Record, ?RADIO_MAPPING),
		{RadioIndex, Radio1} = 
		case get_value(radioIndex, Radio) of
		undefined -> {Index, [{radioIndex, Index}|Radio]};
		Val -> {Val, Radio}
		end,

        Model = get_value(radioModel, Radio1, 0),
        Model1 = radio_mod(Mib, Model),
        Radio2 = keyreplace(radioModel, 1, Radio1, {radioModel, Model1}),

        {RadioIndex, [{radio_no, RadioIndex} | Radio2]}
    end.

ssid_mapping_fun(SsidIndex) ->
	fun(Record)->
		Ssid = mapping(Record, ?SSID_MAPPING),
        {SsidIndex, [{ssidIndex, SsidIndex} |Ssid]}
	end.

portal_mapping(Portal) ->
    [_Len|Ssid] = get_value('$tableIndex', Portal),
    Url = get_value(portalServerURL, Portal),
    Ssid ++ ": " ++ Url.

portal_reduce(Portals) ->
    [{portalServerURL, list_to_binary(string:join(Portals, ","))}].

radius_mapping(Server) ->
    IP = get_value(radiusAuthServerIPAdd, Server),
    Port = get_value(radiusAuthServerPort, Server),
    {ip(IP), s(Port)}.

radius_reduce(Servers) ->
    {Ips, Ports} = lists:unzip(Servers),
    [{radiusAuthServerIpAdd, binary_join(Ips, ",")},
     {radiusAuthServerPort, binary_join(Ports, ",")}].

ippool_mapping(IpPool) ->
	ippool_mapping(IpPool, []).

ippool_mapping([], Acc) ->
	Acc;
ippool_mapping([{'$tableIndex', IdxOid}|IpPool], Acc) ->
	ippool_mapping(IpPool, [{ipPoolId, list_to_binary(mib_oid:to_str(IdxOid))}|Acc]);
ippool_mapping([{ipPoolName, Name}|IpPool], Acc) ->
	ippool_mapping(IpPool, [{ipPoolName, s(Name)}|Acc]);
ippool_mapping([{ipPoolStartAddr, StartAddr}|IpPool], Acc) ->
	ippool_mapping(IpPool, [{ipPoolStartAddr, ip(StartAddr)}|Acc]);
ippool_mapping([{ipPoolStopAddr, StopAddr}|IpPool], Acc) ->
	ippool_mapping(IpPool, [{ipPoolStopAddr, ip(StopAddr)}|Acc]);
ippool_mapping([H|T], Acc) ->
	?ERROR("unknown ippool attr: ~p", [H]),
	ippool_mapping(T, Acc).

ippool_reduce(IpPools) ->
	{Ids, Names, StartAddrs, StopAddrs} = 
	lists:foldl(fun(IpPool, {IdAcc, NameAcc, StartAddrAcc, StopAddrAcc}) -> 
		{value, Id} = dataset:get_value(ipPoolId, IpPool),
		{value, Name} = dataset:get_value(ipPoolName, IpPool),
		{value, StartAddr} = dataset:get_value(ipPoolStartAddr, IpPool, ip("0.0.0.0")),
		{value, StopAddr} = dataset:get_value(ipPoolStopAddr, IpPool, ip("0.0.0.0")),
		{[Id|IdAcc], [Name|NameAcc], [StartAddr|StartAddrAcc], [StopAddr|StopAddrAcc]}
	end, {[], [], [], []}, IpPools),
	[{ippool_ids, binary_join(Ids, ",")},
	{ippool_name, binary_join(Names, ",")},
	{ipPoolStartAddr, binary_join(StartAddrs, ",")},
	{ipPoolStopAddr, binary_join(StopAddrs, ",")}].

sta_mapping(Record) ->
	{value, ApMac} = dataset:get_value(apId, Record),
	Sta = mapping(Record, ?STA_MAPPING),
	{value, StaMac} = dataset:get_value(sta_mac, Sta),
	%case dataset:get_value(staUpTime, Record) of
 	%	{value, StaUpTime} ->
	%		{value, Timestamp} = dataset:get_value('$timestamp', Record),
	%		UpTime = Timestamp - StaUpTime div 100,
	%		{mac(ApMac), {StaMac, [{sta_uptime, UpTime}|Sta]}};
	%	{false, _} ->
	{mac(ApMac), {StaMac, Sta}}.
	%end.	

sta_reduce(Records) ->
    sta_reduce(Records, dict:new()).

sta_reduce([], Dict) ->
    dict:to_list(Dict);

sta_reduce([{ApMac, Sta}|T], Dict) ->
    NewDict = 
    case dict:find(ApMac, Dict) of
    {ok, _} -> dict:append(ApMac, Sta, Dict);
    error -> dict:store(ApMac, [Sta],Dict)
    end,
    sta_reduce(T, NewDict).

user_mapping(Record) ->
    {value, Index} = dataset:get_value('$tableIndex', Record),
     ApMac = lists:sublist(Index, 2, 17),
     StaMac = lists:sublist(Index, 20, 17),
     {ApMac, {StaMac, sta_login(Record)}}.

sta_login(Record) ->
   {value, LoginNum0}=dataset:get_value(loginNum,Record,[]),
   parse(LoginNum0).

mapping(Record, Mapper) ->
    mapping(Record, Mapper, []).

mapping([], _, Acc) ->
    Acc;

mapping([{Mib, Val}|T], Mapper, Acc) ->
    case lists:keysearch(Mib, 1, Mapper) of
    {value, {_, Name}} when is_atom(Name) -> 
        mapping(T, Mapper, [{Name, Val}|Acc]);
    {value, {_, Name, Formatter}} when is_atom(Name) ->
        Val1 = apply(mib_formatter, format, [Formatter, Val]),
        mapping(T, Mapper, [{Name, Val1}|Acc]);
    {value, {_, Names}} when is_list(Names) ->
        TupList = [{Name, Val} || Name <- Names],
        mapping(T, Mapper, TupList ++ Acc);
    {value, {_, Names, Formatter}} when is_list(Names) ->
        Val1 = apply(mib_formatter, format, [Formatter, Val]),
        TupList = [{Name, Val1} || Name <- Names],
        mapping(T, Mapper, TupList ++ Acc);
    false ->
        mapping(T, Mapper, Acc)
    end.

online(Mib, Val) ->
    case proplists:get_value(Mib, ?ONLINES) of
    undefined -> 
        ?ERROR("no online for mib: ~p", [Mib]),
        0;
    Online ->
        if
        Val == Online -> 1;
        true -> 0
        end
    end.

radio_mod(Mib, V) ->
    Default = integer_to_list(V),
    RadioMod = 
    case proplists:get_value(Mib, ?RADIO_MODS) of
    undefined -> 
        ?ERROR("no radio_mod for mib: ~p", [Mib]),
        Default;
    Mapping ->
        proplists:get_value(V, Mapping, Default)
    end,
    list_to_binary(RadioMod).
        
parse([]) ->
    [];
parse(LoginString) ->
    parse(LoginString,"@").

parse(LoginString,"@") ->
  [Num0|_]=string:tokens(LoginString,"@"),
  Num=extbif:to_integer(Num0),
  if is_integer(Num) ->
      Num;
    true ->
      parse(LoginString,".")
  end;

parse(LoginString,".") ->
  Tokens=string:tokens(LoginString,"."),
  case lists:filter(fun(Token) -> is_integer(extbif:to_integer(Token)) end,Tokens) of
    [] ->
      "unknow number";
    L -> 
      hd(L)
  end;

parse(_LoginString,_Separator) ->
  "unknow number".
ifdn(IfIndex, Dn) ->
    list_to_binary(["ifindex=", integer_to_list(IfIndex), ",", Dn]).


