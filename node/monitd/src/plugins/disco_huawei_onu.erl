-module(disco_huawei_onu).

-created("hejin 2010-06-24").

-include_lib("elog/include/elog.hrl").

-include("snmp/huawei_new.hrl").

-include("snmp/huawei_onu.hrl").

-include("snmp/rfc1213.hrl").

-export([disco/4, disco_boards/3, disco_ports/4, get_profile/2, get_version/2, get_version/3,disco_self/3]).

-define(PortType,[{506, "adslPort"}, {524, "pstnPort"},{573, "geElcPort"},{577, "eponOnuPort"},{576, "gponOnuPort"},{571,"feElcPort"}]).
-define(StatusType,[{506, "onu_adsl"}, {524, "onu_voip"},{573, "onu_pon"},{577, "onu_pon"},{576, "onu_pon"},{571,"onu_lan"}]).

-import(extbif,[to_list/1]).

disco(Dn, Ip, AgentData, _Args) ->
	{ok,OnuInfo} = disco_self(Dn, Ip, AgentData),
    ?INFO("begin to disco onu: ~p", [Ip]),
    {BoardClass,Boards} = disco_boards(Dn, Ip, AgentData),
    ?INFO("Boards: ~p,~p", [BoardClass,Boards]),
    {ok, Ports} = disco_ports(Dn, Ip, BoardClass, AgentData),
    ?INFO("Ports: ~p", [Ports]),
    {ok, OnuInfo, [{entry,boards,Dn,Boards}] ++ [{entry,ports,Dn,Ports}]}.


disco_self(_Dn, Ip, AgentData) ->
    case snmp_mapping:get_entry(Ip, disco_util:map2oid([?hwNarrowIp]), [0], AgentData) of
        {ok, Row} ->
                {value, NarrowIp} = dataset:get_value(narrowIp, Row),
                {ok,[{narrow_ip, mib_formatter:ip(NarrowIp)}]};
        {error, Reason2} ->
            ?WARNING("~p, ~p", [Ip, Reason2]),
            {ok, []}
    end.

disco_boards(_Dn, Ip, AgentData) ->
    ?INFO("disco onu boards: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, disco_util:map2oid(?hwSlotEntry), AgentData) of
    {ok, Rows} ->
         lists:mapfoldl(fun(Row, Data) ->
            {value, SlotNo} = dataset:get_value(index, Row),
            Boardid = lists:concat(["NA-0-",SlotNo]),
            {value, MemUsage} = dataset:get_value(memusage, Row),
            {value, CpuLoad} = dataset:get_value(cpuload, Row),
            {value, Type} = dataset:get_value(type, Row),
            {value, Descr} = dataset:get_value(boardDescr, Row),
            BoardClass = proplists:get_value(Descr, ?BoardType, unknow),
            {value, OperStatus0} = dataset:get_value(operstatus, Row),
            {value, AdminStatus0} = dataset:get_value(adminstatus, Row),
			OperStatus = disco_util:lookup("huawei",<<"all">>,<<"olt_board">>,<<"oper">>,OperStatus0),
            AdminStatus = disco_util:lookup("huawei",<<"all">>,<<"olt_board">>,<<"admin">>,AdminStatus0),
            {value, Version} = dataset:get_value(version, Row),
            MainVersion = get_version(Version, "Board"),
            HardVersion = get_version(Version, "PCB") ++ get_version(Version, "Pcb"),
            BootVersion = get_version(Version, "BIOS"),
            LogicVersion = get_version(Version, "Logic"),
            CpuVersion = get_version(Version, "CPU", 2),
            AppVersion = get_version(Version, "APP"),
            Board = [{cfgmaintype, Type}, {actmaintype, Type},{slot_no, SlotNo},
                {memusage, MemUsage},{cpuload, CpuLoad},{boardid,Boardid},
                {cardacttype, Descr}, {operstatus, OperStatus},
                {adminstatus, AdminStatus},{hardversion, HardVersion},
                {masterversiontag, MainVersion}, {bootromtag, BootVersion},
                {logicversiontag, LogicVersion}, {cpuversiontag, CpuVersion}, {appversiontag, AppVersion}],
            { {SlotNo, BoardClass}, [Board| Data] }
        end, [], Rows);
    {error, Reason} ->
        ?WARNING("~p,~p", [Ip, Reason]),
        {[],[]}
    end.


disco_ports(Dn, Ip, BoardClass, AgentData) ->
    ?INFO("disco onu port: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?IfEntry, AgentData) of
    {ok, Rows} ->
        Rows1 = lists:filter(fun(Row) ->
%            {value, IfType} = dataset:get_value(ifType, Row),
            {value, IfDescr} = dataset:get_value(ifDescr, Row),
            Idx = string:str(IfDescr, "Huawei-"),
            (Idx =/= 0)
        end, Rows),

        %adsl 上下行带宽模板
        {ok, Profiles} = case lists:keysearch('adslPort', 2, BoardClass) of
            {value, _BoardType} ->
                get_profile(Ip, AgentData);
             false ->
                 {ok, []}
             end,
        IfEntries = lists:foldl(fun(Row, Dict) ->
            {value, [IfIndex]} = dataset:get_value('$tableIndex', Row),
            {SlotNo, PortNo, PortCategory} = calc_no(IfIndex),
            {value, IfDescr} = dataset:get_value(ifDescr, Row),
            {value, IfSpeed} = dataset:get_value(ifSpeed, Row),
            PortName = "NA-0-" ++ to_list(SlotNo) ++ "-" ++ to_list(PortNo),
			{value, PortOper} = dataset:get_value(ifAdminStatus, Row),
            {value, PortAdmin} = dataset:get_value(ifOperStatus, Row),
            {value, PortType} = dataset:get_value(SlotNo, BoardClass, unknow),
            PortData = case PortType of
                adslPort -> %adsl 端口取上下行带宽
                    case snmp_mapping:get_entry(Ip, disco_util:map2oid([?adslLineConfProfile]), [IfIndex], AgentData) of
                        {ok, Entry} ->
                            {value, Profile} = dataset:get_value(adslLineConfProfile, Entry),
                            proplists:get_value(Profile, Profiles, []);
                        {error, Reason} ->
                            ?WARNING("~p", [Reason]),
                            []
                    end;
                _ ->
                    []
            end,
            PortDn = "port=" ++ integer_to_list(IfIndex) ++ "," ++ binary_to_list(Dn),
            PortInfo = {entry, port, list_to_binary(PortDn), [{port_index,IfIndex},{port_category, PortCategory},{port_no, PortNo},{admin_status,PortAdmin},
						{oper_status,PortOper},{port_name,PortName},{speed,IfSpeed},{slot_no, SlotNo},{ponid,PortName},{port_desc,IfDescr} | PortData]},
            PortId = to_list(SlotNo * 10000) ++ to_list(PortNo),
            dict:store(PortId, PortInfo, Dict)
        end, dict:new(), Rows1),
%        ?INFO("port info :~p",[dict:to_list(IfEntries)]),
        Entries = case snmp_mapping:get_table(Ip, [?hwPortType, ?hwPortOperStatus, ?hwPortAdminStatus], AgentData) of
            {ok, PortEntries} ->
                lists:foldl(fun(Entry, NewPort) ->
                   % ?INFO("entry is ~p", [Entry]),
                    {value, [_, SlotNo, _, PortNo]} = dataset:get_value('$tableIndex', Entry),
                    {value, PortTypeValue} = dataset:get_value(hwPortType, Entry),
                    %统一用共有状态
%                    {value, PortOper} = dataset:get_value(hwPortOperStatus, Entry),
                    PortId = to_list(SlotNo * 10000) ++ to_list(PortNo),
                    case dict:find(PortId, IfEntries) of
                        {ok, {entry, port, _PortDn, PortAttr} } ->
							{value, PortOper} = dataset:get_value(admin_status, PortAttr),
					        {value, PortAdmin} = dataset:get_value(oper_status, PortAttr),
					        {value, PortType} = dataset:get_value(PortTypeValue, ?PortType, PortTypeValue),
					        {value, StatusType} = dataset:get_value(PortTypeValue, ?StatusType, PortTypeValue),
							OperStatus = disco_util:lookup("huawei",<<"all">>,StatusType,<<"oper">>,PortOper),
					        AdminStatus = disco_util:lookup("huawei",<<"all">>,StatusType,<<"admin">>,PortAdmin),
                            PortAttr0 = lists:keyreplace(admin_status, 1, PortAttr, {admin_status, AdminStatus}),
                            PortAttr1 = lists:keyreplace(oper_status, 1, PortAttr0, {oper_status, OperStatus}),
                            [PortAttr1++[{port_type, PortType}]|NewPort];
                        error -> %基本是pstn口
                            {value, PortType} = dataset:get_value(SlotNo, BoardClass, unknow),
                            {PortIndex, PortCage} = synth_index(PortType, SlotNo, PortNo),
                            %get pstn telno 2011-6-12
                            TelNo = get_pstn_telno(Ip, PortIndex, AgentData),
                            PstnName =  "NA-0-" ++ to_list(SlotNo) ++ "-" ++ to_list(PortNo),
                            PortInfo = [{port_category, PortCage}, {port_no, PortNo}, {slot_no, SlotNo},{ponid,PstnName},
                                        {port_type, PortType},{port_name, PstnName}, {port_index, PortIndex},
                                        {telno, TelNo}],
                            [PortInfo | NewPort]
                    end
                end,[], PortEntries);
            {error, Reason} ->
               ?WARNING("~p", [Reason]),
                []
         end,
        ?INFO("port : ~p", [Entries]),
        {ok, Entries};
    {error, Reason} ->
       ?WARNING("~p,~p", [Ip, Reason]),
        {ok,[]}
    end.

get_profile(Ip, AgentData) ->
    DbwOid = [?adslAtucChanConfInterleaveMaxTxRate, ?adslAturChanConfInterleaveMaxTxRate],
    case snmp_mapping:get_table(Ip, DbwOid, AgentData, 10000) of
    {ok, Rows} ->
        Entries = lists:map(fun(Row) ->
            {value, Profile} = dataset:get_value('$tableIndex', Row),
            {value, Downmaximumbw} = dataset:get_value(adslAtucMaxTxRate, Row),
            {value, Upmaximumbw} = dataset:get_value(adslAturMaxTxRate, Row),
            {Profile, [{downmaximumbw, Downmaximumbw},{upmaximumbw, Upmaximumbw}]}
        end, Rows),
        {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p,~p", [Ip, Reason]),
        {ok, []}
    end.

get_pstn_telno(Ip, Index, AgentData) ->
    case snmp_mapping:get_entry(Ip, disco_util:map2oid([?hwPstnCTPTelNo]), [Index], AgentData) of
        {ok, Entry} ->
            {value, PstnCTPTelNo} = dataset:get_value(hwPstnCTPTelNo, Entry),
            PstnCTPTelNo;
        {error, Reason} ->
            ?WARNING("~p,~p", [Ip, Reason]),
            []
    end.

%fe-E,adsl-C,pstn-30
calc_no(IfIndex) when (IfIndex > 16#C000000) and (IfIndex < 16#C0FFFFF) -> %adsl
   PortNo = (IfIndex band (16#0FFF)) div 16#40,
   SlotNo = ((IfIndex bsr 12) band (16#FF)) div 2,
   {SlotNo, PortNo, 3};
calc_no(IfIndex) when (IfIndex > 16#E000000) and (IfIndex < 16#E0FFFFF) -> %fe
   PortNo = (IfIndex band (16#0FFF)) div 16#40,
   SlotNo = ((IfIndex bsr 12) band (16#FF)) div 2,
   {SlotNo, PortNo, 3};
calc_no(IfIndex) when (IfIndex > 16#30000000) and (IfIndex < 16#F0000000) ->%pstn
   PortNo = (IfIndex band (16#0FFF)) div 16#10,
   SlotNo = ((IfIndex bsr 12) band (16#FF)) * 2,
   PortNo2 = if PortNo > 128 ->
       PortNo - 128;
       true -> PortNo
   end,
   SlotNo2 = if SlotNo == 0 ->
       1;
       true -> SlotNo
    end,
%   ?INFO("index: ~p, slot: ~p, port: ~p",[IfIndex, SlotNo, PortNo]),
   {SlotNo2, PortNo2, 3};
   %后面有对没有发现的端口加入对应的slot_no和port_no，后面的更准确，这里怕出错影响后面其他口数据
calc_no(IfIndex) when (IfIndex > 16#F0000000) and (IfIndex < 16#FFFFFFFF) ->%epon --- main
    PortNo = (IfIndex band (16#0FFF)) div 16#100,
    SlotNo = ((IfIndex bsr 12) band (16#FF)) div 2,
   {SlotNo, PortNo, 2};
calc_no(IfIndex) ->
    ?ERROR("failed to calc slotno and portno for ifindex: ~p", [IfIndex]),
    {0, 0, 2}.

synth_index(PortType, SoltNo, PortNo) ->
    %取solt，port，合成共有index
    case PortType of
        pstnPort -> %30
            {IfSoltNo, IfPortNo} =  case SoltNo of
                1 -> %30000810
                    {0, 16#80 + PortNo};
                2 -> %30001010
                    {1, PortNo};
                3 -> %30001810
                    {1, 16#80 + PortNo};
                _ ->
                 {(SoltNo div 2), PortNo}
             end,
            {(IfSoltNo bsl 12) + (IfPortNo bsl 4) + 16#30000000, 3};
        _ -> %error
            {SoltNo * 1000 + PortNo, 2}
    end.

get_version(Version, Type) ->
    get_version(Version, Type, 1).

get_version(Version, Type, N) ->
	Version1 = string:strip(Version),
    TypeN= string:str(Version1, Type),
     case TypeN of
          0 ->
              "";
          _ ->
             LeftTypeStr = string:strip(string:substr(Version1, TypeN + length(Type))),
             case string:str(LeftTypeStr, "\r") of
                 0 ->
                    string:substr(LeftTypeStr, string:str(LeftTypeStr, ":")+1);
                 E ->
                    TypeVersion = string:sub_string(LeftTypeStr, string:str(LeftTypeStr, ":")+1, E-1),
                    if  N =< 1 ->
                      TypeVersion;
                      true ->
                          LeftStr = string:substr(LeftTypeStr, E),
                          TypeVersion ++ get_version(LeftStr, Type, N -1)
                   end
            end
         end.

