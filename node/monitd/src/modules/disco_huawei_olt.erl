%% 		MA5680 MA5683
-module(disco_huawei_olt).

-include_lib("elog/include/elog.hrl").

-include("snmp/huawei_new.hrl").

-include("snmp/rfc1213.hrl").

-define(PON, 1).
-define(GE, 2).
-define(FE, 3).

-import(extbif, [to_list/1]).

-export([disco/4,disco_boards/3,disco_ports/3,disco_onus/3,disco_onu_ports/3, calc_no/2]).

-define(PortType,[{592, "eponOltPort"}, {573, "geElcPort"}, {574, "geOptPort"}, {591, "geOptPort"},{579, "gponOltPort"}]).
-define(PortCategory,[{592, 1}, {573, 2}, {574, 2}, {579, 1}, {591, 2}]).


disco(Dn, Ip, AgentData, _Args) ->
    ?INFO("begin to disco epon olt: ~p", [Ip]),
    {ok, Boards} = disco_boards(Dn, Ip, AgentData),
    {ok, Ports,Dicts} = disco_ports(Dn, Ip, AgentData),
    ?INFO("Ports: ~p,key:~p", [Ports,dict:fetch_keys(Dicts)]),
	Keys = lists:filter(fun(Elem) ->Elem=="eponOltPort" orelse Elem =="gponOltPort" end,dict:fetch_keys(Dicts)),
	case lists:flatten(Keys) of
		"eponOltPort" ->
			{ok,EponData} = disco_onus(Dn, Ip, AgentData),
			{ok,OnusPort} = disco_onu_ports(Dn, Ip, AgentData),
			{ok, [{olt_pon_type,"epon"}], Boards ++ Ports ++ EponData ++ OnusPort};
		"gponOltPort" ->
			{ok,GponData} = snmp_huawei_golt:disco(Dn, Ip, AgentData),
			{ok, [{olt_pon_type,"gpon"}], Boards ++ Ports ++ GponData};
		"eponOltPortgponOltPort" ->
			{ok,EponData} = disco_onus(Dn, Ip, AgentData),
			{ok,OnusPort} = disco_onu_ports(Dn, Ip, AgentData),
			{ok,GponData} = snmp_huawei_golt:disco(Dn, Ip, AgentData),
			{ok, [{olt_pon_type,"epon_gpon"}], Boards ++ Ports ++ EponData++GponData ++ OnusPort};
		"gponOltPorteponOltPort" ->
			{ok,EponData} = disco_onus(Dn, Ip, AgentData),
			{ok,OnusPort} = disco_onu_ports(Dn, Ip, AgentData),
			{ok,GponData} = snmp_huawei_golt:disco(Dn, Ip, AgentData),
			{ok, [{olt_pon_type,"epon_gpon"}], Boards ++ Ports ++ EponData++GponData ++ OnusPort};
		 _ ->
			?WARNING("can not find epon or gpon ~p,key: ~p", [Ports,lists:flatten(Keys)]),
			{ok, [], Boards ++ Ports}
	end.


disco_boards(Dn, Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, ?hwSlotEntry, AgentData) of
    {ok, Rows} ->
        Entries  = lists:map(fun(Row) ->
            ?INFO("~p", [Row]),
            {value, SlotNo} = dataset:get_value(index, Row),
            Boardid = lists:concat(["NA-0-",SlotNo]),
            SlotDn = "slot=" ++Boardid ++ "," ++ to_list(Dn),
            Board = transform_board(Row),
            {entry, board, list_to_binary(SlotDn), [{slot_no, SlotNo},{boardid,Boardid} | Board]}
        end, Rows),
        ?INFO("~p", [Entries]),
        {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p, ~p", [Ip, Reason]),
        {ok, []}
    end.


disco_ports(Dn, Ip, AgentData) ->
	 case snmp_mapping:get_table(Ip, [?hwPortType, ?hwPortOperStatus, ?hwPortAdminStatus], AgentData) of
        {ok, Rows} ->
            {Entries,Dicts} = lists:mapfoldl(fun(Row,Dict) ->
                {value, [_, SoltNo, _, PortNo]} = dataset:get_value('$tableIndex', Row),
                {value, PortType0} = dataset:get_value(hwPortType, Row),
                {value, PortOper} = dataset:get_value(ponOperStatus, Row),
                {value, PortAdmin} = dataset:get_value(ponAdminStatus, Row),
                PortId = "NA-0-" ++ integer_to_list(SoltNo) ++ "-" ++ integer_to_list(PortNo),
                {value, PortType} = dataset:get_value(PortType0, ?PortType, PortType0),
                {value, PortCategory} = dataset:get_value(PortType0, ?PortCategory, 1),
                PortIndex = synth_index(SoltNo,PortNo,PortType),
       			OperStatus = disco_util:lookup("huawei",<<"all">>,<<"olt_pon">>,<<"oper">>,PortOper),
       			AdminStatus = disco_util:lookup("huawei",<<"all">>,<<"olt_pon">>,<<"admin">>,PortAdmin),
				PortDn = lists:concat(["port=",PortIndex,",",to_list(Dn)]),
				 PortAttr = [{port_type,PortType},{admin_status,AdminStatus},{oper_status,OperStatus},
				{port_category, PortCategory},{port_no, PortNo}, {slot_no, SoltNo},{ponid,PortId},{port_name,PortId},
							{port_index,PortIndex}],
                {{entry, port, PortDn, PortAttr},dict:append_list(PortType,PortDn,Dict)}
            end, dict:new(),Rows),
	        ?INFO("~p", [Entries]),
			{ok,Entries,Dicts};
        {error, Reason} ->
           ?WARNING("~p, ~p", [Ip, Reason]),
        {ok, []}
     end.

disco_onus(Dn, Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, disco_util:map2oid(?hwOnuEntry), AgentData, 10000) of
    {ok, Rows} ->
        ?INFO("disco onus~p", [Rows]),
        {ok, DbaProfileEntries} = get_dba_profile(Ip, AgentData),
        {ok, BandwidthEntries} = get_onu_bandwidth(Ip, AgentData),
        Entries = lists:map(fun(Row) ->
            {value, [PonIndex, OnuNo]} = dataset:get_value('$tableIndex', Row),
            {value, LineProfile} = dataset:get_value(ontLineProfile, Row),
            DbaProfile = case lists:keysearch(LineProfile, 1, DbaProfileEntries) of
                {value, DbaProfileEntry} ->
                    {_Key,Value} = DbaProfileEntry,
                    Value;
                false ->
                    ""
            end,
            Bandwidth = case lists:keysearch(DbaProfile, 1, BandwidthEntries) of
                {value, BandwidthEntry} ->
                    {_Key1,Value1} = BandwidthEntry,
                    Value1;
                false ->
                    []
            end,
            %?INFO("bandwidth: ~p~p~p", [LineProfile,DbaProfile,Bandwidth]),
            {SlotNo, PortNo, _PortType} = calc_no(onu, PonIndex),
            Onu = transform(Row),
            Rdn = integer_to_list(PonIndex + OnuNo),
			PonId = lists:concat(["NA-0-",SlotNo,"-",PortNo]),
            [{rdn, Rdn}, {discovery_state, 1},
                {slot_no, SlotNo}, {port_no, PortNo}, {onu_no, OnuNo},{ponid,PonId} | Onu] ++ Bandwidth
        end, Rows),
        ?INFO("~p", [Entries]),
        {ok, [{entry, onus, Dn, Entries}]};
    {error, Reason} ->
        ?WARNING("~p, ~p", [Ip, Reason]),
        {ok, []}
    end.

get_dba_profile(Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, [?hwOntDbaProfileName], AgentData, 10000) of
    {ok, Rows} ->
        Entries = lists:map(fun(Row) ->
            {value, LineProfile} = dataset:get_value('$tableIndex', Row),
            {value, DbaProfile} = dataset:get_value(ontDbaProfile, Row),
            LineProfile1 =  lists:sublist(LineProfile, 2, length(LineProfile)-2),
            {LineProfile1, DbaProfile}
        end, Rows),
        {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p, ~p", [Ip, Reason]),
        {ok, []}
    end.

get_onu_bandwidth(Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, ?hwOnuDbaRateEntry, AgentData, 10000) of
    {ok, Rows} ->
       % ?INFO("~p", [Rows]),
        Entries = lists:map(fun(Row) ->
            {value, DbaProfile} = dataset:get_value('$tableIndex', Row),
            %{value, FixedRate} = dataset:get_value(ontDbaFixedRate, Row),
            {value, AssuredRate} = dataset:get_value(ontDbaAssuredRate, Row),
            {value, MaxRate} = dataset:get_value(ontDbaMaxRate, Row),
            {DbaProfile, [{downassuredbw, AssuredRate}, {downmaximumbw, MaxRate},
                          {upassuredbw, AssuredRate}, {upmaximumbw, MaxRate}]}
        end, Rows),
        ?INFO("~p", [Entries]),
        {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p, ~p", [Ip, Reason]),
        {ok, []}
    end.

disco_onu_ports(Dn, Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, [?hwEponDeviceOntPortOperateStatus, ?hwEponDeviceOntifEthernetOnlineState,?hwEponDeviceOntifEthernetSpeed], AgentData) of
    {ok, Rows} ->
        ?INFO("~p", [length(Rows)]),
        {_,AdslDict} = lists:mapfoldl(fun(Row,Dict) ->
            {value, [PonIdx, OnuIdx, PortIdx]} = dataset:get_value('$tableIndex', Row),
            {value, AdminStatus} = dataset:get_value(adminstate, Row),
            {value, OnlineStatus} = dataset:get_value(operstate, Row),
            {value, Speed0} = dataset:get_value(speed, Row),
            Speed = if
                    Speed0 < 0 -> 0;
                    true -> Speed0 * 1024000
                    end,
            OnuDn = "onu=" ++ integer_to_list(PonIdx + OnuIdx) ++ "," ++ to_list(Dn),
            PortName =lists:concat(["NA-NA-NA-",PortIdx]),
            PortAttr = [{port_name, PortName},{port_type,"feElcPort"},{ponid,PortName},{port_index,PortIdx},{port_desc,"ftth"},{port_category,3},
			{port_no, PortIdx},{slot_no, 0} ,{admin_status,disco_util:lookup("huawei",<<"all">>,<<"onu_adsl">>,<<"admin">>,AdminStatus)},
			{speed,Speed},{oper_status, disco_util:lookup("huawei",<<"all">>,<<"onu_adsl">>,<<"oper">>,OnlineStatus)}],
			{PortAttr,dict:append(OnuDn,PortAttr,Dict)}
        end,dict:new(), Rows),
        {ok, Dicts} = disco_onu_pstns(Dn, Ip, AgentData,AdslDict),
		Entries = [{entry,ports,Odn,Ports} || {Odn,Ports} <- dict:to_list(Dicts),length(Ports) < 8],
		?INFO("~p", [Entries]),
        {ok, Entries};
    {error, Reason} ->
       ?WARNING("~p, ~p", [Ip, Reason]),
        {ok, []}
    end.


disco_onu_pstns(Dn, Ip, AgentData,AdslDict) ->
    case snmp_mapping:get_table(Ip, [?hwEponDeviceOntPOTSPortOperateStatus], AgentData) of
    {ok, Rows} ->
        ?INFO("huawei onu pstn~p", [length(Rows)]),
        {_,Dicts} = lists:mapfoldl(fun(Row,Dict) ->
            {value, [PonIdx, OnuIdx, PortIdx]} = dataset:get_value('$tableIndex', Row),
            {value, AdminStatus} = dataset:get_value(adminstate, Row),
            OnuDn = "onu=" ++ integer_to_list(PonIdx + OnuIdx) ++ "," ++ to_list(Dn),
            PortName =lists:concat(["NA-NA-NA-",PortIdx]),
            PortAttr = [{port_name, PortName},{port_type,"pstnPort"},{ponid,PortName},
            {port_index,1000+PortIdx},{port_desc,"ftth"},{port_category,3},
			{port_no, PortIdx},{slot_no, 1} ,{admin_status,AdminStatus}],
			{PortAttr,dict:append(OnuDn,PortAttr,Dict)}
        end,AdslDict, Rows),
        {ok, Dicts};
    {error, Reason} ->
       ?WARNING("~p, ~p", [Ip, Reason]),
        {ok, AdslDict}
    end.






transform(Row) ->
    transform(Row, []).

transform([], Acc) ->
     Acc;
transform([{ontMacAddr, Mac} | T], Acc) ->
    transform(T, [{macaddr, to_mac_str(Mac)},{authmacaddr, to_mac_str(Mac)}|Acc]);
transform([{ipAddress, IpB}|T], Acc) ->
    Ip = mib_formatter:ip(IpB),
    case Ip of
    <<"0.0.0.0">> ->
        transform(T, Acc);
    _ ->
        transform(T, [{ip, Ip}|Acc])
    end;
transform([{onuProfile, Profile} | T], Acc) ->
    transform(T, [{type, Profile}|Acc]);
transform([{ontDescr, Descr} | T], Acc) ->
    transform(T, [{device_name, Descr}, {userinfo, Descr}|Acc]);
transform([{ontRunStatus, Status}|T], Acc) ->
	OnlineStatus = disco_util:lookup("huawei",<<"all">>,<<"onu">>,<<"oper">>,Status),
    transform(T, [{operstate,OnlineStatus}|Acc]);
transform([{ontConfigStatus, Status}|T], Acc) ->
	AdminStatus = disco_util:lookup("huawei",<<"all">>,<<"onu">>,<<"admin">>,Status),
    transform(T, [{adminstate, AdminStatus}|Acc]);
transform([{ontDownBandWidth, DownBW} | T], Acc) ->
    transform(T, [{downassuredbw, DownBW} | Acc]);
transform([{ontUpBandWidth, UpBW}|T], Acc) ->
    transform(T, [{upassuredbw, UpBW}|Acc]);
transform([{ontHardVer, HardVer}|T], Acc) ->
    ?INFO("huawei onu type~p", [HardVer]),
	Hardware_type = trans_version(HardVer),
    {Vendor,Hardware_type2} = case Hardware_type of
        "H821GPFD" ->
            {<<"huawei">>,"MA5620E"};
        "H821EPUA" ->
            {<<"huawei">>,"MA5620E"};
        "130C4500" ->
            {<<"huawei">>,"HG8245"};
        "V1.0" ->
            {<<"bell">>,"RE20"};
        "V3.0" ->
            {<<"zte">>,"ZTE-F460"};
        "AOB" ->
            {<<"huaqin">>,"421N"};
        "CA_V1A2" ->
            {<<"huawei">>,"R200"};
        "1A0D4500" ->
            {<<"huawei">>,"HGB8240R"};
        "240E4522" ->
            {<<"huawei">>,"HG8040"};
        "130D4600" ->
            {<<"huawei">>,"HG8245"};
        "3FE52958DEAB02" ->
            {<<"bell">>,"I-240W-Q"};
        "140C4510" ->
            {<<"huawei">>,"MA5626E"};
        _ ->
            {<<"huawei">>,Hardware_type}
        end,
    transform(T, [{hardwareversion, Hardware_type},{vendor,Vendor},{type, Hardware_type2}|Acc]);
transform([{ontSoftVer, SoftVer}|T], Acc) ->
    transform(T, [{softwareversion, trans_version(SoftVer)}|Acc]);
transform([{rtt, Roundtriptime}|T], Acc) ->
    transform(T, [{roundtriptime, Roundtriptime}|Acc]);
transform([{authpassword, Roundtriptime}|T], Acc) ->
    transform(T, [{authpassword, Roundtriptime}|Acc]);
transform([{loid, Loid}|T], Acc) ->
    transform(T, [{loid, Loid}|Acc]);
transform([_|T], Acc) ->
    transform(T, Acc).


transform_board(Board) ->
    transform_board(Board, []).

transform_board([], Data) ->
    Data;
transform_board([{workMode, 1}|Board], Data) ->
    transform_board(Board, [{bservice, "SCU"}|Data]);
transform_board([{workMode, "main"}|Board], Data) ->
    transform_board(Board, [{bservice, "SCU"}|Data]);
transform_board([{memusage, Value}|Board], Data) ->
    transform_board(Board, [{memusage, Value}|Data]);
transform_board([{cpuload, Value}|Board], Data) ->
    transform_board(Board, [{cpuload, Value}|Data]);
transform_board([{type, Value}|Board], Data) ->
    transform_board(Board, [{cfgmaintype, Value}, {actmaintype, Value}|Data]);
transform_board([{boardDescr, Value}|Board], Data) ->
    transform_board(Board, [{cardacttype, Value}|Data]);
transform_board([{operstatus, Value}|Board], Data) ->
    OperStatus = disco_util:lookup("huawei",<<"all">>,<<"olt_board">>,<<"oper">>, Value),
    transform_board(Board, [{operstatus, OperStatus}|Data]);
transform_board([{adminstatus, Value}|Board], Data) ->
    AdminStatus = disco_util:lookup("huawei",<<"all">>,<<"olt_board">>,<<"admin">>,Value),
    transform_board(Board, [{adminstatus, AdminStatus}|Data]);
transform_board([{version, Version}|Board], Data) ->
    MainVersion = snmp_disco_huawei_onu:get_version(Version, "Board"),
    HardVersion = snmp_disco_huawei_onu:get_version(Version, "PCB") ++ snmp_disco_huawei_onu:get_version(Version, "Pcb"),
    BootVersion = snmp_disco_huawei_onu:get_version(Version, "BIOS"),
    LogicVersion = snmp_disco_huawei_onu:get_version(Version, "Logic"),
    CpuVersion = snmp_disco_huawei_onu:get_version(Version, "CPU", 2),
    AppVersion = snmp_disco_huawei_onu:get_version(Version, "APP"),
    transform_board(Board, [{hardversion, HardVersion},
                {masterversiontag, MainVersion}, {bootromtag, BootVersion},
                {logicversiontag, LogicVersion}, {cpuversiontag, CpuVersion}, {appversiontag, AppVersion}|Data]);
transform_board([_|Board], Data) ->
    transform_board(Board, Data).


to_mac_str(L) when is_list(L) and (length(L) == 6) ->
	L1 = io_lib:format("~.16B~.16B~.16B~.16B~.16B~.16B", L),
    L2 = lists:map(fun(C) ->
        case length(C) of
            2 -> C;
            1 -> "0" ++ C
        end
    end, L1),
    list_to_binary(string:join(L2, ":"));

to_mac_str(L) when is_list(L) ->
    list_to_binary(L);

to_mac_str(L) ->
    ?WARNING("invalid mac: ~p", [L]),
	<<>>.

calc_no(_Type, IfIndex) when (IfIndex > 16#F0000000) and (IfIndex < 16#FCFFFFFF) ->
   PortType = ?PON,
   PortNo = (IfIndex bsr 8) band (16#0F),
   SlotNo = ((IfIndex bsr 12) band (16#FF)) div 2,
   {SlotNo, PortNo, PortType};
calc_no(Type, IfIndex) when (IfIndex > 16#E000000) and (IfIndex < 16#E0FFFFF) ->
   PortType = case Type of
       olt ->
           ?GE;
       onu ->
           ?FE
       end,
   PortNo = (IfIndex band (16#00FF)) div 16#40,
   SlotNo = ((IfIndex bsr 12) band (16#FF)) div 2,
   {SlotNo, PortNo, PortType};
calc_no(Type, IfIndex) ->
    ?ERROR("~p : failed to calc slotno and portno for ifindex: ~p", [Type, IfIndex]),
    {0, 0, 0}.


trans_version(Version) ->
    lists:filter(fun(Item) ->
        Item > 32  andalso  Item < 127
    end, Version).

synth_index(SoltNo, PortNo,PortType) ->
   if PortType == "geOptPort" %gei
       ->
          (PortNo * 16#40) + ((SoltNo * 2) bsl 12) + 16#E000000;
	 PortType=="gponOltPort" ->
		 (PortNo bsl 8) + ((SoltNo * 2) bsl 12) + 16#FA000000;
      true ->
          (PortNo bsl 8) + ((SoltNo * 2) bsl 12) + 16#FC000000
    end.



