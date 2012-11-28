-module(disco_bell_olt).

-created("chibj 2012-9-10").

-include_lib("elog/include/elog.hrl").

-include("snmp/bell.hrl").

-include("snmp/rfc1213.hrl").

-export([disco/3, disco_boards/3, disco_ports/4, disco_onus/3,disco_gonus/4]).

-import(extbif, [to_list/1]).

-define(Boards,[
    ?BoardType,
    ?BoardAdminStatus,
    ?BoardOperStatus
]).

-define(PonPort, [
    ?PortTemperature,
    ?PortVoltage,
    ?PortCurrent,
    ?PortSendPower,
    ?PortReceivePower
]).

-define(Onus, [
    ?OnuMac,
    ?OnuHardVersion,
    ?OnuSoftVersion,
    ?OnuOnlineStatus
]).

-define(GOnus, [
    ?GOnuType,
    ?GOnuHardVersion,
    ?GOnuSoftVersion,
    ?GOnuAdminStatus,
    ?GOnuPWD,
    ?GOnuOperStatus
]).

% 3EC15570         7300（DSLAM）
% 3FE20922         7325（DSLAM）
% 3FE21961         7302(DSLAM)/7360（GPON OLT）
% 3FE60153         7342（EPON OLT）
% 3FE64076         7353（GPON MDU ）
% MOD_XXXX         7342(GPON OLT，包含7342和7342META)
-define(OltTypes,[{"3FE21961" ,[{type,"7360"},{olt_pon_type,"gpon"}]},
                  {"3FE60153" ,[{type,"7342"},{olt_pon_type,"gpon"}]}]).

-define(GEpon,[{220,"eponOltPort"},{238,"gponOltPort"},{250,"gponOltPort"}]).

disco(Dn, Ip, AgentData) ->
    ?WARNING("begin to disco olt: ~p", [Ip]),
    {ok, OltSelf} = disco_self(Ip, AgentData),
    ?INFO("disco oltself: ~p", [OltSelf]),
    {ok, Boards} = disco_boards(Dn, Ip, AgentData),
    ?INFO("disco board and gei: ~p", [Boards]),
    {ok, Ports} = disco_ports(Dn, Ip, AgentData,OltSelf),
    ?INFO("disco port: ~p", [Ports]),
    case dataset:get_value(olt_pon_type, OltSelf) of
            {value, "gpon"} ->
                {ok, GOnus} = disco_gonus(Dn, Ip, AgentData,OltSelf),
                {ok, OntPorts} = disco_ont_ports(Dn, Ip, AgentData),
                {ok, OltSelf, Boards ++ Ports ++ GOnus ++ OntPorts };
            {value, "epon"} ->
                {ok, EOnus} = disco_onus(Dn, Ip, AgentData),
                {ok, OltSelf, Boards ++ Ports ++ EOnus};
            _ -> ?ERROR("unknown bell olt type ~p",[Dn])
    end.



disco_self(Ip, AgentData) ->
    ?INFO("disco olt self: ~p", [Ip]),
    case snmp_mapping:get_entry(Ip, monet_util:map2oid([?OltType]),[0],AgentData) of
        {ok, Row} ->
            {value, OltType} = dataset:get_value(olt_type, Row),
            OltSelf =  case string:str(OltType,"MOD") of
                        0 -> transform_olt_type(OltType);
                        _ -> SH = string:tokens(OltType,"_"),
                             [{hardversion, string:join(lists:sublist(SH,1,length(SH)-1),"_")},{softversion,lists:last(SH)},{type,"7342"},{olt_pon_type,"gpon"}]
                    end,
            {ok,OltSelf};
        {error, Reason2} ->
            ?WARNING("~p, ~p", [Ip, Reason2]),
            {ok, []}
    end.


transform_olt_type(OltType) ->
    case string:left(OltType,8,$.) of
            "........" -> [];
                Type0 ->
                        {value, Type} = dataset:get_value(Type0, ?OltTypes,[]),
                        [S,H] = string:tokens(OltType -- Type ,"_"),
                        [{hardversion,S},{softversion,H} | Type]
    end.



disco_boards(Dn, Ip, AgentData) ->
    ?INFO("disco olt boards: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?Boards, AgentData) of
    {ok, Rows} ->
        Entries0 =lists:filter(fun(Row) ->
            {value, Type} = dataset:get_value(type, Row),
            (Type =/= "EMPTY") and (Type =/= "empty") and (Type =/= "NOT_PLANNED")
        end, Rows),
        ?INFO("disco bell olt boards: ~p,~p", [Ip, Entries0]),
        Entries  = lists:map(fun(Row) ->
            {value, [BoardIndex]} = dataset:get_value('$tableIndex', Row),
            SlotNo = calc_board_no(BoardIndex),
            Boardid = lists:concat(["1-1-",SlotNo]),
            BoardDn = "slot=" ++Boardid ++ "," ++ to_list(Dn),
            {value, OperStatus0} = dataset:get_value(operstatus, Row),
            {value, AdminStatus0} = dataset:get_value(adminstatus, Row),
            {value, Type} = dataset:get_value(type, Row),
            BoardAttrs = [{boardid, Boardid},
						  {slot_no, SlotNo},
                          {operstatus, OperStatus0},
                          {adminstatus, AdminStatus0},
                          {cardacttype, Type}],
             %只有 4353 4354 等固定索引 才会是这个板卡
             PortIndices = case {Type,BoardIndex} of
                                    {"FANT-A",4353} -> [{35848192,1},{35880960,2},{35913728,3},{35946496,4}];
                                    {"FANT-A",4354} -> [{36012032,1},{36044800,2},{36077568,3},{36110336,4}];
                                    {"FANT-F",4353} -> [{35848192,1},{35880960,2},{35913728,3},{35946496,4}];
                                    {"FANT-F",4354} -> [{36012032,1},{36044800,2},{36077568,3},{36110336,4}];
                                    {"OANT-A",4353} -> [{1,1},{2,2},{3,3},{4,4}];
                                    {"OANT-A",4354} -> [{5,1},{6,2},{7,3},{8,4}];
                                    {"EXNT-A",4353} -> [{1,1},{2,2},{5,3},{6,4}];
                                    {"EXNT-A",4354} -> [{3,1},{4,2},{7,3},{8,4}];
                                    {"EHNT-B",4353} -> [{1,1},{2,2},{3,3},{4,4}];
                                    {"EHNT-B",4354} -> [{5,1},{6,2},{7,3},{8,4}];
                                                 _  -> []
                            end,
            Gei = create_gei(SlotNo,PortIndices,Dn),
            ?INFO("create bell olt gei: ~p,~p", [Ip, Gei]),
            [{entry, board, list_to_binary(BoardDn), BoardAttrs}] ++ Gei
        end, Entries0),
        {ok, lists:flatten(Entries)};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.


create_gei(_,[],_) ->
        [];
create_gei(SlotNo,PortIndices,Dn) ->
      lists:map(fun({PortIndex,PortNo})->
                     PortName = lists:concat(["1-1-",SlotNo,"-",PortNo]),
                     PortDn =lists:concat(["port=",PortIndex,",",binary_to_list(Dn)]),
                     {entry, port, list_to_binary(PortDn),
                      [{port_name,PortName},{ponid,PortName},{port_index, PortIndex},
                       {port_no, PortNo},{slot_no, SlotNo},{port_category, 2},{port_type, "gei"}]}
                  end,PortIndices).


disco_ports(Dn, Ip, AgentData,Olt) ->
    case snmp_mapping:get_table(Ip, [?IfDescr, ?IfType, ?IfAdminStatus, ?IfOperStatus], AgentData) of
    {ok, Rows} ->
        Entries0 =lists:filter(fun(Row) ->
            {value, IfType} = dataset:get_value(ifType, Row),
             (IfType == 220) or (IfType == 238) or (IfType == 250) %采集的都是pon口
        end, Rows),
        Entries = lists:map(fun(Row) ->
            {value, [IfIndex]} = dataset:get_value('$tableIndex', Row),
            {SlotNo, PortNo} = calc_port_no(IfIndex,Olt),
            case {SlotNo, PortNo} of
                    {0,0} -> [];
                        _ ->
                            {value, IfDescr} = dataset:get_value(ifDescr, Row),
                            {value, IfAdminStatus} = dataset:get_value(ifAdminStatus, Row),
                            {value, IfOperStatus} = dataset:get_value(ifOperStatus, Row),
                            {value, IfType} = dataset:get_value(ifType, Row),
                            {value, PortType} = dataset:get_value(IfType, ?GEpon),
                            PortDn = "port=" ++ integer_to_list(IfIndex) ++ "," ++ binary_to_list(Dn),
                			PortName = lists:concat(["1-1-",SlotNo,"-",PortNo]),
                			?INFO("ponid: ~p", [PortName]),
                            {entry, port, list_to_binary(PortDn),
                                [{port_name,PortName},{ponid,PortName},{port_index, IfIndex},{port_desc, IfDescr},
                                 {port_no, PortNo},{slot_no, SlotNo},{port_category, 1},{port_type, PortType},
                                 {admin_status, IfAdminStatus},{oper_status, IfOperStatus}]}
            end
        end,Entries0),
		{ok,disco_util:lists_delete_null(Entries)};
    {error, Reason} ->
       ?WARNING("~p", [Reason]),
        {ok,[]}
    end.

disco_onus(Dn, Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, ?Onus, AgentData) of
    {ok, Rows} ->
        ?INFO("disco bell olt onus: ~p,~p", [Ip, Rows]),
        Entries = lists:map(fun(Row) ->
            {value, [Idx]} = dataset:get_value('$tableIndex', Row),
		    {value, Mac0} = dataset:get_value(mac, Row),
            Mac = monet_util:macaddr(Mac0),
		    {value, HardVer} = dataset:get_value(hardversion, Row),
		    {value, SoftVer} = dataset:get_value(softversion, Row),
            OnuType = case SoftVer of
                "R210" -> "7330";
                     _ -> "7353"
             end,
		    {value, OnlineState0} = dataset:get_value(onlinestate, Row),
			OnlineState = disco_util:lookup("bell",<<"all">>,<<"onu">>,<<"oper">>,OnlineState0),
            {SlotNo, PortNo, OnuNo} = calc_onu_no(Idx),
            Rdn = integer_to_list(Idx),
            OnuDn = "onu=" ++ Rdn ++ "," ++ binary_to_list(Dn),
			Ponid = lists:concat([lists:concat(["1-1-",SlotNo,"-",PortNo])]),
            {entry, onu, list_to_binary(OnuDn), [{ponid,Ponid},
                                {rdn, Rdn}, {vendor, <<"bell">>},
                                {type, OnuType},  {snmp_v, "v2c"},
                                {slot_no, SlotNo}, {port_no, PortNo}, {onu_no, OnuNo}, {operstate,OnlineState},{onlinestatus, OnlineState},
                                {macaddr, Mac}, {hardwareversion, HardVer}, {softwareversion, SoftVer}]}
        end, Rows),
		?INFO("bell onu : ~p", [Entries]),
        {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.


disco_gonus(Dn, Ip, AgentData,Olt) ->
    case snmp_mapping:get_table(Ip, ?GOnus, AgentData) of
    {ok, Rows} ->
        ?INFO("disco bell olt gonus: ~p,~p", [Ip, Rows]),
        Entries = lists:map(fun(Row) ->
            {value, [Idx]} = dataset:get_value('$tableIndex', Row),
            {SlotNo, PortNo, OnuNo} = calc_gonu_no(Idx,Olt),
            case {SlotNo, PortNo, OnuNo} of
                    {0,0,0} -> [];
                    _ ->
            		    {value, HardVer} = dataset:get_value(hardversion, Row),
            		    {value, SoftVer} = dataset:get_value(softversion, Row),
            		    {value, OnuType} = dataset:get_value(type, Row),
            		    {value, AuthPassword} = dataset:get_value(authpassword, Row),
            		    {value, OnlineState} = dataset:get_value(operstate, Row),
            		    {value, AdminStatus} = dataset:get_value(adminstate, Row),
            			AdminState = disco_util:lookup("bell",<<"all">>,<<"gonu">>,<<"admin">>,AdminStatus),
                        ?INFO("disco bell index: ~p,slotno: ~p,portno: ~p,onuno: ~p", [Idx,SlotNo, PortNo, OnuNo]),
                        Rdn = integer_to_list(Idx),
                        OnuDn = "onu=" ++ Rdn ++ "," ++ binary_to_list(Dn),
            			Ponid = lists:concat([lists:concat(["1-1-",SlotNo,"-",PortNo])]),
                        {entry, onu, list_to_binary(OnuDn),
                            [{ponid,Ponid},{rdn, Rdn}, {vendor, <<"bell">>},{authpassword,AuthPassword},
                            {type, onutype(string:strip(OnuType,both,$_))},  {snmp_v, "v2c"},{adminstate,AdminState},
                            {slot_no, SlotNo}, {port_no, PortNo}, {onu_no, OnuNo}, {operstate,OnlineState},{onlinestatus, OnlineState},
                            {hardwareversion, HardVer}, {softwareversion, SoftVer}]}
            end
        end, Rows),
		?INFO("bell gonu : ~p", [Entries]),
        {ok, disco_util:lists_delete_null(Entries)};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.

onutype([$B|_]) ->
    "7353";
onutype(Type) ->
    Type.


disco_ont_ports(Dn, Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, [?LanAdminstate, ?LanOperstate], AgentData) of
    {ok, Rows} ->
        ?INFO("feRows ~p", [Rows]),
        {_,LanDicts} = lists:mapfoldl(fun(Row,Dict) ->
            {value, [OnuRdn,PortIdx]} = dataset:get_value('$tableIndex', Row),
            {value, AdminStatus} = dataset:get_value(adminstate, Row),
            {value, OperStatus} = dataset:get_value(operstate, Row),
            OnuDn = "onu=" ++ integer_to_list(OnuRdn) ++ "," ++ to_list(Dn),
            {OntSlotNo,OntPortNO} = calc_gonu_ontport(PortIdx),
            PortName =lists:concat(["NA-NA-",OntSlotNo,"-",OntPortNO]),
            ?INFO("index ~p ,fe ~p,~p", [PortIdx,OnuRdn,PortName]),
            PortAttr = [{port_name, PortName},{port_type,"feElcPort"},{ponid,PortName},{port_index,PortIdx},{port_desc,"ftth"},
            {port_category,3},{port_no, OntPortNO},{slot_no, OntSlotNo} ,
            {admin_status,disco_util:lookup("bell",<<"all">>,<<"onu_lan">>,<<"admin">>,AdminStatus)},
            {oper_status, disco_util:lookup("bell",<<"all">>,<<"onu_lan">>,<<"admin">>,OperStatus)}],
			{PortAttr,dict:append(OnuDn,PortAttr,Dict)}
        end,dict:new(), Rows),
        {ok, Dicts} = get_pstn_port(Dn, Ip, AgentData,LanDicts),
		Entries = [{entry,ports,Odn,Ports} || {Odn,Ports} <- dict:to_list(Dicts)],
		?INFO("~p", [Entries]),
        {ok,Entries};
    {error, Reason} ->
       ?WARNING("~p, ~p", [Ip, Reason]),
        {ok,[]}
    end.

get_pstn_port(Dn, Ip, AgentData,LanDicts)->
    case snmp_mapping:get_table(Ip, [?PstnAdminstate, ?PstnOperstate], AgentData) of
    {ok, Rows} ->
        ?INFO("pstn rows~p", [length(Rows)]),
        {_,Dicts} = lists:mapfoldl(fun(Row,Dict) ->
            {value, [OnuRdn,PortIdx]} = dataset:get_value('$tableIndex', Row),
            {value, AdminStatus} = dataset:get_value(adminstate, Row),
            {value, OperStatus} = dataset:get_value(operstate, Row),
            OnuDn = "onu=" ++ integer_to_list(OnuRdn) ++ "," ++ to_list(Dn),
            {OntSlotNo,OntPortNO} = calc_gonu_ontport(PortIdx),
            PortName =lists:concat(["NA-NA-",OntSlotNo,"-",OntPortNO]),
            ?INFO("index ~p ,voip ~p,~p", [PortIdx,OnuRdn,PortName]),
            PortAttr = [{port_name, PortName},{port_type,"pstnPort"},{ponid,PortName},{port_index,PortIdx},{port_desc,"ftth"},
            {port_category,3},{port_no, OntPortNO},{slot_no, OntSlotNo} ,
            {admin_status,disco_util:lookup("bell",<<"all">>,<<"onu_voip">>,<<"admin">>,AdminStatus)},
            {oper_status, disco_util:lookup("bell",<<"all">>,<<"onu_voip">>,<<"admin">>,OperStatus)}],
			{PortAttr,dict:append(OnuDn,PortAttr,Dict)}
        end,LanDicts, Rows),
        {ok, Dicts};
    {error, Reason} ->
       ?WARNING("~p, ~p", [Ip, Reason]),
        {ok,LanDicts}
    end.


calc_board_no(IfIndex) ->  %gong you
    SlotNo = (IfIndex band 16#00FF),
    SlotNo.

calc_port_no(IfIndex,Olt) ->  %gong you
    {value, Type} = dataset:get_value(type, Olt),
    {value, Softwareversion} = dataset:get_value(softversion, Olt),
    case Type of
         "7342" ->
            SlotNo = ((IfIndex bsr 16)  band 16#00FF),
            PortNo = (IfIndex band 16#0FFF)+1,
            {SlotNo, PortNo};
         "7360" ->
             case string:str(Softwareversion,"V4.3") of
                        0 -> %R4.4
                            PortNo = ((IfIndex bsr 17)  band 15)+1,
                            SlotNo = ((IfIndex bsr 25)  band 63)+1,
                            {SlotNo, PortNo};
                        _ -> %R4.3
                            PortNo = ((IfIndex bsr 18)  band 7) + 1,
                            SlotNo = ((IfIndex bsr 25)  band 63) + 1,
                            {SlotNo, PortNo}
             end;
         _ -> {0,0}
    end.

calc_onu_no(OnuIndex)  ->
    SlotNo = ((OnuIndex bsr 20)  band 16#0F) + 1,
    PortNo = ((OnuIndex bsr 8)  band 16#FF) div 16#4 + 1,
    OnuNo = (OnuIndex band 16#00FF) + 1,
    {SlotNo, PortNo, OnuNo}.

calc_gonu_no(OnuIndex,Olt)  ->
    {value, Type} = dataset:get_value(type, Olt),
    {value, Softwareversion} = dataset:get_value(softversion, Olt),
    case Type of
         "7342" ->
             SlotNo = ((OnuIndex bsr 20)  band 31),
             PortNo = ((OnuIndex bsr 14)  band 3) + 1,
             OnuNo = ((OnuIndex bsr 8)  band 63) + 1,
             {SlotNo, PortNo, OnuNo};
         "7360" ->
             case string:str(Softwareversion,"V4.3") of
                        0 -> %R4.4
                            OnuNo = ((OnuIndex bsr 10)  band 127)+1,
                            PortNo = ((OnuIndex bsr 17)  band 15)+1,
                            SlotNo = ((OnuIndex bsr 25)  band 63)+1,
                            {SlotNo, PortNo,OnuNo};
                        _ -> %R4.3
                            OnuNo = ((OnuIndex bsr 10)  band 127) + 1,
                            PortNo = ((OnuIndex bsr 18)  band 7) + 1,
                            SlotNo = ((OnuIndex bsr 25)  band 63) + 1,
                            {SlotNo, PortNo,OnuNo}
             end;
         _ -> {0,0,0}
    end.

calc_gonu_ontport(PortIndex)  ->
     SlotNo = ((PortIndex bsr 22)  band 31),
     PortNo = ((PortIndex bsr 16)  band 31),
     {SlotNo, PortNo}.

% calc_port_info(PortData) ->
%     calc_port_info(PortData, []).
%
% calc_port_info([], Data) ->
%     Data;
% calc_port_info([{led_power, LedPower}|PortData], NewData) ->
%     LedPower2 =  if LedPower > 0 ->
%             math:log10(LedPower/10000) * 10;
%             true -> LedPower
%     end,
%     [LedPower3] = io_lib:format("~.2f", [LedPower2 + 0.0]),
%     calc_port_info(PortData, [{led_power, LedPower3}|NewData]);
% calc_port_info([{received_power, Power}|PortData], NewData) ->
%     Power2 =  if Power > 0 ->
%             math:log10(Power/10000) * 10;
%             true -> Power
%     end,
%     [Power3] = io_lib:format("~.2f", [Power2 + 0.0]),
%     calc_port_info(PortData, [{received_power, Power3}|NewData]);
% calc_port_info([{temperature, Data}|PortData], NewData) ->
%     [Data2] = io_lib:format("~.2f", [Data/500 + 0.0]),
%     calc_port_info(PortData, [{temperature, Data2}|NewData]);
% calc_port_info([{e_current, Data}|PortData], NewData) ->
%     [Data2] = io_lib:format("~.2f", [Data/256 + 0.0]),
%     calc_port_info(PortData, [{e_current, Data2}|NewData]);
%  calc_port_info([T|PortData], NewData) ->
%      calc_port_info(PortData, [T|NewData]).
