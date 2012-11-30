-module(disco_zte_onu_f820).

-include_lib("elog/include/elog.hrl").

-include("snmp/zte_onu.hrl").

-include("snmp/rfc1213.hrl").

-define(PON, 202).
-define(GE, 201).
-define(FE, 200).

-define(BoardEntry, [
    ?BoardType2,
    ?BoardAdminStatus,
    ?BoardOperStatus,
    ?BoardHardVersion,
    ?BoardSoftVersion,
    ?BoardCpuLoad,
    ?BoardMemUsage
]).

-export([disco/4,disco_ports/3,disco_self/3]).

-import(extbif,[to_list/1, to_integer/1]).

disco(Dn, Ip, AgentData, _Args) ->
	{ok,OnuInfo} = disco_self(Dn, Ip, AgentData),
    ?INFO("begin to disco onu: ~p", [Ip]),
    {BoardClass,Boards} = disco_boards(Dn, Ip, AgentData),
   ?INFO("Boards: ~p", [Boards]),
    Ports = disco_ports(Ip, BoardClass, AgentData),
    {ok, OnuInfo, [{entry,boards,Dn,Boards}] ++ [{entry,ports,Dn,Ports}]}.

disco_self(_Dn, Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, disco_util:map2oid([?F820NarrowIp]), AgentData) of
        {ok, Rows} ->
            NarrowIp = lists:foldl(fun(Row, AccIp) ->
                {value, IpB} = dataset:get_value(narrowIp, Row),
                IpA = mib_formatter:ip(IpB),
                if  AccIp == [] ->
                    IpA;
                  true ->
                    list_to_binary([AccIp, ",", IpA])
                end
            end, [], Rows),
            {ok,[{narrow_ip, NarrowIp}]};
        {error, Reason2} ->
            ?WARNING("~p, ~p", [Ip, Reason2]),
            {ok,[]}
    end.

disco_boards(_Dn, Ip, AgentData) ->
    ?INFO("disco olt boards: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?BoardEntry, AgentData) of
    {ok, Rows} ->
       lists:mapfoldl(fun(Row, Data) ->
            {value, [_ZxAnRackNo, _ZxAnShelfNo, ZxAnSlotNo]} = dataset:get_value('$tableIndex', Row),
            Row1 = lists:keydelete('$tableIndex', 1, Row),
            Boardid = lists:concat(["1-1-",ZxAnSlotNo]),
            {value, BoardType} = dataset:get_value(boardtype, Row1),
            {value, BoardClass} = dataset:get_value(BoardType, ?F820BoardType, unknow),
            Row2 = lists:keyreplace(boardtype, 1, Row1, {cardacttype, BoardType}),
            { {ZxAnSlotNo, BoardClass}, [[{slot_no, ZxAnSlotNo},{boardid,Boardid} | Row2] | Data]}
        end, [], Rows);
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {[],[]}
    end.

disco_ports(Ip, BoardClass, AgentData) ->
	?INFO("ip===: ~p", [Ip]),
    FePort= disco_fe_ports(Ip, BoardClass, AgentData),
	   ?INFO("feports: ~p", [FePort]),
    PstnPort= disco_pstn_ports( Ip, BoardClass, AgentData),
	   ?INFO("pstnports: ~p", [PstnPort]),
    FePort ++ PstnPort.

disco_fe_ports(Ip, BoardClass, AgentData) ->
    case snmp_mapping:get_table(Ip, ?IfEntry -- [?IfIndex], AgentData) of
    {ok, Rows} ->
        Entries = lists:map(fun(Row) ->
            {value, [IfIndex]} = dataset:get_value('$tableIndex', Row),
            {SlotNo, PortNo} = calc_no(IfIndex),
            {value, PortClass} = dataset:get_value(SlotNo, BoardClass, unknow),
            Row2 = lists:keyreplace(ifType, 1, Row, {ifType, PortClass}),
            Row3 = lists:keydelete('$tableIndex', 1, Row2),
            Row4 = change_port_state(Row3),
            PortName = lists:concat(["1-1-",SlotNo,"-",PortNo]),
            [{port_name,PortName},{ponid,PortName},{port_index, IfIndex}, {port_category, 3}, {port_no, PortNo}, {slot_no, SlotNo} | disco_util:transform_port(Row4)]
        end, Rows),
        Entries;
    {error, Reason} ->
       ?WARNING("~p", [Reason]),
        []
    end.

disco_pstn_ports(Ip, BoardClass, AgentData) ->
         case lists:keysearch('pstnPort', 2, BoardClass) of
             {value, _} ->
                case snmp_mapping:get_table(Ip, [?F820PortStatus], AgentData) of
                     {ok, Entries} ->
                            lists:map(fun(Entry) ->
                                 {value, [IfIndex]} = dataset:get_value('$tableIndex', Entry),
                                 {SlotNo, PortNo} = calc_pstn_no(IfIndex),
                                 {value, PstnStatus} = dataset:get_value(portStatus, Entry),
                                  PortStatus = [{oper_status, disco_util:lookup("zte",<<"all">>,<<"onu_voip">>,<<"oper">>,PstnStatus)}],
 					              PortName = lists:concat(["1-1-",SlotNo,"-",PortNo]),
                                  [{port_name,PortName},{ponid,PortName},{port_index, IfIndex}, {port_type, pstnPort}, {port_category, 3}, {port_no, PortNo}, {slot_no, SlotNo}, {port_desc, PortName}|PortStatus]                             end, Entries);
                     {error, _} -> []
                 end;
              _ -> []
         end.

change_port_state(Row) ->
    {value, OperStatus0} = dataset:get_value(ifOperStatus, Row),
    {value, AdminStatus0} = dataset:get_value(ifAdminStatus, Row),
	 OperStatus = disco_util:lookup("zte",<<"all">>,<<"onu_adsl">>,<<"oper">>,OperStatus0),
     AdminStatus = disco_util:lookup("zte",<<"all">>,<<"onu_adsl">>,<<"admin">>,AdminStatus0),
     PortAttr1 = lists:keyreplace(ifOperStatus, 1, Row, {ifOperStatus, OperStatus}),
     lists:keyreplace(ifAdminStatus, 1, PortAttr1, {ifAdminStatus, AdminStatus}).

calc_no(IfIndex) ->
    PortNo = (IfIndex  bsr 8) band 16#00FF ,
    SlotNo = (IfIndex bsr 16) band 16#00FF ,
   {SlotNo + 1, PortNo + 1}.

calc_pstn_no(IfIndex) ->
    PortNo = (IfIndex  bsr 8) band 16#00FF ,
    SlotNo = (IfIndex bsr 16) band 16#00FF ,
   {SlotNo, PortNo}.
