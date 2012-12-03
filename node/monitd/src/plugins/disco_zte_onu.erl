-module(disco_zte_onu).

-include_lib("elog/include/elog.hrl").

-include("snmp/zte_onu.hrl").

-include("snmp/rfc1213.hrl").

-define(PON, 202).
-define(GE, 201).
-define(FE, 200).

-define(zxDslBoardEntry, [
    ?zxDslBoardType,
    ?zxDslBoardAdminStatus,
    ?zxDslBoardOperStatus,
    ?zxDslBoardHardVersion,
    ?zxDslBoardSoftVersion,
    ?zxDslBoardCpuLoad,
    ?zxDslBoardMemUsage
]).

-export([disco/4,disco_ports/3,disco_self/3]).

-import(extbif,[to_list/1, to_integer/1]).

disco(Dn, Ip, AgentData, _Args) ->
	{ok,OnuInfo} = disco_self(Dn, Ip, AgentData),
    ?INFO("begin to disco onu: ~p", [Ip]),
    {BoardClass,Boards} = disco_boards(Dn, Ip, AgentData),
   ?INFO("Boards: ~p", [Boards]),
    Ports = disco_ports(Ip, BoardClass, AgentData),
    {ok, OnuInfo, [{board, Dn, Boards}, {port, Dn, Ports}]}.

disco_self(_Dn, Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, [?ZteNarrowIp], AgentData) of
        {ok, Rows} ->
		    ?INFO("  disco self onu Rows: ~p", [Rows]),
			Rows1 = lists:filter(fun(Row)->
				{value,NType}=dataset:get_value(narrowIp, Row),
				to_integer(NType)==3
			end,Rows),
            NarrowIp = lists:foldl(fun(Row, AccIp) ->
                {value, IpB} = dataset:get_value('$tableIndex', Row),
                IpA = mib_formatter:ip(IpB),
                if  AccIp == [] ->
                    IpA;
                  true ->
                    list_to_binary([AccIp, ",", IpA])
                end
            end, [], Rows1),
            {ok,[{narrow_ip, NarrowIp}]};
        {error, Reason2} ->
            ?WARNING("~p, ~p", [Ip, Reason2]),
            {ok,[]}
    end.


disco_boards(_Dn, Ip, AgentData) ->
    ?INFO("disco olt boards: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?zxDslBoardEntry, AgentData) of
    {ok, Rows} ->
       lists:mapfoldl(fun(Row, Data) ->
            {value, [_ZxAnRackNo, _ZxAnShelfNo, ZxAnSlotNo]} = dataset:get_value('$tableIndex', Row),
            Row1 = lists:keydelete('$tableIndex', 1, Row),
            Boardid = lists:concat(["1-1-",ZxAnSlotNo]),
            {value, OperStatus0} = dataset:get_value(operstatus, Row1),
            {value, AdminStatus0} = dataset:get_value(adminstatus, Row),
            OperStatus = disco_util:lookup("zte",<<"all">>,<<"onu_board">>,<<"oper">>,OperStatus0),
            AdminStatus = disco_util:lookup("zte",<<"all">>,<<"onu_board">>,<<"admin">>,AdminStatus0),
            {value, BoardType} = dataset:get_value(boardtype, Row1),
            {BoardType1, BoardClass} = case lists:keysearch(BoardType, 1, ?BoardType) of
                {value, {_, Value, Class}} ->
                    {Value, Class};
                false ->
                    {BoardType, unknow}
            end,
            Row2 = lists:keyreplace(boardtype, 1, Row1, {cardacttype, BoardType1}),
            Row3 = lists:keyreplace(operstatus, 1, Row2, {operstatus, OperStatus}),
            Row4 = lists:keyreplace(adminstatus, 1, Row3, {adminstatus, AdminStatus}),
            { {ZxAnSlotNo, BoardClass}, [[{slot_no, ZxAnSlotNo},{boardid,Boardid} | Row4]| Data]}
        end, [], Rows);
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {[],[]}
    end.

disco_ports(Ip, BoardClass, AgentData) ->
	?INFO("ip===: ~p", [Ip]),
    FePort= disco_fe_ports(Ip, BoardClass, AgentData),
	   ?INFO("feports: ~p", [FePort]),
    PstnPort= disco_pstn_ports(Ip, BoardClass, AgentData),
	   ?INFO("pstnports: ~p", [PstnPort]),
    FePort ++ PstnPort.

disco_fe_ports( Ip, BoardClass, AgentData) ->
    case snmp_mapping:get_table(Ip, ?IfEntry -- [?IfIndex], AgentData) of
    {ok, Rows} ->
        %adsl 上下行带宽模板
        {ok, Profiles} = case lists:keysearch('adslPort', 2, BoardClass) of
            {value, _BoardType} ->
                get_profile(Ip, AgentData);
             false ->
                 {ok, []}
             end,
        Entries = lists:map(fun(Row) ->
            {value, [IfIndex]} = dataset:get_value('$tableIndex', Row),
            {SlotNo, PortNo} = calc_no(IfIndex),
            {value, PortClass} = dataset:get_value(SlotNo, BoardClass, unknow),
            {PortType, PortData} = case PortClass of
                adslPort -> %adsl 端口取上下行带宽
                    case snmp_mapping:get_entry(Ip, disco_util:map2oid([?adslLineConfProfile]), [IfIndex], AgentData) of
                         {ok, Entry} ->
                                 {value, Profile} = dataset:get_value(adslLineConfProfile, Entry),
                                 {3, proplists:get_value(Profile, Profiles, []) };
                        {error, Reason} ->
                            ?WARNING("~p", [Reason]),
                            {3, []}
                        end;
                 geElcPort ->
                     {2, []};
                _ ->
                    {3, []}
            end,
            Row1 = Row ++ PortData,
            Row2 = lists:keyreplace(ifType, 1, Row1, {ifType, to_list(PortClass)}),
            Row3 = lists:keydelete('$tableIndex', 1, Row2),
            Row4 = change_port_state(Row3),
            PortName = lists:concat(["1-1-",SlotNo,"-",PortNo]),
            [{port_name,PortName},{ponid,PortName},{port_index, IfIndex}, {port_category, PortType}, {port_no, PortNo}, {slot_no, SlotNo} | disco_util:transform_port(Row4)]
        end, Rows),
        Entries;
    {error, Reason} ->
       ?WARNING("~p", [Reason]),
        []
    end.

disco_pstn_ports(Ip, BoardClass, AgentData) ->
    lists:foldl(fun({SlotNo, BoardClassName}, PstnPorts) ->
         case BoardClassName of
            pstnPort ->
                PstnStatusOid = [{Name, Oid ++ [to_integer(SlotNo)]} || {Name, Oid} <- disco_util:map2oid([?PortStatus])],
                FisrtPstnport = case snmp_mapping:get_entry(Ip, PstnStatusOid, [0], AgentData) of
                    {ok, Entry} ->
                         [get_pstn_port( SlotNo, 0, Entry)];
                    {error, Reason} ->
                        ?WARNING("~p", [Reason]),
                        []
                end,
                case snmp_mapping:get_table(Ip, PstnStatusOid, AgentData) of
                     {ok, Entries} ->
                            NewPorts = lists:map(fun(Entry) ->
                                 {value, [PortNo]} = dataset:get_value('$tableIndex', Entry),
                                 get_pstn_port(SlotNo, PortNo, Entry)
                             end, Entries),
                             PstnPorts ++ FisrtPstnport ++ NewPorts;
                    {error, _} ->
                        []
                    end;
            _ ->
                PstnPorts
        end
    end, [], BoardClass).

get_pstn_port( SlotNo, PortNo, Entry) ->
      PortIndex = 10000 * SlotNo + PortNo,
      PortName = lists:concat(["1-1-",SlotNo,"-",PortNo]),
     {value, PstnStatus} = dataset:get_value(portStatus, Entry),
      PortStatus = calc_status(PstnStatus),
      [{port_name,PortName},{ponid,PortName},{port_index, PortIndex}, {port_type, pstnPort}, {port_category, 3}, {port_no, PortNo}, {slot_no, SlotNo}|PortStatus].
get_profile(Ip, AgentData) ->
    DbwOid = [?adslAtucChanConfInterleaveMaxTxRate, ?adslAturChanConfInterleaveMaxTxRate],
    case snmp_mapping:get_table(Ip, DbwOid, AgentData, 10000) of
    {ok, Rows} ->
        Entries = lists:map(fun(Row) ->
            {value, [_|Profile]} = dataset:get_value('$tableIndex', Row),
            {value, Downmaximumbw} = dataset:get_value(adslAtucMaxTxRate, Row),
            {value, Upmaximumbw} = dataset:get_value(adslAturMaxTxRate, Row),
            {Profile, [{downmaximumbw, Downmaximumbw * 1000},{upmaximumbw, Upmaximumbw * 1000}]}
        end, Rows),
        {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.


change_port_state(Row) ->
    {value, OperStatus0} = dataset:get_value(ifOperStatus, Row),
    {value, AdminStatus0} = dataset:get_value(ifAdminStatus, Row),
     OperStatus = disco_util:lookup("zte",<<"all">>,<<"onu_adsl">>,<<"oper">>,OperStatus0),
     AdminStatus = disco_util:lookup("zte",<<"all">>,<<"onu_adsl">>,<<"admin">>,AdminStatus0),
     PortAttr1 = lists:keyreplace(ifOperStatus, 1, Row, {ifOperStatus, OperStatus}),
     lists:keyreplace(ifAdminStatus, 1, PortAttr1, {ifAdminStatus, AdminStatus}).

calc_no(IfIndex) ->
    PortNo = IfIndex  rem 24 ,
    SlotNo = IfIndex div 24 ,
    if PortNo == 0 ->
        {SlotNo - 1, PortNo + 24};
        true ->
            {SlotNo, PortNo}
        end.

calc_status(Status) ->
    OperStatus =  (Status band 16#01)+1,
    Status1 = Status band 16#FF20,
    AdminStatus = case ((Status1 bsr 5) band 16#01) of
        0 -> 2;
        1 ->
            case (Status1 bsr 3) of
                0 -> 2;
                16#4 -> 3;
                16#8 -> 6;
                16#10 -> 7;
                16#20 -> 8;
                16#40 -> 5;
                16#80 -> 9;
                _ -> 99
             end
      end,
      [{admin_status, disco_util:lookup("zte",<<"all">>,<<"onu_voip">>,<<"admin">>,AdminStatus)},
		{oper_status, disco_util:lookup("zte",<<"all">>,<<"onu_voip">>,<<"oper">>,OperStatus)}].
