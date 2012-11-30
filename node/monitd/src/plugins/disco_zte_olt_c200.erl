-module(disco_zte_olt_c200).

-include_lib("elog/include/elog.hrl").

-include("snmp/zte.hrl").

-include("snmp/rfc1213.hrl").

-define(PON, 202).
-define(GE, 201).
-define(FE, 200).

-define(ETH, "ethernet-csmacd").
-define(EPON, "epon").
-import(extbif, [to_list/1]).

-export([disco/4, disco_boards/3, disco_ports/3, disco_onus/3, calc_no/1,disco_ont_ports/3]).

disco(Dn, Ip, Agent, _Args) ->
    ?INFO("begin to disco olt: ~p", [Ip]),
    {ok, Boards} = disco_boards(Dn, Ip, Agent),
    ?DEBUG("Boards: ~p", [Boards]),
    {ok, Ports} = disco_ports(Dn, Ip, Agent),
    ?DEBUG("Ports: ~p", [Ports]),
    {ok, Onus} = disco_onus(Dn, Ip, Agent),
    ?DEBUG("Onus: ~p", [Onus]),
    {ok, OntPorts} = disco_ont_ports(Dn, Ip, Agent),
    ?INFO("OntPorts: ~p", [OntPorts]),
    {ok, [], Boards ++ Ports ++ Onus ++ OntPorts}.

disco_boards(Dn, Ip, Agent) ->
    ?INFO("disco olt boards: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?zxAnCard, Agent) of
    {ok, Rows} ->
        Boards = lists:map(fun(Row) ->
            {value, [ZxAnRackNo, ZxAnShelfNo, ZxAnSlotNo]} = dataset:get_value('$tableIndex', Row),
            Row1 = lists:keydelete('$tableIndex', 1, Row),
			Boardid = lists:concat(["1-1-",ZxAnSlotNo]),
            BoardDn = lists:concat([to_list(Dn), ",board=", Boardid]),
            {value, OperStatus0} = dataset:get_value(operstatus, Row1),
            {value, AdminStatus0} = dataset:get_value(adminstatus, Row1),
            %与上面的walk值不一致，少状态不在线数据
           Versions =  case snmp_mapping:get_entry(Ip, ?zxAnCardVersion, [ZxAnRackNo, ZxAnShelfNo, ZxAnSlotNo], Agent) of
                {ok, Version} ->
                    lists:filter(fun({Name, Value}) ->
                        (Name =/= tableIndex) and (Value =/= noSuchInstance)
                    end, Version);
                 {error, Reason} ->
                        ?WARNING("~p", [Reason]),
                        []
                    end,
            OperStatus = disco_util:lookup("zte",<<"all">>,<<"olt_board">>,<<"oper">>,OperStatus0),
            AdminStatus = disco_util:lookup("zte",<<"all">>,<<"olt_board">>,<<"admin">>,AdminStatus0),
            Row2 = lists:keyreplace(operstatus, 1, Row1, {operstatus, OperStatus}),
            Row3 = lists:keyreplace(adminstatus, 1, Row2, {adminstatus, AdminStatus}),
            Row4 = Row3 ++ Versions,
            [{board_dn, list_to_binary(BoardDn)}, {slot_no, ZxAnSlotNo}, {boardid,Boardid} | Row4]
        end, Rows),
        {ok, [{board, Dn, Boards}]};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.

disco_ports(Dn, Ip, Agent) ->
    case snmp_mapping:get_table(Ip, ?IfEntry, Agent) of
    {ok, Rows} ->
        Ports = lists:map(fun(Row) ->
            {value, IfIndex} = dataset:get_value(ifIndex, Row),
            {value, IfDescr} = dataset:get_value(ifDescr, Row),
            {value, IfType} = dataset:get_value(ifType, Row),
            {value, IfSpeed} = dataset:get_value(ifSpeed, Row),
            PortType = case string:sub_string(IfDescr, 1, 4) of
                "gei" ++ _ ->
                    ?GE;
                 "epon" ->
                     ?PON;
				"gpon" ->
                     ?PON;
                 _ ->
                    0
             end,
            IfTypeStr = case IfType of
                6 -> ?ETH;
                300 -> ?EPON;
     			_ ->
                    IfType
			end,
            {SlotNo, PortNo,_OnuNo} = calc_no(IfIndex),
             %?INFO("after_port,SlotNo: ~p, PortNo:~p ,~p",[SlotNo, PortNo,Row]),
			 {value, OperStatus0} = dataset:get_value(ifOperStatus, Row),
	         {value, AdminStatus0} = dataset:get_value(ifAdminStatus, Row),
	         OperStatus = disco_util:lookup("zte",<<"all">>,<<"olt_pon">>,<<"oper">>,OperStatus0),
	         AdminStatus = disco_util:lookup("zte",<<"all">>,<<"olt_pon">>,<<"admin">>,AdminStatus0),
            PortName = lists:concat(["1-1-",SlotNo,"-",PortNo]),
            Rdn = "port=" ++ integer_to_list(IfIndex),
            [{rdn, Rdn}, {port_no, PortNo},{port_index,IfIndex}, {port_category, PortType},
			 {port_type, IfTypeStr}, {slot_no, SlotNo},{port_name,PortName},{ponid,PortName},
 			 {admin_status,AdminStatus},{oper_status,OperStatus},{speed,IfSpeed},{port_desc,IfDescr}]
        end, Rows),
        ?INFO("zte port : ~p", [Ports]),
        {ok, [{port, Dn, Ports}]};
    {error, Reason} ->
       ?WARNING("~p", [Reason]),
        {ok,[]}
    end.

disco_onus(Dn, Ip, Agent) ->
    ?INFO("disco olt onus: ~p", [Ip]),
    MibOids = ?zxAnOnu ++ ?slaUp ++ ?slaDown ++ ?onuVerTable ++ ?onuIpTable,
    case snmp_mapping:get_table(Ip, disco_util:map2oid(MibOids), Agent) of
    {ok, Rows} ->
        Onus = lists:map(fun(Row) ->
            {value, [Idx]} = dataset:get_value('$tableIndex', Row),
            OidIdx = integer_to_list(Idx),
		    {value, UserInfo} = dataset:get_value(userinfo, Row),
            {SlotNo, PortNo, OnuNo} = calc_no(Idx),
            %?INFO("after_onu,SlotNo: ~p, PortNo :~p",[SlotNo, PortNo]),
            Row1 = transform(lists:keydelete('$tableIndex', 1, Row)),
            %{value, AuthSn} = dataset:get_value(authmacsn, Row2),
            Rdn = integer_to_list(Idx),
			{value, Name0} = dataset:get_value(device_name, Row1),
            {value, AdminState0} = dataset:get_value(adminstate, Row1),
            AdminState = disco_util:lookup("zte",<<"all">>,<<"onu">>,<<"admin">>,AdminState0),
            Row3 = lists:keyreplace(adminstate, 1, Row1, {adminstate, AdminState}),
		    Row4 = case Name0 of
                "no description" -> lists:keyreplace(name, 1, Row3, {name, UserInfo});
     			_ ->
                    Row3
			end,
            {value, OnuType} = dataset:get_value(type, Row4),
             Row5 = lists:keydelete(type, 1, Row4),
             Row6 = lists:keydelete(onuModel, 1, Row5),
			PonId = lists:concat(["1-1-",SlotNo,"-",PortNo]),
             [{rdn, Rdn}, {oididx, OidIdx}, {discovery_state, 1},
             {slot_no, SlotNo}, {port_no, PortNo}, {onu_no, OnuNo},
             {type, OnuType},{ponid,PonId}| Row6]
        end, Rows),
		?INFO("zte onu : ~p", [Onus]),
        {ok, [{node, onus, Dn, Onus}]};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.

disco_ont_ports(Dn, Ip, Agent) ->
    case snmp_mapping:get_table(Ip, [?zxAnEponOnuPhyAdminState, ?zxAnEponOnuEthPortLinkState], Agent) of
    {ok, Rows} ->
        ?INFO("~p", [length(Rows)]),
        {_,FeDicts} = lists:mapfoldl(fun(Row,Dict) ->
            {value, [OnuRdn,OntPortNO]} = dataset:get_value('$tableIndex', Row),
            {value, AdminStatus} = dataset:get_value(adminstate, Row),
            {value, OperStatus} = dataset:get_value(operstate, Row),
            OnuDn = "onu=" ++ integer_to_list(OnuRdn) ++ "," ++ to_list(Dn),
            PortName =lists:concat(["NA-NA-NA-",OntPortNO]),
            PortAttr = [{port_name, PortName},{port_type,"feElcPort"},{ponid,PortName},{port_index,OntPortNO},{port_desc,"ftth"},
            {port_category,3},{port_no, OntPortNO},{slot_no, 0} ,{admin_status,AdminStatus},{oper_status, OperStatus}],
			{PortAttr,dict:append(OnuDn,PortAttr,Dict)}
        end,dict:new(), Rows),
       Dicts = get_pstn_port(Dn, Ip, Agent,FeDicts),
		Entries = [{port,Odn,Ports} || {Odn,Ports} <- dict:to_list(Dicts),length(Ports) < 8],
		?INFO("~p", [Entries]),
        {ok,Entries};
    {error, Reason} ->
       ?WARNING("~p, ~p", [Ip, Reason]),
        {ok,[]}
    end.

%TODO
get_pstn_port(Dn, Ip, Agent,FeDicts)->
    case snmp_mapping:get_table(Ip, [?zxAnEponOnuVoipPortEnable, ?zxAnEponRmVoipPortOperStatus], Agent) of
    {ok, Rows} ->
        ?INFO("~p", [length(Rows)]),
        {_,AllDicts} = lists:mapfoldl(fun(Row,Dict) ->
            {value, [OnuRdn,OntPort]} = dataset:get_value('$tableIndex', Row),%OntPort=16777217缺少解析
            {value, AdminStatus} = dataset:get_value(adminstate, Row),
            {value, OperStatus} = dataset:get_value(operstate, Row),
            OnuDn = "onu=" ++ integer_to_list(OnuRdn) ++ "," ++ to_list(Dn),
            PortNo = OntPort band 16#FF,
            PortName =lists:concat(["NA-NA-NA-",PortNo]),
            PortAttr = [{port_name, PortName},{port_type,"pstnPort"},{ponid,PortName},{port_index,OntPort},{port_desc,"ftth"},
                        {port_category,3},{port_no, PortNo},{slot_no, 1},
                        {admin_status,disco_util:lookup("zte",<<"all">>,<<"onu_voip">>,<<"admin">>,AdminStatus)},
                        {oper_status, disco_util:lookup("zte",<<"all">>,<<"onu_voip">>,<<"oper">>,OperStatus)}],
			{PortAttr,dict:append(OnuDn,PortAttr,Dict)}
        end,FeDicts, Rows),
        AllDicts;
    {error, Reason} ->
       ?WARNING("~p, ~p", [Ip, Reason]),
        FeDicts
    end.



transform(Onu) ->
    transform(Onu, []).

transform([{name, Name}|T], Acc) ->
    Name1 = string:strip(Name, both, $\$),
    case string:tokens(Name1, "$") of
    [_No, Name2] ->
        transform(T, [{device_name, Name2}|Acc]);
    [X|_] ->
        transform(T, [{device_name, X}|Acc]);
    [] ->
        transform(T, [{device_name, ""}|Acc])
    end;
transform([{onlinestatus, Status}| T], Acc) ->
    Status1 = disco_util:lookup("zte",<<"all">>,<<"onu">>,<<"oper">>,Status),
    transform(T, [{operstate,Status1} | Acc]);
transform([{adminstate, Status}| T], Acc) ->
    Status1 = disco_util:lookup("zte",<<"all">>,<<"onu">>,<<"admin">>,Status),
	transform(T, [{adminstate, Status1} | Acc]);
transform([{authmacsn, AuthMacSN}|T], Acc) ->
    case list_to_binary(AuthMacSN) of
    <<"O", _Branch:3/binary, Vendor:1/binary, _RackNo:2/binary, _ShelfNo:1/binary, _SlotNo:2/binary, _PortNo:2/binary,_OnuNo:4/binary,_CityNo:3/binary>> ->
        transform(T, [{vendor, vendor(Vendor)}, {authmacsn, AuthMacSN} | Acc]);
    <<"o", _Branch:3/binary, Vendor:1/binary, _RackNo:2/binary, _ShelfNo:1/binary, _SlotNo:2/binary, _PortNo:2/binary,_OnuNo:4/binary,_CityNo:3/binary>> ->
        transform(T, [{vendor, vendor(Vendor)}, {authmacsn, AuthMacSN} | Acc]);
    _ ->
        transform(T, [{vendor, <<"zte">>},{authmacsn, AuthMacSN} | Acc])
    end;

transform([{macaddr, MacAddr}|T], Acc) ->
    transform(T, [{macaddr, to_mac_str(MacAddr)} | Acc]);
transform([{authmacaddr, AuthMac}|T], Acc) ->
    transform(T, [{authmacaddr, to_mac_str(AuthMac)}|Acc]);
transform([{registermacaddress, RegMac}|T], Acc) ->
    transform(T, [{registermacaddress, to_mac_str(RegMac)}|Acc]);
transform([{ip, IpB}|T], Acc) ->
    Ip = to_ip_str(IpB),
    case Ip of
    <<"0.0.0.0">> -> %TODO: ftth onu, cannot be managed by snmp?
        transform(T, Acc);
    _ ->
        transform(T, [{ip, Ip}|Acc])
    end;
transform([{mask, Mask}|T], Acc) ->
    transform(T, [{mask, to_ip_str(Mask)}|Acc]);
transform([H|T], Acc) ->
    transform(T, [H|Acc]);
transform([], Acc) ->
    Acc.

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

to_ip_str(L) when is_list(L) and (length(L) == 4)->
	S = string:join(io_lib:format("~.10B~.10B~.10B~.10B", L), "."),
    list_to_binary(S);

to_ip_str(_) ->
    <<>>.

vendor(<<"Z">>) ->
    <<"zte">>;
vendor(<<"F">>) ->
    <<"fenghuo">>;
vendor(<<"U">>) ->
    <<"ut">>;
vendor(<<"z">>) ->
    <<"zte">>;
vendor(V) ->
    ?ERROR("unknown vendor: ~p", [V]),
    V.

calc_no(IfIndex) ->  %gong you
    Type = ((IfIndex bsr 28) band 16#00FF) ,
    case Type of
        1 -> %olt
            PortNo = ((IfIndex bsr 8) band 16#00FF) + 1,
            SlotNo0 = ((IfIndex bsr 16) band 16#00FF),
            ?INFO("before calc ,Type :~p, SlotNo: ~p, PortNo :~p",[Type, SlotNo0, PortNo]),
            SlotNo =  get_slotno_c200(SlotNo0),
            {SlotNo, PortNo, no_onu};
        3 -> %onu
            OnuNo = ((IfIndex bsr 8) band 16#00FF),
            %%slot_no * 8 + port_no
            OltNo = (IfIndex bsr 16) band 16#00FF,
            SlotNo = OltNo div 8,
%            SlotNo = get_slotno_c200(SlotNo0),
            PortNo = OltNo rem 8,
            {SlotNo, PortNo, OnuNo};
         _ ->
             {0,0,0}
    end.

get_slotno_c200(SlotNo) ->
    if
       SlotNo == 0 ->
         4;
       SlotNo == 5 ->
         6;
       SlotNo == 4 ->
         5;
       SlotNo =< 3 ->
        SlotNo;
       true ->
         5
     end.
