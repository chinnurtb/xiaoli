-module(olt_zte_c200).

-include_lib("mit/include/mit.hrl").

-include("snmp/zte.hrl").

-include("snmp/rfc1213.hrl").

-import(monitd_node, [agent/2]).

-export([boards/2,
         ports/2,
         onus/2]).

boards(#mit_node{dn=Dn, ip=Ip}=Node, Args) ->
    VsnTab = mib_record:table(?zxAnCardVersion, fun version_mapper/1),
    VsnTab(binary_to_list(Ip), agent(Node, Args)),

    BoardTab = mib_record:table(?zxAnCard, fun board_mapper/1)
    Boards = BoardTab(binary_to_list(Ip), agent(Node, Args)),
    {ok, [{boards, olt, Dn, Boards}]}.

version_mapper(Row) ->
    [ZxAnRackNo, ZxAnShelfNo, ZxAnSlotNo] = get_value('$tableIndex', Row),
    BoardIdx = boardidx(ZxAnRackNo, ZxAnShelfNo, ZxAnSlotNo),
    put({board, BoardIdx}, [{Col, Val} || {Col, Val} <- Row, Col =/= '$tableInex']).

board_mapper(Row) ->
    [ZxAnRackNo, ZxAnShelfNo, ZxAnSlotNo] = get_value('$tableIndex', Row),
    BoardName = lists:concat(["1-1-", ZxAnSlotNo]),
    BoardIdx = boardidx(ZxAnRackNo, ZxAnShelfNo, ZxAnSlotNo),

    OperStatus = status_mapper:lookup(zte, all, olt_board, oper, get_value(operstatus, Row)),
    AdminStatus = status_mapper:lookup(zte,all,olt_board,admin,get_value(adminstatus, Row)),
    
    Board = [{name, BoardName}, {slot_no, ZxAnSlotNo}, 
             {operstatus, OperStatus}, {adminstatus, AdminStatus}
             | [Col || {N,_} = Col <- Row, board_attr(N)]],

    {BoardIdx, boardvsn(BoardIdx) ++ Board}.

board_attr('$tableIndex') -> false;
board_attr(operstatus) -> false;
board_attr(adminstatus) -> false;
board_attr(_) -> true.

boardidx(Rack, Shelf, Slot) ->
    (ZxAnRackNo bsl 16) + (ZxAnShelfNo bsl 8) + ZxAnSlotNo.

boardvsn(BoardIdx) ->
    case get({board, BoardIdx}) of
    undefined -> [];
    Versions -> Versions
    end.

ports(Node, Args) ->
    PortTab = mib_record:table(?IfEntry, fun port_mapper/1),
    Ports = PortTab(binary_to_list(Ip), agent(Node, Args)),
    {ok, [{ports, olt, Dn, Ports}]}.

%FIXME LATER
port_mapper(Row) ->
    IfIndex = get_value(ifIndex, Row),
    IfDescr = get_value(ifDescr, Row),
    IfType = get_value(ifType, Row),
    IfSpeed = get_value(ifSpeed, Row),
    IfPhysAddr = get_value(ifPhysAddress, Row),
    PortType = port_type(string:sub_string(IfDescr, 1, 4)),
    {SlotNo, PortNo,_OnuNo} = calc_no(IfIndex),
    OperStatus = status_mapper:lookup(zte,all,olt_pon,oper,get_value(ifOperStatus, Row)),
    AdminStatus = status_mapper:lookup(zte,all,olt_pon,admin,get_value(ifAdminStatus, Row)),
    PortName = lists:concat(["1-1-",SlotNo,"-",PortNo]),
    {IfIndex, 
        [{name, list_to_binary(PortName)},
        {alias, list_to_binary(IfDescr)},
        {biztype, PortType},
        {port_no, PortNo},
        {slot_no, SlotNo},
        {ponid, PortName},
        {iftype, IfType},
        {ifadminstatus, AdminStatus},
        {ifoperstatus, OperStatus},
        {ifspeed, IfSpeed},
        {ifphysaddr, IfPhysAddr},
        {ifdescr, list_to_binary(IfDescr)}]}.

port_type("gei" ++ _) -> ?GE;
port_type("epon" ++ _) -> ?PON;
port_type("gpon" ++ _) -> ?PON;
port_type(_) -> 0.

calc_no(IfIndex) ->  %gong you
    Type = ((IfIndex bsr 28) band 16#00FF) ,
    case Type of
        1 -> %olt
            PortNo = ((IfIndex bsr 8) band 16#00FF) + 1,
            SlotNo0 = ((IfIndex bsr 16) band 16#00FF),
            ?INFO("before calc ,Type :~p, SlotNo: ~p, PortNo :~p",[Type, SlotNo0, PortNo]),
            SlotNo =  slotno_c200(SlotNo0),
            {SlotNo, PortNo, no_onu};
        3 -> %onu
            OnuNo = ((IfIndex bsr 8) band 16#00FF),
            %%slot_no * 8 + port_no
            OltNo = (IfIndex bsr 16) band 16#00FF,
            SlotNo = OltNo div 8,
%            SlotNo = slotno_c200(SlotNo0),
            PortNo = OltNo rem 8,
            {SlotNo, PortNo, OnuNo};
         _ ->
             {0,0,0}
    end.

slotno_c200(0) -> 4;
slotno_c200(5) -> 6;
slotno_c200(4) -> 5;
slotno_c200(I) when I =< 3 -> I;
slotno_c200(_) -> 5.

onus(#mit_node{ip=Ip}=Node, Args) ->
    MibOids = ?zxAnOnu ++ ?slaUp ++ ?slaDown ++ ?onuVerTable ++ ?onuIpTable,
    OnuTab = mib_record:table(disco_util:map2oid(MibOids), fun onumapping/1),
    Onus = OnuTab(binary_to_list(Ip), agent(Node, Args)),
    {ok, [{nodes, onu, Dn, Onus}]}.

onumapping(Row) ->
    {value, [OnuIdx]} = dataset:get_value('$tableIndex', Row),
    OidIdx = integer_to_list(OnuIdx),
    {value, UserInfo} = dataset:get_value(userinfo, Row),
    {SlotNo, PortNo, OnuNo} = calc_no(OnuIdx),
    OnuName = lists:concat([SlotNo, "-", PortNo, "-", OnuNo]),
    Row1 = transform(lists:keydelete('$tableIndex', 1, Row)),
    {value, Alias} = dataset:get_value(device_name, Row1),
    {value, AdminState0} = dataset:get_value(adminstate, Row1),
    AdminState = disco_util:lookup("zte",<<"all">>,<<"onu">>,<<"admin">>,AdminState0),
    Row3 = lists:keyreplace(adminstate, 1, Row1, {adminstate, AdminState}),
    Alias1 =
    case Alias of
    "no description" -> UserInfo;
    _ ->
        Alias
    end,
    {value, OnuType} = dataset:get_value(type, Row3),
    Row4 = lists:keydelete(type, 1, Row3),
    Row5 = lists:keydelete(onuModel, 1, Row4),
    PonId = lists:concat(["1-1-",SlotNo,"-",PortNo]),
    {OnuIdx, 
        [{name, binary(OnuName)}, {alias, binary(Alias1)}, 
        {onuidx, OnuIdx}, {oididx, binary(OidIdx)},
        {slot_no, SlotNo}, {port_no, PortNo}, {onu_no, OnuNo},
        {model, binary(OnuType)}, {ponid,PonId} | Row5]}.

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
    transform(T, [{mac, to_mac_str(MacAddr)} | Acc]);
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
        transform(T, [{addr, Ip}|Acc])
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

binary(L) when is_list(L) -> list_to_binary(L);
binary(B) when is_binary(B) -> B.
