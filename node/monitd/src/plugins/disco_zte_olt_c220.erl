-module(disco_zte_olt_c220).

-include("snmp/zte.hrl").

-include("snmp/rfc1213.hrl").

-include_lib("elog/include/elog.hrl").

-define(GE, 2).
-define(FE, 1).
-define(PON, 3).

-define(ETH, "ethernet-csmacd").
-define(EPON, "epon").
-define(GPON, "gpon").

-import(extbif, [to_list/1]).

-export([disco/4, disco_onus/3, calc_no/1]).

disco(Dn, Ip, AgentData, _Args) ->
    ?INFO("begin to disco olt: ~p", [Ip]),
    {ok, Boards} = disco_boards(Dn, Ip, AgentData),
    ?DEBUG("Boards: ~p", [Boards]),
    {ok, Ports} = disco_ports(Dn, Ip, AgentData),
    ?DEBUG("Ports: ~p", [Ports]),
    {ok, Onus} = disco_onus(Dn, Ip, AgentData),
    {ok, Onus0} = disco_zte_olt_c200:disco_onus(Dn, Ip, AgentData),
    ?DEBUG("Onus: ~p", [Onus]),
    {ok, [], Boards ++ Ports ++ Onus++ Onus0}.

disco_boards(Dn, Ip, AgentData) ->
    ?INFO("disco olt boards: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?zxAnCard, AgentData) of
    {ok, Rows} ->
        Boards = lists:map(fun(Row) ->
            {value, [ZxAnRackNo, ZxAnShelfNo, ZxAnSlotNo]} = dataset:get_value('$tableIndex', Row),
            Row1 = lists:keydelete('$tableIndex', 1, Row),
			BoardName = lists:concat(["1-1-",ZxAnSlotNo]),
            BoardIdx = (ZxAnRackNo bsl 16) + (ZxAnShelfNo bsl 8) + ZxAnSlotNo,
            {value, OperStatus0} = dataset:get_value(operstatus, Row1),
            {value, AdminStatus0} = dataset:get_value(adminstatus, Row1),
            %与上面的walk值不一致，少状态不在线数据
           Versions =  case snmp_mapping:get_entry(Ip, ?zxAnCardVersion, [ZxAnRackNo, ZxAnShelfNo, ZxAnSlotNo], AgentData) of
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
            {BoardIdx, [{name, BoardName}, {slot_no, ZxAnSlotNo} | Row4]}
        end,[], Rows),
        {ok, {boards, olt, Dn, Boards}};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.

disco_ports(Dn, Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, ?IfEntry, AgentData) of
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
            {SlotNo, PortNo,_OnuNo} = calc_no(IfIndex),
			{value, OperStatus0} = dataset:get_value(ifOperStatus, Row),
	        {value, AdminStatus0} = dataset:get_value(ifAdminStatus, Row),
	        OperStatus = disco_util:lookup("zte",<<"all">>,<<"olt_pon">>,<<"oper">>,OperStatus0),
	        AdminStatus = disco_util:lookup("zte",<<"all">>,<<"olt_pon">>,<<"admin">>,AdminStatus0),
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
                {ifdescr, list_to_binary(IfDescr)}]}
       end, Rows),
        ?INFO("zte port : ~p", [Ports]),
        {ok, [{ports, olt, Dn, Ports}]};
    {error, Reason} ->
       ?WARNING("~p", [Reason]),
        {ok,[]}
    end.

disco_onus(Dn, Ip, AgentData) ->
    ?INFO("disco olt onus: ~p", [Ip]),
    case sesnmp:get_table(Ip, ?onuVerTable ++ ?slaUp ++ ?slaDown  , AgentData) of
    {ok, Rows} ->
        ?INFO(" :~p",[Rows]),
        {_,Dicts} = lists:mapfoldl(fun(Row,Dict) ->
            {value, [OnuIdx]} = dataset:get_value('$tableIndex', Row),
            OidIdx = integer_to_list(OnuIdx),
            {SlotNo, PortNo, OnuNo} = calc_no_zte(OnuIdx),
            PonId = lists:concat(["1-1-",SlotNo,"-",PortNo]),
            Key = lists:concat(["1-1-",SlotNo,"-",PortNo,"-",OnuNo]),
            OnuAttr = case sesnmp:get_entry(Ip, disco_util:map2oid(?zxAnOnu), [Idx], AgentData) of
                        {ok, Data} ->
                            NewRow = Data ++ Row,
                            [{slot_no, SlotNo}, {port_no, PortNo},{onu_no, OnuNo},
                             {onuidx, OnuIdx}, {ponid, PonId}, {oididx, OidIdx} | transform(NewRow)];
                        {error, Reason2} ->
                            ?WARNING("~p", [Reason2]),
                            []
                      end,
             {[],dict:append_list(Key,OnuAttr,Dict)}
        end, dict:new(),Rows),
        ?INFO(" Dicts:~p",[dict:to_list(Dicts)]),
        {_,EntryDicts} = case snmp_mapping:get_table(Ip, ?gonuIpTable , AgentData) of
                            {ok, Datas} ->
                                ?INFO(" :~p",[Datas]),
                                lists:mapfoldl(fun(Data,Dict) ->
                                    {value, [PortIndex,OnuNo]} = dataset:get_value('$tableIndex', Data),
                                    {SlotNo, PortNo, _} = calc_no_zte(PortIndex),
                                    Key = lists:concat(["1-1-",SlotNo,"-",PortNo,"-",OnuNo]),
                                    {value, Ip0} = dataset:get_value(ip, Data,[]),
                                    {value, Mask} = dataset:get_value(mask, Data,[]),
                                    {[],dict:append_list(Key,[{addr,to_ip_str(Ip0)},{mask,to_ip_str(Mask)}],Dict)}
                                end, Dicts,Datas);
                                {error, Reason2} ->
                                    ?WARNING("~p", [Reason2]),
                                    {[],Dicts}
                                end,
        Onus =  [ Onu || {_,Onu}<- dict:to_list(EntryDicts),filter(Onu)],
        ?INFO("zte onu : ~p", [Onus]),
	   {ok, [{nodes, onu, Dn, Onus}]};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.

filter(Onu) ->
    case proplists:get_value(onuidx, Onu,false) of
        false -> false;
        _ -> true
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
    AdminState = disco_util:lookup("zte",<<"all">>,<<"onu">>,<<"admin">>,Status),
    transform(T, [{adminstate,AdminState} | Acc]);
transform([{authmacsn, AuthMacSN}|T], Acc) ->
        transform(T, [{vendor, <<"zte">>},{authmacsn, AuthMacSN} | Acc]);
transform([{macaddr, MacAddr}|T], Acc) ->
    transform(T, [{mac, to_mac_str(MacAddr)} | Acc]);
transform([{authmacaddr, AuthMac}|T], Acc) ->
    transform(T, [{authmacaddr, to_mac_str(AuthMac)}|Acc]);
transform([{registermacaddress, RegMac}|T], Acc) ->
    transform(T, [{registermacaddress, to_mac_str(RegMac)}|Acc]);
transform([{type, Value}|T], Acc) ->
    transform(T, [{type, Value}|Acc]);
transform([{loid, Value}|T], Acc) ->
    transform(T, [{loid, Value}|Acc]);
transform([{userinfo, Value}|T], Acc) ->
    transform(T, [{userinfo, Value}|Acc]);
transform([{downassuredbw, Value}|T], Acc) ->
    transform(T, [{downassuredbw, Value}|Acc]);
transform([{downmaximumbw, Value}|T], Acc) ->
    transform(T, [{downmaximumbw, Value}|Acc]);
transform([{downmaxburstsize, Value}|T], Acc) ->
    transform(T, [{downmaxburstsize, Value}|Acc]);
transform([{upassuredbw, Value}|T], Acc) ->
    transform(T, [{upassuredbw, Value}|Acc]);
transform([{upmaximumbw, Value}|T], Acc) ->
    transform(T, [{upmaximumbw, Value}|Acc]);
transform([{upmaxburstsize, Value}|T], Acc) ->
    transform(T, [{upmaxburstsize, Value}|Acc]);
transform([_|T], Acc) ->
    transform(T, Acc);
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

calc_no(IfIndex) ->  %gong you
    Type = ((IfIndex bsr 28) band 16#00FF) ,
    case Type of
        1 -> %olt
            PortNo = ((IfIndex bsr 8) band 16#00FF) + 1,
            SlotNo0 = ((IfIndex bsr 16) band 16#00FF),
            SlotNo =  get_slotno_c220(SlotNo0),
            ?INFO("before calc index ~p,Type :~p, SlotNo: ~p, PortNo :~p",[IfIndex,Type, SlotNo, PortNo]),
            {SlotNo, PortNo, no_onu};
        3 -> %onu
            OnuNo = ((IfIndex bsr 8) band 16#00FF),
            PortNo = ((IfIndex bsr 16) band 7),
            SlotNo = ((IfIndex bsr 19) band 31),
            {SlotNo, PortNo, OnuNo};
         _ ->
             {0,0,0}
    end.

calc_no_zte(IfIndex) ->  %si you
    Type = ((IfIndex bsr 28) band 16#00FF) ,
    case Type of
        1 -> %olt
            PortNo = ((IfIndex bsr 8) band 16#00FF),
            SlotNo = ((IfIndex bsr 16) band 16#00FF),
            ?INFO("before calc index ~p,Type :~p, SlotNo: ~p, PortNo :~p",[IfIndex,Type, SlotNo, PortNo]),
            {SlotNo, PortNo, no_onu};
        3 -> %onu
               OnuNo = ((IfIndex bsr 8) band 16#00FF),
               PortNo = ((IfIndex bsr 16) band 7) + 1,
               SlotNo = ((IfIndex bsr 19) band 31),
               {SlotNo, PortNo, OnuNo};
         _ ->
             {0,0,0}
    end.

get_slotno_c220(SlotNo) ->
    if
       SlotNo =< 5 ->
         SlotNo + 1;
       (SlotNo >= 6) and (SlotNo =< 11) ->
         SlotNo + 3;
       true ->
         0
     end.

