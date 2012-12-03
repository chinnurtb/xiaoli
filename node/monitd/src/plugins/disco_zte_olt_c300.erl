-module(disco_zte_olt_c300).

-include_lib("elog/include/elog.hrl").

-include("snmp/zte.hrl").

-include("snmp/rfc1213.hrl").

-define(PON, 202).
-define(GE, 201).
-define(FE, 200).
-define(ETH, "ethernet-csmacd").
-define(EPON, "eponOltPort").
-define(GPON, "gponOltPort").
-import(extbif, [to_list/1]).

-export([disco/4, disco_onus/3, calc_no/1]).

disco(Dn, Ip, AgentData, _Args) ->
    ?INFO("begin to disco olt: ~p", [Ip]),
    {ok, Boards} = disco_boards(Dn, Ip, AgentData),
    ?INFO("Boards: ~p", [Boards]),
    {ok, Ports,Dicts} = disco_ports(Dn, Ip, AgentData),
    ?INFO("Ports: ~p,key:~p", [Ports,dict:fetch_keys(Dicts)]),
	Keys = lists:filter(fun(Elem) ->Elem=="eponOltPort" orelse Elem =="gponOltPort" end,dict:fetch_keys(Dicts)),
	case lists:flatten(Keys) of
		"eponOltPort" ->
			{ok,Onus} = disco_onus(Dn, Ip, AgentData),
			{ok, [{olt_pon_type,"epon"}], Boards ++ Ports ++ Onus};
		"gponOltPort" ->
			{ok,GOnus} = disco_gonus(Dn, Ip, AgentData),
			{ok, [{olt_pon_type,"gpon"}], Boards ++ Ports ++ GOnus};
		"eponOltPortgponOltPort" ->
			{ok,Onus} = disco_onus(Dn, Ip, AgentData),
			{ok,GOnus} = disco_gonus(Dn, Ip, AgentData),
			{ok, [{olt_pon_type,"epon_gpon"}], Boards ++ Ports ++ Onus++GOnus};
		"gponOltPorteponOltPort" ->
			{ok,Onus} = disco_onus(Dn, Ip, AgentData),
			{ok,GOnus} = disco_gonus(Dn, Ip, AgentData),
			{ok, [{olt_pon_type,"epon_gpon"}], Boards ++ Ports ++ Onus++GOnus};
		 _ ->
			?WARNING("can not find epon or gpon ~p", [Ports]),
			{ok, [], Boards ++ Ports}
	end.

disco_boards(Dn, Ip, AgentData) ->
    ?INFO("disco olt boards: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?zxAnCard, AgentData) of
    {ok, Rows} ->
        Entries  = lists:foldl(fun(Row, Data) ->
            {value, [ZxAnRackNo, ZxAnShelfNo, ZxAnSlotNo]} = dataset:get_value('$tableIndex', Row),
            Row1 = lists:keydelete('$tableIndex', 1, Row),
            Boardid = lists:concat(["1-1-",ZxAnSlotNo]),
            BoardDn = "slot=" ++Boardid ++ "," ++ to_list(Dn),
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
            [{entry, board, list_to_binary(BoardDn), [{slot_no, ZxAnSlotNo},{boardid,Boardid} | Row4]} | Data]
        end,[], Rows),
        {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.

disco_ports(Dn, Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, ?IfEntry, AgentData) of
    {ok, Rows} ->
        {Entries,Dicts} = lists:mapfoldl(fun(Row,Dict) ->
            {value, IfIndex} = dataset:get_value(ifIndex, Row),
            {value, IfDescr} = dataset:get_value(ifDescr, Row),
            {value, IfType} = dataset:get_value(ifType, Row),
            {value, IfSpeed} = dataset:get_value(ifSpeed, Row),
            PortType = case string:sub_string(IfDescr, 1, 4) of
                "xgei" ->
                    ?GE;
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
                301 -> ?GPON;
     			_ ->
                    IfType
			end,
            {SlotNo, PortNo,_OnuNo} = calc_no(IfIndex),
             %?INFO("after_port,SlotNo: ~p, PortNo :~p",[SlotNo, PortNo]),
			 {value, OperStatus0} = dataset:get_value(ifOperStatus, Row),
	         {value, AdminStatus0} = dataset:get_value(ifAdminStatus, Row),
	        OperStatus = disco_util:lookup("zte",<<"all">>,<<"olt_pon">>,<<"oper">>,OperStatus0),
	        AdminStatus = disco_util:lookup("zte",<<"all">>,<<"olt_pon">>,<<"admin">>,AdminStatus0),
            PortName = lists:concat(["1-1-",SlotNo,"-",PortNo]),
            PortDn = "port=" ++ integer_to_list(IfIndex) ++ "," ++ binary_to_list(Dn),
			{{entry, port, list_to_binary(PortDn), [{port_no, PortNo},{port_index,IfIndex}, {port_category, PortType},
			{port_type, IfTypeStr}, {slot_no, SlotNo},{port_name,PortName},{ponid,PortName},
 			{admin_status,AdminStatus},{oper_status,OperStatus},{speed,IfSpeed},{port_desc,IfDescr}]},dict:append_list(IfTypeStr,PortDn,Dict)}
        end, dict:new(),Rows),
        ?INFO("zte port : ~p", [Entries]),
		{ok,Entries,Dicts};
    {error, Reason} ->
       ?WARNING("~p", [Reason]),
        {ok,[]}
    end.

disco_onus(Dn, Ip, AgentData) ->
    ?INFO("disco olt onus: ~p", [Ip]),
    case sesnmp:get_table(Ip, ?onuVerTable ++ ?slaUp ++ ?slaDown, AgentData) of
    {ok, Rows} ->
        ?INFO(" :~p",[Rows]),
        {_,Dicts} = lists:mapfoldl(fun(Row,Dict) ->
            {value, [Idx]} = dataset:get_value('$tableIndex', Row),
            {SlotNo, PortNo, OnuNo} = calc_no_zte(Idx),
            Rdn = integer_to_list(Idx),
            PonId = lists:concat(["1-1-",SlotNo,"-",PortNo]),
            Key = lists:concat(["1-1-",SlotNo,"-",PortNo,"-",OnuNo]),
            OnuAttr = case sesnmp:get_entry(Ip, disco_util:map2oid(?zxAnOnu), [Idx], AgentData) of
                        {ok, Data} ->
                            NewRow = Data ++ Row,
                            [{rdn, Rdn},{discovery_state, 1},{slot_no, SlotNo}, {port_no, PortNo},{onu_no, OnuNo},
                                        {ponid,PonId} | transform(NewRow)];
                        {error, Reason2} ->
                            ?WARNING("~p", [Reason2]),
                            []
                      end,
             {[],dict:append_list(Key,OnuAttr,Dict)}
        end, dict:new(),Rows),
        {_,EntryDicts} = case snmp_mapping:get_table(Ip, ?gonuIpTable , AgentData) of
                            {ok, Datas} ->
                                ?INFO(" :~p",[Datas]),
                                lists:mapfoldl(fun(Data,Dict) ->
                                    {value, [PortIndex,OnuNo]} = dataset:get_value('$tableIndex', Data),
                                    {SlotNo, PortNo, _} = calc_no_zte(PortIndex),
                                    Key = lists:concat(["1-1-",SlotNo,"-",PortNo,"-",OnuNo]),
                                    {value, Ip0} = dataset:get_value(ip, Data,[]),
                                    {value, Mask} = dataset:get_value(mask, Data,[]),
                                    {[],dict:append_list(Key,[{ip,to_ip_str(Ip0)},{mask,to_ip_str(Mask)}],Dict)}
                                end, Dicts,Datas);
                                {error, Reason2} ->
                                    ?WARNING("~p", [Reason2]),
                                    {[],Dicts}
                                end,
        Entries =  [ Onu || {_,Onu}<- dict:to_list(EntryDicts),filter(Onu)],
        ?INFO("zte onu : ~p", [Entries]),
	   {ok, [{entry, onus, Dn, Entries}]};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.


filter(Onu) ->
    case proplists:get_value(rdn, Onu,false) of
        false -> false;
        _ -> true
    end.


disco_gonus(Dn, Ip, AgentData) ->
    ?INFO("disco olt gonus: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?zxAnGOnu , AgentData) of
    {ok, Rows} ->
        ?INFO(" :~p",[Rows]),
        Entries = lists:map(fun(Row) ->
                        {value, [PonIndex,OnuNo]} = dataset:get_value('$tableIndex', Row),
                        {SlotNo, PortNo, _} = calc_no_zte(PonIndex),
                        PonId = lists:concat(["1-1-",SlotNo,"-",PortNo]),
                        Rdn = rdn(3,SlotNo,PortNo,OnuNo),
                        [{rdn, Rdn},{discovery_state, 1},{slot_no, SlotNo}, {port_no, PortNo},
                        {onu_no, OnuNo},{ponid,PonId} | transform(Row)]
                     end, Rows),
        ?INFO("zte gonu : ~p", [Entries]),
	   {ok, [{entry, onus, Dn, Entries}]};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
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
    transform(T, [{macaddr, to_mac_str(MacAddr)} | Acc]);
transform([{authmacaddr, AuthMac}|T], Acc) ->
    transform(T, [{authmacaddr, to_mac_str(AuthMac)}|Acc]);
transform([{registermacaddress, RegMac}|T], Acc) ->
    transform(T, [{registermacaddress, to_mac_str(RegMac)}|Acc]);
transform([{type, Value}|T], Acc) ->
    transform(T, [{type, Value}|Acc]);
transform([{loid, Value}|T], Acc) ->
    transform(T, [{loid, Value}|Acc]);
transform([{ip, Value}|T], Acc) ->
    transform(T, [{ip, to_ip_str(Value)}|Acc]);
transform([{mask, Value}|T], Acc) ->
    transform(T, [{mask, to_ip_str(Value)}|Acc]);
transform([{authpassword, Value}|T], Acc) ->
    transform(T, [{authpassword, Value}|Acc]);
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

rdn(Type,SlotNo,PortNo,OnuNo) ->
    (Type bsl 28) + (SlotNo bsl 19) + ((PortNo -1) bsl 16) + (OnuNo bsl 8).

calc_no(IfIndex) ->  %gong you
    Type = ((IfIndex bsr 28) band 16#00FF) ,
    case Type of
        1 -> %olt
            PortNo = ((IfIndex bsr 8) band 16#00FF) + 1,
            SlotNo0 = ((IfIndex bsr 16) band 16#00FF),
            SlotNo =  get_slotno_c300(SlotNo0),
            ?INFO("before calc ,Type :~p, SlotNo: ~p, PortNo :~p",[Type, SlotNo, PortNo]),
            {SlotNo, PortNo, no_onu};
        3 -> %onu
            OnuNo = ((IfIndex bsr 8) band 16#00FF),
            PortNo = ((IfIndex bsr 16) band 7) + 1,
            SlotNo = ((IfIndex bsr 19) band 31),
            %SlotNo =  get_slotno_c300(SlotNo0),
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
            ?INFO("before calc ,Type :~p, SlotNo: ~p, PortNo :~p",[Type, SlotNo, PortNo]),
            {SlotNo, PortNo, no_onu};
        3 -> %onu
            OnuNo = ((IfIndex bsr 8) band 16#00FF),
            PortNo = ((IfIndex bsr 16) band 7) + 1,
            SlotNo = ((IfIndex bsr 19) band 31),
            {SlotNo, PortNo, OnuNo};
         _ ->
             {0,0,0}
    end.

get_slotno_c300(SlotNo) ->
    if
       SlotNo =< 7 ->
         SlotNo + 2;
       (SlotNo >= 8) and (SlotNo =< 18) ->
         SlotNo + 4;
       true ->
         0
     end.
