-module(disco_zte_olt_c200).

-include_lib("elog/include/elog.hrl").

-define(PON, 3).
-define(GE, 2).
-define(FE, 1).

-define(ETH, "ethernet-csmacd").
-define(EPON, "epon").
-import(extbif, [to_list/1]).

-export([disco/4, disco_boards/3, disco_ports/3, disco_onus/3, calc_no/1,disco_ont_ports/3]).

disco(Dn, Ip, Agent, _Args) ->
    ?INFO("begin to disco olt: ~p", [Ip]),
    ?DEBUG("Onus: ~p", [Onus]),
    {ok, [], Boards ++ Ports ++ Onus}.

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

