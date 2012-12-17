-module(disco_laketune_clt).

-include_lib("elog/include/elog.hrl").

-include("snmp/laketune.hrl").

-include("snmp/rfc1213.hrl").

-import(extbif, [to_list/1]).

-export([disco/4,disco_self/2]).

disco(Dn, Ip, AgentData, _Args) ->
    ?INFO("begin to disco clt: ~p", [Ip]),
	    {ok, Clt} = disco_self(Ip, AgentData),
	    {ok, Mac} = disco_self_mac(Ip, AgentData),
	    ?INFO("result to disco Clt: ~p", [Clt++Mac]),
	    {ok, Port} = disco_port(Dn, Ip, AgentData),
	    ?INFO("result to disco Port: ~p", [Port]),
	    {ok, Cnu} = disco_cnu(Dn, Ip, AgentData),
	    ?INFO("result to disco Cnu: ~p", [Cnu]),
	    {ok, Cnu_port} = disco_cnu_port(Dn, Ip, AgentData),
	    ?INFO("result to disco Cnu_port: ~p", [Cnu_port]),
	{ok, Clt++Mac, Port++Cnu++Cnu_port}.

disco_self(Ip, AgentData) ->
    ?INFO("disco  clt self: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?zxCltTable, AgentData) of
    {ok, [Rows]} ->
	    Row1 = lists:keydelete('$tableIndex', 1, Rows),
		?INFO("disco  clt self: ~p", [Rows]),
        {ok, transform(Row1)};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.

disco_self_mac(Ip, AgentData) ->
    ?INFO("disco  clt self mac: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, [?headendNwEmac], AgentData) of
    {ok, [Rows]} ->
	    Row1 = lists:keydelete('$tableIndex', 1, Rows),
		?INFO("disco  clt self mac: ~p", [Rows]),
        {ok, transform(Row1)};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.

disco_port(Dn, Ip, AgentData) ->
    ?INFO("disco  clt port: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?zxCltPort, AgentData) of
    {ok, Rows} ->
		?INFO("disco clt port : ~p", [Rows]),
        Entries = lists:map(fun(Row) ->
            {value, [Id1,Id2]} = dataset:get_value('$tableIndex', Row),
			PortName = lists:concat([Id1,"-",Id2]),
			[{slot_no,Id1}, {port_no,Id2},{port_name,PortName},{port_category,2},{ponid,PortName},{port_index,Id1*1000+Id2}]
		end,Rows),
		?INFO("disco clt port Entries: ~p", [Entries]),
        {ok, [{entry,ports,Dn,Entries}]};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.

disco_cnu(Dn, Ip, AgentData) ->
    {ok, Cnu0} = disco_cnu0(Ip, AgentData),
    {ok, Cnu1} = disco_cnu1(Ip, AgentData),
    {ok, Cnu2} = disco_cnu2(Ip, AgentData),
    Entries = lists:map(fun({CnuDn,Data0}) ->
                {value, Data1} = dataset:get_value(CnuDn, Cnu1),
	            {value, Data2} = dataset:get_value(CnuDn, Cnu2),
        		Data0++Data1++Data2
    	    end,Cnu0),
    {ok, [{entry,cnus,Dn,Entries}]}.

disco_cnu0(Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, ?zxCnuTable0, AgentData) of
    {ok, Rows} ->
	        Entries = lists:map(fun(Row) ->
	            {value, [Id1,Id2]} = dataset:get_value('$tableIndex', Row),
				DeviceName = lists:concat([Id1,"-",Id2]),
				Rdn = Id1*10000+Id2,
	            Row1 = lists:keydelete('$tableIndex', 1, Row),
				{Rdn,[{rdn,Rdn},{cnu_no,Id2},{device_name,DeviceName}|transform(Row1)]}
			end,Rows),
            {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.
disco_cnu1(Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, ?zxCnuTable1, AgentData) of
    {ok, Rows} ->
	        Entries = lists:map(fun(Row) ->
	            {value, [Id1,Id2]} = dataset:get_value('$tableIndex', Row),
				Rdn = Id1*10000+Id2,
	            Row1 = lists:keydelete('$tableIndex', 1, Row),
				{Rdn,transform(Row1)}
			end,Rows),
            {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.
disco_cnu2(Ip, AgentData) ->
    case snmp_mapping:get_table(Ip, ?zxCnuTable2, AgentData) of
    {ok, Rows} ->
	        Entries = lists:map(fun(Row) ->
	            {value, [Id1,Id2]} = dataset:get_value('$tableIndex', Row),
				Rdn = Id1*10000+Id2,
	            Row1 = lists:keydelete('$tableIndex', 1, Row),
				{Rdn,transform(Row1)}
			end,Rows),
            {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.


disco_cnu_port(Dn, Ip, AgentData) ->
    ?INFO("disco  cnu port: ~p", [Ip]),
    case snmp_mapping:get_table(Ip, ?zxCnuPortD, AgentData) of
    {ok, Rows} ->
		?INFO("disco cnu port: ~p", [Rows]),
        {_,CnuDicts} = lists:mapfoldl(fun(Row,Dict) ->
    		        {value, [Id1,Id2,Id3]} = dataset:get_value('$tableIndex', Row),
            		PortIndex = Id2*1000+Id3,
            		CnuRdn = Id1*10000+Id2,
                	{value, Upassuredbw} = dataset:get_value(cnuPortCfgUlBwLmt, Row),
            		{value, Downassuredbw} = dataset:get_value(cnuPortCfgDlBwLmt, Row),
                    CnuDn = lists:concat(["cnu=",to_list(CnuRdn),"," ,to_list(Dn)]),
                    PortName = lists:concat([Id1,"-",Id2,"-",Id3]),
                    {[],dict:append(CnuDn,[{slot_no,Id2}, {port_no,Id3},{port_category,3},{port_name,PortName},{ponid,PortName},{port_index,PortIndex},
                                            {upassuredbw,Upassuredbw},{downassuredbw,Downassuredbw}],Dict)}
                end,dict:new(), Rows),
		Entries = [{entry,ports,CnuDn,Ports} || {CnuDn,Ports} <- dict:to_list(CnuDicts)],
		?INFO("~p", [Entries]),
        {ok, Entries};
    {error, Reason} ->
        ?WARNING("~p", [Reason]),
        {ok, []}
    end.



transform(Data) ->
    transform(Data, []).
transform([{ip, Value}|T], Acc) ->
    transform(T, [{ip, to_ip_str(Value)}|Acc]);
transform([{mask, Value}|T], Acc) ->
    transform(T, [{mask, to_ip_str(Value)}|Acc]);
transform([{mac, Value}|T], Acc) ->
    transform(T, [{mac, to_mac_str(Value)}|Acc]);
transform([{mac1, Value}|T], Acc) ->
    transform(T, [{mac1, to_mac_str(Value)}|Acc]);
transform([{cnu_status, Value}|T], Acc) ->
    transform(T, [{cnu_status, disco_util:lookup("laketune",<<"all">>,<<"cnu">>,<<"oper">>,Value)}|Acc]);
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

