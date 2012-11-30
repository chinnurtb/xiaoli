%%%----------------------------------------------------------------------
%%% File    : coll_intf.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : check availability of device by ping and snmp
%%% Created : 02 Sep. 2008
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(coll_intf).

-include("mit.hrl").

-include("mib.hrl").

-include("event.hrl").

-include("metric.hrl").

-include_lib("elog/include/elog.hrl").

-export([run/2]).

-import(proplists, [get_value/2, get_value/3]).

run(#node{ip=undefined}, _Args) ->
    ignore;

run(#node{attrs=Attrs}=Node, Args) ->
	Ts = extbif:timestamp(),
	Ip = binary_to_list(Node#node.ip), 
	Dn = Node#node.dn,
    Mib = proplists:get_value(mib, Args),
    Community = get_value(snmp_comm, Attrs, <<"public">>),
    
    IfTab = mib_record:table(?MIB(Mib, ifTraffic), fun intftraffic/1),

    Records = IfTab(Ip, [{community, Community}]),

    Metrics = [#metric{name = 'intf.traffic',
                       from = Ip,
                       dn = ifdn(Dn, IfIndex),
                       timestamp = Ts,
                       data = Data}
                || {IfIndex, Data} <- Records],

    {ok, Metrics, Args}.

ifdn(Dn, IfIndex) ->
    list_to_binary([Dn, ",ifindex=", integer_to_list(IfIndex)]).

intftraffic(Record) ->
	IfIndex = 
	case dataset:get_value('$tableIndex', Record) of
    {value, [IfIdx]} -> IfIdx;
	{value, [BoardIdx, IfIdx]} -> BoardIdx * 256 + IfIdx;
	{false, _} -> 0
	end,
    Traffic = intftraffic([{N, mib_formatter:i(V)} || {N, V} <- Record], 0, 0, []),
    {IfIndex, Traffic}.

intftraffic([], IfInPkts, IfOutPkts, Acc) ->
    [{ifInPkts, IfInPkts}, {ifOutPkts, IfOutPkts}|Acc];
intftraffic([{'$tableIndex', _}|Record], IfInPkts, IfOutPkts, Acc) ->
    intftraffic(Record, IfInPkts, IfOutPkts, Acc);
intftraffic([{ifDescr, _}|Record], IfInPkts, IfOutPkts, Acc) ->
    intftraffic(Record, IfInPkts, IfOutPkts, Acc);
intftraffic([{ifInOctets, V}|Record], IfInPkts, IfOutPkts, Acc) ->
    intftraffic(Record, IfInPkts, IfOutPkts, [{ifInOctets, V*8}|Acc]);
intftraffic([{ifOutOctets, V}|Record], IfInPkts, IfOutPkts, Acc) ->
    intftraffic(Record, IfInPkts, IfOutPkts, [{ifOutOctets, V*8}|Acc]);
intftraffic([{ifInUcastPkts, V}|Record], IfInPkts, IfOutPkts, Acc) ->
    intftraffic(Record, V+IfInPkts, IfOutPkts, [{ifInUcastPkts, V}|Acc]);
intftraffic([{ifInNUcastPkts, V}|Record], IfInPkts, IfOutPkts, Acc) ->
    intftraffic(Record, V+IfInPkts, IfOutPkts, [{ifInNUcastPkts, V}|Acc]);
intftraffic([{ifOutUcastPkts, V}|Record], IfInPkts, IfOutPkts, Acc) ->
    intftraffic(Record, IfInPkts, V+IfOutPkts, [{ifOutUcastPkts, V}|Acc]);
intftraffic([{ifOutNUcastPkts, V}|Record], IfInPkts, IfOutPkts, Acc) ->
    intftraffic(Record, IfInPkts, V+IfOutPkts, [{ifOutNUcastPkts, V}|Acc]);
intftraffic([{Name, Value}|Record], IfInPkts, IfOutPkts, Acc) ->
    intftraffic(Record, IfInPkts, IfOutPkts, [{Name, Value}|Acc]).
