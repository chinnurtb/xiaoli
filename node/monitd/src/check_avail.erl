%%%----------------------------------------------------------------------
%%% File    : check_avail.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : check availability of device by ping and snmp
%%% Created : 02 Sep. 2008
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(check_avail).

-include("mit.hrl").

-include("event.hrl").

-include("metric.hrl").

-include_lib("elog/include/elog.hrl").

-export([run/2]).

-import(proplists, [get_value/2, get_value/3]).

run(#node{ip=undefined}, _Args) ->
    ignore;

run(#node{attrs=Attrs}=Node, Args) ->
	Ts = extbif:timestamp(),
	Ip = Node#node.ip, 
	Dn = Node#node.dn,
    Community = get_value(snmp_comm, Attrs, <<"public">>),
    {PingStatus, PingSummary} = check_ping(Ip),
    {SnmpStatus, SnmpSummary} = check_snmp(Ip, Community),
	AvailStatus = avail_status(PingStatus, SnmpStatus),
	PingEvent = #event{name = ping_status,
					   sender = Dn,
					   evtkey = "ping_status",
					   severity = ping_severity(PingStatus),
					   summary = PingSummary,
					   timestamp = Ts,
					   manager = node(),
					   from = monitor},
	SnmpEvent = #event{name = snmp_status,
					   sender = Dn,
					   evtkey = "snmp_status",
					   severity = snmp_severity(SnmpStatus),
					   summary = SnmpSummary,
					   timestamp = Ts,
					   manager = node(),
					   from = monitor},
	AvailEvent = #event{name = avail_status,
						sender = Dn,
						evtkey = "snmp_status",
						severity = avail_severity(AvailStatus),
						timestamp = Ts,
						manager = node(),
						from = monitor,
						vars = [{status, AvailStatus},
								{ping_status, ping_status(PingStatus)},
								{ping_summary, PingSummary},
								{snmp_status, snmp_status(SnmpStatus)},
								{snmp_summary, SnmpSummary}]},
    Metric = #metric{name = 'ping', 
					 from = Ip, 
					 dn = Dn, 
					 timestamp = Ts, 
					 data = [parse(loss, PingSummary)|parse(rtt, PingSummary)]},
    {ok, [PingEvent, SnmpEvent, AvailEvent, Metric], Args}.

avail_status("PING OK", _) -> 1;
avail_status("PING WARNING", _) -> 1;
avail_status(_, "SNMP OK") -> 1;
avail_status(_, _) -> 0.

avail_severity(1) -> major;
avail_severity(0) -> clear. 

ping_status("PING OK") -> <<"OK">>;
ping_status("PING WARNING") -> <<"OK">>;
ping_status("PING CRITICAL") -> <<"CRITICAL">>;
ping_status(_) -> <<"CRITICAL">>.

ping_severity("PING OK") -> clear;
ping_severity("PING WARNING") -> clear;
ping_severity("PING CRITICAL") -> major;
ping_severity(_) -> major.

snmp_status("SNMP OK") -> <<"OK">>;
snmp_status("SNMP problem") -> <<"CRITICAL">>;
snmp_status(_) -> <<"CRITICAL">>.

snmp_severity("SNMP OK") -> clear;
snmp_severity("SNMP problem") -> major;
snmp_severity(_) -> major.

check_ping(Ip) ->
	{Status, Summary} = check_ping:run(Ip),
	{ping_format(Status), Summary}.

ping_format("CRITICAL") -> "PING CRITICAL";
ping_format("WARNING") -> "PING WARNING";
ping_format(Other) -> Other.

check_snmp(Ip, Community) ->
	check_snmp:run(Ip, Community).

parse(rtt, Summary) ->
	Regexp = "((\\d+(\\.\\d+)*\/){2}\\d+(\\.\\d+)*)",
	case re:run(Summary,Regexp,[{capture,[1],list}]) of
	{match, [Str]} ->
		[Min,Rta,Max|_] = string:tokens(Str,"/"),
		[{rtmin, num(Min)},{rta, num(Rta)},{rtmax, num(Max)}];
	nomatch -> 
		?INFO("nomatch: ~p", [Summary]),
		[{rtmin,0.0},{rta,0.0},{rtmax,0.0}]
	end;

parse(loss, Summary) ->
	case re:run(Summary, "\\s(\\d+)% packet loss", [{capture, [1], list}]) of
	{match, [LostRate]} ->
		{loss,list_to_integer(LostRate)};
	nomatch -> 
		?WARNING("no loss: ~p", [Summary]),
		{loss,100}
   end.

num(S) ->
    case string:chr(S, $.) of
    0 -> list_to_integer(S);
    _ -> list_to_float(S)
    end.

