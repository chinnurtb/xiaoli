-module(check_snmp).

-include_lib("elog/include/elog.hrl").

-include("mib/rfc1213.hrl").

-import(extbif, [to_list/1]).

-export([run/2]).

run(Host, Community) ->
    AgentData = [{community, to_list(Community)}],
    case sesnmp:get_group(to_list(Host), 161, [?SysDescr, ?SysUpTime], AgentData, 30000) of
    {ok, Values} ->
        {value, SysDescr} = dataset:get_value(sysDescr, Values),
        {value, SysUptime} = dataset:get_value(sysUpTime, Values),
        Summary = lists:concat(["SysDescr = ", SysDescr, ", Uptime = ", SysUptime]),
        {"SNMP OK", Summary};
    {error, {timeout, _}} ->
        {"SNMP problem", "SNMP Timeout."};
    {error, Reason} ->
        ?ERROR("~p", [Reason]),
        {"SNMP problem", "SNMP Error."}
    end.
