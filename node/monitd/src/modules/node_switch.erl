-module(node_switch).

-include("mib.hrl").

-import(proplists, [get_value/2]).

-export([ports/2]).

ports(#mit_node{dn=Dn, ip=Ip, category=Cat}=Node, Args) ->
    Agent = monitd_node:agent(Node, Args),
    PortTab = mib_record:table(
        ?MIB(get_value(mib, Args), ifEntry), 
        fun ifentry_mapper:map/1),
    Ports = PortTab(Ip, [{limit, 640} | Agent]),
    {ok, [{ports, Cat, Dn, [map(Port) || Port <- Ports]}]}.

map({IfIndex, Port}) ->
    IfDescr = proplists:get_value(ifdescr, Port), 
    {IfIndex, [{biztype, 1}, {name, IfDescr}, {alias, IfDescr}|Port]}

