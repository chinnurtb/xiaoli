-module(disco_switch).

-include("mib.hrl").

-export([disco/4]).

disco(Dn, Ip, Agent, Args) ->
    {value, Mib} = dataset:get_value(mib, Args),
    EthTab = mib_record:table(
        ?MIB(Mib, ifEntry), 
        fun mib_mapper:intf_mapping/1),
    EthIfs = EthTab(Ip, [{limit, 640} | Agent]),
    MapFun = fun({IfIndex, Eth}) ->
        IfDescr = proplists:get_value(ifdescr, Eth), 
        {IfIndex, [{biztype, 1},
                  {name, IfDescr},
                  {alias, IfDescr}|Eth]}
    end,
    EthIfs1 = [MapFun(EthIf) || EthIf <- EthIfs],
    {ok, [], [{ports, switch, Dn, EthIfs1}]}.

