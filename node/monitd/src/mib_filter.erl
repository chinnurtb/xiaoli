-module(mib_filter).

-import(proplists, [get_value/2]).

-export([nofilter/1, 
	badval_filter/1,
        radius/1,
        poolname/1,
        ethif/1]).

nofilter(_) -> 
    true.

radius(Radius) ->
    "0.0.0.0" =/= get_value(radiusAuthServerIPAdd, Radius).

poolname(Pool) ->
    "" =/= get_value(ipPoolName, Pool).

ethif(IfEntry) ->
    ifdescr(get_value(ifDescr, IfEntry)).
    
ifdescr(undefined) -> true;
ifdescr("ge" ++ _) -> true;
ifdescr("GE" ++ _) -> true;
ifdescr("eth" ++ _) -> true;
ifdescr("ETH" ++ _) -> true;
ifdescr("LAN" ++ _) -> true;
ifdescr("Gigabit" ++ _) -> true;
ifdescr(" Gigabit" ++ _) -> true;
ifdescr("M-Ethernet" ++ _ ) -> true;
ifdescr("M-GigabitEthernet1" ++ _) -> true;
ifdescr("Ten-GigabitEthernet" ++ _) -> true;
ifdescr("Huawei-WS6603-V100R003-ETHERNET") -> true;
ifdescr("wan port" ++ _) -> true;
ifdescr("wlan" ++ _) -> true;
ifdescr("vlan" ++ _) -> true;
ifdescr("VLAN" ++ _) -> true;
ifdescr("lan port" ++ _) -> true;
ifdescr("Internet port" ++ _) -> true;
ifdescr("INTERNET" ++ _) -> true;
ifdescr("Bridge-" ++ _) -> true;
ifdescr("ve" ++ _) -> true;
ifdescr("ebr" ++ _) -> true;
ifdescr("1" ++ _) -> true;
ifdescr("2" ++ _) -> true;
ifdescr("3" ++ _) -> true;
%bras ifDesc
%ifdescr("InLoopBack0") ->true;
%ifdescr("Aux" ++ _) -> true;
%ifdescr("LoopBack" ++ _) -> true;
%ifdescr("Virtual" ++ _) -> true;
%ifdescr("NULL" ++ _) -> true;
ifdescr(_) -> false.

badval_filter([]) ->
	true;
badval_filter([{_, noSuchObject}|_]) ->
	false;
badval_filter([{_, noSuchInstance}|_]) ->
	false;
badval_filter([_|Record]) ->
	badval_filter(Record).
