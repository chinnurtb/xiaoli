-module(ifentry_mapper).

-import(mib_formatter, [mac/1]).

-export([map/1]).

map(Record) ->
    map(Record, []).

map([], Port) ->
    IfIndex = get_value(ifindex, Port, 1),
    {IfIndex, lists:keydelete(ifindex, 1, Port)};
    
map([{'$tableIndex', TabIdx} | Record], Port) ->
    IfIndex1 =
    case TabIdx of
    [BroadIdx, IfIndex] -> (BroadIdx bsl 8) + IfIndex;
    [IfIndex] -> IfIndex
    end,
    map(Record, [{ifindex, IfIndex1}|Port]);

map([{ifindex, _} | Record], Port) ->
    map(Record, Port);

map([{ifphysaddr, PhysAddr} | Record], Port) ->
    map(Record, [{ifphysaddr, mac(PhysAddr)} | Port]);

map([Attr | Record], Port) ->
    map(Record, [Attr|Port]).


