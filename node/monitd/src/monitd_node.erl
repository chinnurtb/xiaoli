-module(monitd_node).

-include_lib("mit/include/mit.hrl").

-export([agent/1, agent/2]).

agent(Node) -> 
    agent(Node, []).

%TODO: FIXME Later
agent(#mit_node{community=Community,
    write_commuity=WriteCommunity} = Node, _Args) ->
    [{community, Community}, {write_commuity, WriteCommunity}].

