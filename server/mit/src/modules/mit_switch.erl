-module(mit_switch).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([load/0, add/1, update/2]).

-define(TAB, node_switchs).

-define(PORT_TAB, port_switchs).

load() ->
    mit_node:load(switch).

add({ports, _Dn, []}) ->
    ignore;

add({ports, Dn, Ports}) ->
    {ok, Node} = mit:lookup(Dn),
    mit_port:insert(?PORT_TAB, Node, Ports).

update(Dn, Attrs) ->
    mit_node:update(?TAB, Dn, Attrs).


