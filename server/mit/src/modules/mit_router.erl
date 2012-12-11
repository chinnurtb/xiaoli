-module(mit_router).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-export([load/0, add/1, update/2]).

-define(TAB, node_routers).

-define(PORT_TAB, node_routers).

load() ->
    mit_node:load(router).

add({ports, Dn, Ports}) ->
    {ok, Node} = mit:lookup(Dn),
    mit_port:insert(?PORT_TAB, Node, Ports).

update(Dn, Attrs) ->
    mit_node:update(?TAB, Dn, Attrs).

