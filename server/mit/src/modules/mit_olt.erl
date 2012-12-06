-module(mit_olt).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([load/0, add/1, update/2]).

-define(TAB, node_olts).
 
-define(PORT_TAB, port_olts).

load() ->
    mit_node:load(olt).

add({_, _Dn, []}) ->
    ignore;

add({boards, Dn, Boards}) ->
    {ok, Node} = mit:lookup(Dn),
    mit_board:insert(Node, Boards);

add({ports, Dn, [Port|_]=Ports}) ->
    ?INFO("~p", [Port]),
    {ok, Node} = mit:lookup(Dn),
    mit_port:insert(?PORT_TAB, Node, Ports).

update(Dn, Attrs) ->
    mit_node:update(?TAB, Dn, Attrs).

