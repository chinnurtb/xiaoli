-module(mit_eoc).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-export([load/0, update/2, add/1]).

-define(TAB, node_eocs).

-define(PORT_TAB, port_eocs).

load() ->
    mit_node:load(eoc).

add({_, _Dn, []}) ->
    ignore;

%board: {Index, Attrs}
add({boards, Dn, Boards}) ->
    ?INFO("eoc boards: ~s", [Dn]),
    ?INFO("~p", [Boards]);
    %compare,
    %insert or update

add({ports, Dn, Ports}) ->
    {ok, Node} = mit:lookup(Dn),
    mit_port:insert(?PORT_TAB, Node, Ports).

update(Dn, Attrs) ->
    mit_node:update(?TAB, Dn, Attrs).

