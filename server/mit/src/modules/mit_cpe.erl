-module(mit_cpe).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-export([load/0, update/2]).

-define(TAB, node_cpes).

load() ->
    mit_node:load(cpe).

update(Dn, Attrs) ->
    mit_node:update(?TAB, Dn, Attrs).

