-module(olt_zte_c220).

-include_lib("mit/include/mit.hrl").

-include_lib("elog/include/elog.hrl").

-export([disco/3, coll/3]).

disco(boards, Node, Args) ->
    {ok, []};

disco(ports, Node, Args) ->
    {ok, []};

coll(ponpower, Node, Args) ->
    {ok, [], Args};

coll(X, Node, Args) ->
    ?ERROR("unsupport coll: ~p", [X]),
    {ok, [], Args}.


    
