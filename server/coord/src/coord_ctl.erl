%%%----------------------------------------------------------------------
%%% File    : coord_ctl.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : coord control
%%% Created : 24 Arg 2011
%%% Updated : 24 Apr.2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(coord_ctl).

-author('ery.lee@gmail.com').

-compile(export_all).

-include_lib("mit/include/mit.hrl").

-include_lib("elog/include/elog.hrl").

syncdb() ->
    ?PRINT_MSG("loading db...~n"),
    coord_db:reload(),
    ?PRINT_MSG("syncdb...~n"),
    coord_db:syncdb(),
    ?PRINT_MSG("[done] ~n").

run() ->
    ?PRINT_MSG("runing...~n"),
    ?INFO("begin to dispatch ~p nodes...", [mnesia:table_info(node, size)]),
    try
        dispatch(mnesia:first(node)),
        ?PRINT("dispatched, done.~n", [])
    catch
    _:Err -> 
        ?ERROR("dispatch error: ~p", [Err]),
        ?ERROR("~p", [erlang:get_stacktrace()])
    end.

dispatch('$end_of_table') -> 
    ok;
dispatch(Dn) ->
    case mnesia:dirty_read(node, Dn) of
    [Node] ->
        coord:dispatch({monitor, Node}),
        timer:sleep(2);
    [] ->
        ?ERROR("~p is not found", [Dn])
    end,
    dispatch(mnesia:next(Dn)).
    
status() ->
    {InternalStatus, _ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p.", [node(), InternalStatus]),
    case lists:keysearch(coord, 1, application:which_applications()) of
	false ->
		?PRINT_MSG("coord is not running~n");
	{value,_Version} ->
		?PRINT_MSG("coord is running~n")
    end.

shards() ->
    ?PRINT("~p~n", [coord_dist:shards()]).

cluster_info() ->
    ?PRINT("~p~n", [nodes()]).

presences() ->
    ?PRINT("~p~n", [coord:presences()]).

dispatches(Dn) ->
    ?PRINT("~p~n", [coord_dist:dispatches(Dn)]).

lookup(Dn) ->
    ?PRINT("~p~n", [mit:lookup(Dn)]).

