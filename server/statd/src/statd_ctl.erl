%%%----------------------------------------------------------------------
%%% File    : statd_ctl.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : statd control
%%% Created : 12 Nov. 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(statd_ctl).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-compile(export_all).

status() ->
    {InternalStatus, _ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p.", [node(), InternalStatus]),
    case lists:keysearch(statd, 1, application:which_applications()) of
	false ->
		?PRINT_MSG("statd is not running~n");
	{value,_Version} ->
		?PRINT_MSG("statd is running~n")
    end.

cluster_info() ->
	?PRINT("running_db_nodes: ~p~n", [mnesia:system_info(running_db_nodes)]).

hub_info() ->
    ?PRINT("~p~n", [statd_hub:stats()]).

