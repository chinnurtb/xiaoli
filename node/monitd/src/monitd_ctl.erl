%%%----------------------------------------------------------------------
%%% File    : monitd_ctl.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Opengoss node control
%%% Created : 15 Jan 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(monitd_ctl).

-author('ery.lee@gmail.com').

-compile(export_all).  

-include_lib("elog/include/elog.hrl").

workers() ->
	[?PRINT("~p~n", [N]) || N <- nodes()].
	
coord() ->
	Pids = chash_pg:get_pids(monitd_coord),
	Coords = [{Pid, node(Pid)} || Pid <- Pids],
	[?PRINT("~p~n", [C]) || C <- Coords].

tasks() ->
    print_task(ets:first(monitor_task)).

task(TaskId) ->
    case ets:lookup(monitor_task, TaskId) of
	[Task] -> ?PRINT("~p~n", [Task]);
	[] -> ?PRINT_MSG("not found~n")
	end.

status() ->
    {InternalStatus, _ProvidedStatus} = init:get_status(),
    ?PRINT("node ~p is ~p.~n", [node(), InternalStatus]),
    case lists:keysearch(monitd, 1, application:which_applications()) of
	{value,_Version} ->
		?PRINT("monitd is running~n", []);
	false ->
		?PRINT("monitd is not running~n", [])
    end.

print_task('$end_of_table') ->
    ok;
print_task(TaskId) ->
    [Task|_] = ets:lookup(monitor_task, TaskId),
    ?PRINT("~p~n", [Task]),
    print_task(ets:next(monitor_task, TaskId)).

