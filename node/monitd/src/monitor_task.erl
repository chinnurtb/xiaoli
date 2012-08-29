%%%----------------------------------------------------------------------
%%% File    : monitor_task.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : monitor task
%%% Created : 13 Jul 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
%%%----------------------------------------------------------------------
-module(monitor_task).

-include("monitor.hrl").

-include_lib("elog/include/elog.hrl").

-export([run/1]).

run(#monitor_task{mod = Mod, args = Args, handler = Handler} = Task) -> 
    {value, Dn} = dataset:get_value(dn, Args),
	case should_run(Args) of
	true ->
		try apply(Mod, run, [Args]) of
		{ok, DataList, NewArgs} ->
			erlang:apply(Handler, [DataList]),
			{ok, Task#monitor_task{args=NewArgs}};
		{error, Reason, NewArgs} ->
			?WARNING("dn: ~p, mod: ~p, ~n reason: ~p", [Dn, Mod, Reason]),
			{ok, Task#monitor_task{args=NewArgs}};
		ignore ->
			{ok, Task}
		catch
		_:Err ->
			?ERROR("error: ~p~n ~p~n ~p", [Err, Dn, erlang:get_stacktrace()]),
			{ok, Task}
		end;
    false ->
		{ok, Task}
    end.

should_run(Args) ->
   not (2 == proplists:get_value(oper_state, Args, 2)).

