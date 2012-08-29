%%%----------------------------------------------------------------------
%%% File    : evabus_ctl.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Metrix control
%%% Created : 12 Nov. 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2010, www.opengoss.com
%%%----------------------------------------------------------------------
-module(evabus_ctl).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-compile(export_all).

status() ->
    {InternalStatus, _} = init:get_status(),
    ?PRINT("node ~p is ~p.~n", [node(), InternalStatus]),
    case lists:keysearch(evabus, 1, application:which_applications()) of
	false ->
		?PRINT("evabus is not running~n", []);
	{value,_Version} ->
		?PRINT("evabus is running~n", [])
    end.

mnesia_info() ->
    ?PRINT("~p~n", [mnesia:system_info(all)]).

