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
	run(undefined).

%dispatch tasks
run(BDN) ->
    ?PRINT_MSG("runing...~n"),
	Dispatch = fun(Dn) ->
		case mit:lookup(Dn) of
		{ok, Entry} ->
			case should_dispath(BDN, Entry) of
			true -> 
				increas(dispatched, 1),
				coord_dist:dispatch(Entry),
                timer:sleep(3),
				case get(dispatched) rem 1000 of
				0 -> ?INFO("~p entries dispatched.", [get(dispatched)]);
				_ -> ignore
				end;
			false ->
				ignore
			end;
		{false, _} -> 
			?ERROR("~p is not found", [Dn])
		end
	end,
	spawn(fun() -> 
		AllDn = mit:alldn(),
		?INFO("begin to dispatch ~p entries...", [length(AllDn)]),
		try
			lists:foreach(Dispatch, AllDn),
			?INFO("~p dispatched, done.", [get(dispatched)])
		catch
			_:Err -> ?ERROR("dispatch error: ~p", [Err])
		end
	end),
    ?PRINT("dispatching tasks...~n", []),
    ?PRINT("relax and enjoy:)~n", []).

should_dispath(_BDN, #entry{class = <<"/top/ossSite">>}) ->
    false;
should_dispath(_BDN, #entry{oper_state = 2, class = <<"/top/ossIpDevice/ossWirelessAccessPoint">>}) ->
    false;
should_dispath(undefined, _Entry) ->
	true;
should_dispath(BDN, #entry{dn = Dn}) ->
	lists:suffix(BDN, binary_to_list(Dn));
should_dispath(_BDN, _Entry) ->
    true.

increas(dispatched, Num) ->
	case get(dispatched) of
	undefined ->
		put(dispatched, Num);
	Total ->
		put(dispatched, Total+Num)
	end.

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

