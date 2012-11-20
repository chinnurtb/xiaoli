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
run(City) ->
    ?PRINT_MSG("runing...~n"),
	Dispatch = fun(Dn) ->
		case mit:lookup(Dn) of
		{ok, Node} ->
			case should_dispath(City, Node) of
			true -> 
				increas(dispatched, 1),
				coord_dist:dispatch(Node),
                timer:sleep(2),
				case get(dispatched) rem 1000 of
				0 -> ?INFO("~p entries dispatched.", [get(dispatched)]);
				_ -> ignore
				end;
			false ->
                ?INFO("ignore ~p", [Dn]),
				ignore
			end;
		{false, _} -> 
			?ERROR("~p is not found", [Dn])
		end
	end,
    AllDn = mit:alldn(),
    ?INFO("begin to dispatch ~p nodes...", [length(AllDn)]),
    try
        lists:foreach(Dispatch, AllDn),
        ?INFO("~p dispatched, done.", [get(dispatched)])
    catch
        _:Err -> 
            ?ERROR("dispatch error: ~p", [Err]),
            ?ERROR("~p", [erlang:get_stacktrace()])
    end,
    ?PRINT("dispatching tasks...~n", []),
    ?PRINT("relax and enjoy:)~n", []).

should_dispath(undefined, _Node) ->
	true;
should_dispath(City, Node) when is_list(City) ->
    should_dispath(list_to_binary(City), Node);
should_dispath(City, #node{city= City}) when is_binary(City) ->
    true;
should_dispath(_, _) ->
    false.

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

