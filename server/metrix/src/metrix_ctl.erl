%%%----------------------------------------------------------------------
%%% File    : metrix_ctl.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Metrix control
%%% Created : 12 Nov. 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2010, www.opengoss.com
%%%----------------------------------------------------------------------
-module(metrix_ctl).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-export([start/0, init/0, process/1]).  

-define(STATUS_SUCCESS, 0).

-define(STATUS_ERROR,   1).

-define(STATUS_USAGE,   2).

-define(STATUS_BADRPC,  3).

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    SNode1 = case string:tokens(SNode, "@") of
		[_Node, _Server] ->
		    SNode;
		_ ->
		    case net_kernel:longnames() of
			 true ->
			     SNode ++ "@" ++ inet_db:gethostname() ++
				      "." ++ inet_db:res_option(domain);
			 false ->
			     SNode ++ "@" ++ inet_db:gethostname();
			 _ ->
			     SNode
		     end
	    end,
	    Node = list_to_atom(SNode1),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     ?PRINT("RPC failed on the node ~p: ~p~n",
				       [Node, Reason]),
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.

init() ->
	ok.

process(["status"]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p. Status: ~p~n",
              [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(metrix, 1, application:which_applications()) of
        false ->
            ?PRINT("metrix is not running~n", []),
            ?STATUS_ERROR;
        {value,_Version} ->
            ?PRINT("metrix is running~n", []),
			PInfo = erlang:process_info(whereis(metrix), 
				[dictionary,heap_size,total_heap_size]),
			?PRINT("metrix process:~n~n~p~n~n", [PInfo]),
			?PRINT("metrix status:~n~n~p~n~n", [sys:get_status(metrix)]),
			PInfo2 = erlang:process_info(whereis(metrix_store), 
				[dictionary,heap_size,total_heap_size]),
			?PRINT("metrix_store process:~n~n~p~n~n", [PInfo2]),
			?PRINT("metrix_store status:~n~n~p~n~n", [sys:get_status(metrix_store)]),
			PInfo3 = erlang:process_info(metrix_store:errdb_client(), 
				[dictionary,message_queue_len,heap_size,total_heap_size]),
			?PRINT("errdb_client process:~n~n~p~n~n", [PInfo3]),
            ?STATUS_SUCCESS
    end;

process(["mysql"]) ->
	?PRINT("Mysql Connections Info: ~n~p~n", [emysql:info()]);

process(["stop"]) ->
	metrix_app:stop(),
    init:stop(),
    ?STATUS_SUCCESS;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;

process(["mnesia"]) ->
    ?PRINT("~p~n", [mnesia:system_info(all)]),
    ?STATUS_SUCCESS;

process(["mnesia", "info"]) ->
    mnesia:info(),
    ?STATUS_SUCCESS;

process(["mnesia", Arg]) when is_list(Arg) ->
    case catch mnesia:system_info(list_to_atom(Arg)) of
	{'EXIT', Error} -> ?PRINT("Error: ~p~n", [Error]);
	Return -> ?PRINT("~p~n", [Return])
    end,
    ?STATUS_SUCCESS;

process(_) -> 
    print_usage(),
    ?STATUS_ERROR.

print_usage() ->
	CmdDescs = [{"status", "get metrix status"},
	 {"stop", "stop metrix"},
	 {"restart", "restart metrix"},
	 {"log_rotation", "log rotation"},
	 {"mnesia [info]", "show information of Mnesia system"}],
    MaxCmdLen =
	lists:max(lists:map(
		    fun({Cmd, _Desc}) ->
			    length(Cmd)
		    end, CmdDescs)),
    NewLine = io_lib:format("~n", []),
    FmtCmdDescs =
	lists:map(
	  fun({Cmd, Desc}) ->
		  ["  ", Cmd, string:chars($\s, MaxCmdLen - length(Cmd) + 2),
		   Desc, NewLine]
	  end, CmdDescs),
    ?PRINT(
      "Usage: metrix_ctl [--node nodename] command [options]~n"
      "~n"
      "Available commands in this metrix node:~n"
      ++ FmtCmdDescs ++
      "~n"
      "Examples:~n"
      "  metrix_ctl --node metrix@host stop~n",
     []).

