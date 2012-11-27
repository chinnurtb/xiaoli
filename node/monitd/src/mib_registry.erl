%%%----------------------------------------------------------------------
%%% File    : mib_registry.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Mib Registry
%%% Created : 18 Jul 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mib_registry).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-import(lists, [reverse/1]).

-import(proplists, [get_value/2]).

-export([start_link/0, setup/1,  lookup/2]).

-behavior(gen_server).

-export([init/1,
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2,
        code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

setup({miboids, Records}) ->
	gen_server:cast(?MODULE, {setup, miboids, Records}).

lookup(Mib, Tab) ->
    case ets:lookup(mib, {Mib, Tab}) of
    [] -> ?WARNING("oids_not_found: ~p ~p", [Mib, Tab]), [];
    [{_, Oids}] -> Oids
    end.

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    ets:new(mib, [set, protected, named_table]),
    {ok, state}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast({setup, miboids, Records}, State) ->
    ?INFO("setup miboids: ~p", [length(Records)]),
    MibDict =
    lists:foldl(fun(Record, Dict) ->
        Mib = get_value(mib, Record),
        Grp = get_value(grp, Record),
        Name = get_value(name, Record),
        Oid = get_value(oid, Record),
        Key = {atom(Mib), atom(Grp)},
        Var = {atom(Name), mib_oid:new(Oid)},
        case dict:find(Key, Dict) of
        {ok, Oids} -> dict:store(Key, [Var|Oids], Dict);
        error -> dict:store(Key, [Var], Dict)
        end
    end, dict:new(), Records),
    [ets:insert(mib, {Key, reverse(Oids)}) || 
		{Key, Oids} <- dict:to_list(MibDict)],
	{noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ets:delete(mib).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
atom(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B));

atom(L) when is_list(L) ->
	list_to_atom(L).

