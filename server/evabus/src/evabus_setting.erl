%%%----------------------------------------------------------------------
%%% File    : evabus_setting.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Evabus setting
%%% Created : 29 May. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(evabus_setting).

-author('ery.lee@gmail.com').

-import(proplists, [get_value/2]).

-include_lib("elog/include/elog.hrl").

-export([start_link/0,
		lookup/1]).

-behavior(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

lookup(Key) when is_atom(Key) ->
    case ets:lookup(evabus_setting, Key) of
    [{_, Val}] -> Val;
    [] -> undefined
    end.

init([]) ->
    ets:new(evabus_setting, [set, protected, named_table]), 
    handle_info(reload, state),
	?INFO("~p is started.", [?MODULE]),
    {ok, state}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(reload, State) ->
	{ok, Records} = emysql:select(fault_settings),
	lists:foreach(fun(Record) -> 
		Attr = get_value(attr, Record),
		Type = get_value(type, Record),
		Val = get_value(val, Record),
		ets:insert(evabus_setting, {atom(Attr), format(Type, Val)})
	end, Records),
	erlang:send_after(300 * 1000, self(), reload),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
atom(Attr) ->
	list_to_atom(binary_to_list(Attr)).

format(<<"boolean">>, Val) ->
	list_to_atom(binary_to_list(Val));	
format(<<"integer">>, Val) ->
	list_to_integer(binary_to_list(Val));	
format(_, Val) ->
	binary_to_list(Val).

