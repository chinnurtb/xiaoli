%%%----------------------------------------------------------------------
%%% File    : evabus_class.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Event class 
%%% Created : 03 Apr. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(evabus_class).

-author('ery.lee@gmail.com').

-import(proplists, [get_value/2]).

-include_lib("elog/include/elog.hrl").

-export([start_link/0,
		lookup/1,
		lookup/2]).

-behavior(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

lookup(Name) when is_atom(Name) ->
	lookup(class, Name).

lookup(class, Name) when is_atom(Name) ->
    case ets:lookup(alarm_class, Name) of
    [{_, Class}] -> {ok, Class};
    [] -> {false, Name}
    end;

lookup(class, Name) ->
    lookup(class, extbif:to_atom(Name)).

init([]) ->
    ets:new(alarm_class, [set, protected, named_table]), 
    handle_info(reload, #state{}),
    {ok, #state{}}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(reload, State) ->
	{ok, Classes} = epgsql:select(main, alarm_classes),
	[ets:insert(alarm_class, {atom(get_value(name, C)), C}) || C <- Classes],
	erlang:send_after((300 + random:uniform(100)) * 1000, self(), reload),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

atom(Bin) when is_binary(Bin) ->
	list_to_atom(binary_to_list(Bin));
atom(L) when is_list(L) ->
	list_to_atom(L).

