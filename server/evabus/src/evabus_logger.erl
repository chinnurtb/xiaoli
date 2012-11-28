%%%----------------------------------------------------------------------
%%% File    : evabus_logger.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : evabus logger
%%% Created : 30 May. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(evabus_logger).

-author('ery.lee@gmail.com').

-include("alarm.hrl").

-include("event.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/0, log/2]).

-behavior(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

log(Type, Event) ->
	gen_server2:cast(?MODULE, {log, Type, Event}).

init([]) ->
    {ok, state}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast({log, Type, #event{name=Name, 
	sender = Sender, source = Source}}, State) ->
	?INFO("event ~p: name=~p, sender=~p, source=~p", [Type, Name, Sender, Source]), 
	{noreply, State};

handle_cast({log, Type, #alarm{alarm_key=Key, 
	agent = Agent, source = Source}}, State) ->
	?INFO("alarm ~p: key=~p, agent=~p, source=~p", [Type, Key, Agent, Source]), 
	{noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

