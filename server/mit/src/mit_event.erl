%%%----------------------------------------------------------------------
%%% File    : mit_event.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : MIT change callback.
%%% Created : 10 May 2012
%%% License : http://www.opengoss.com/license
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_event). 

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/0, 
        notify/2]).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {channel}).

start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

notify(Oper, Node) when is_record(Node, mit_node) ->
	gen_server2:cast(?MODULE, {notify, Oper, Node}).

init([]) ->
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    ?INFO_MSG("mit_event is started."),
    {ok, #state{channel = Channel}}.

open(C) ->
	{ok, Channel} = amqp:open_channel(C),
	amqp:topic(Channel, "mit.event"),
	Channel.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast({notify, Oper, Node}, #state{channel = Ch} = State) ->
	RouteKey = iolist_to_binary(["mit.", atom_to_list(Oper)]),
	amqp:publish(Ch, "mit.event", term_to_binary({Oper, Node}), RouteKey),
	{noreply, State};

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

handle_info(Info, State) ->
    {stop, {badinfo, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

