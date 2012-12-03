%%%----------------------------------------------------------------------
%%% File    : mit_server.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit server to handle entry
%%% Created : 05 July 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_server).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/0, emit/1, stop/0]).

-behavior(gen_server).

%%callback
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {channel}).

start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

emit(Event) ->
	gen_server2:cast(?MODULE, {emit, Event}).

stop() ->
    gen_server2:call(?MODULE, stop).

init([]) ->
    process_flag(trap_exit, true),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    ?INFO_MSG("mit_server is started."),
    {ok, #state{channel = Channel}}.

open(C) ->
	{ok, Channel} = amqp:open_channel(C),
    {ok, MitQ} = amqp:queue(Channel, atom_to_list(node())),
    amqp:topic(Channel, "mit.server"),
    amqp:bind(Channel, "mit.server", MitQ, "#"),
	amqp:consume(Channel, MitQ),
	Channel.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast({emit, Event}, #state{channel = Channel} = State) ->
    amqp:send(Channel, <<"event">>, term_to_binary(Event)),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({deliver, <<"node">>, _, Payload}, State) ->
    store(binary_to_term(Payload)),
    {noreply, State};

handle_info({deliver, <<"board">>, _, Payload}, State) ->
    store(binary_to_term(Payload)),
    {noreply, State};

handle_info({deliver, <<"port">>, _, Payload}, State) ->
    store(binary_to_term(Payload)),
    {noreply, State};

handle_info({amqp, disconnected}, State) ->
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn}, State) ->
	{noreply, State#state{channel = open(Conn)}};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

store(Data) ->
    ?INFO("~p", [Data]).


