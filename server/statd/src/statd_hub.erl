%%%----------------------------------------------------------------------
%%% File    : statd_hub.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Status Event Hub.
%%% Created : 08 May. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(statd_hub).

-author('ery.lee@gmail.com').

-include("event.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([start_link/0, emit/1, stats/0]).

-behavior(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {channel}).

start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

emit(Event) when is_record(Event, event) ->
	gen_server2:cast(?MODULE, {emit, Event}).

stats() ->
	gen_server2:call(?MODULE, stats).

init([]) ->
	put(emitted, 0),
	{ok, Conn} = amqp:connect(),
    Chan = open(Conn),
	?INFO("~p is started.", [?MODULE]),
    {ok, #state{channel = Chan}}.

open(Conn) ->
	{ok, Chan} = amqp:open_channel(Conn),
	Chan.

handle_call(stats, _From, State) ->
	{reply, [{emitted, get(emitted)}], State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast({emit, #event{name = Name} = Event}, 
	#state{channel = Chan} = State) ->
	Key = "event." ++ atom_to_list(Name),
	amqp:publish(Chan, <<"oss.event">>, term_to_binary(Event), Key),
	{noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

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

