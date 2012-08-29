%%---------------------------------------------------------------------- 
%%% File    : evabus.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : event and alarm bus
%%% Created : 29 May 2012
%%% License : http://www.opengoss.com/license
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(evabus).

-author('ery.lee@gmail.com').

-include("event.hrl").

-include("alarm.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/0,
		 info/0,
		 send/1,
		 severity/1]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        prioritise_call/3,
        handle_cast/2, 
        handle_info/2, 
        prioritise_info/2,
        terminate/2, 
        code_change/3]).

-record(state, {channel}).

start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

info() ->
    gen_server2:call(?MODULE, info).

send(Event) when is_record(Event, event) ->
	gen_server2:cast(?MODULE, {event, Event});

send(Alarm) when is_record(Alarm, alarm) ->
	gen_server2:cast(?MODULE, {alarm, Alarm}).

severity(critical) -> 5;
severity(major) -> 4;
severity(minor) -> 3;
severity(warning) -> 2;
severity(undeterminate) -> 1;
severity(clear) -> 0.

init([]) ->
    put(event_filtered, 0),
    put(event_received, 0),
    put(alarm_filtered, 0),
    put(alarm_received, 0),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
	?INFO("~p is started.", [?MODULE]),
    {ok, #state{channel = Channel}}.

open(Conn) ->
	{ok, Chan} = amqp:open_channel(Conn),
	{ok,Q} = amqp:queue(Chan, node()),
	
	amqp:topic(Chan, <<"oss.event">>),
	amqp:topic(Chan, <<"oss.alarm">>),

	amqp:bind(Chan, <<"oss.event">>, Q, <<"event.#">>),
	amqp:bind(Chan, <<"oss.alarm">>, Q, <<"alarm.#">>),
	amqp:consume(Chan, Q),
	
	Chan.

handle_call(info, _From, State) ->
	Reply = [{Key, get(Key)} || Key <- 
		[event_filtered, event_received,
		 alarm_filtered, alarm_received]],
   {reply, Reply, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

prioritise_call(info, _From, _State) ->
    10;

prioritise_call(_, _From, _State) ->
    0.

handle_cast({event, Event}, State) ->
	handle_event(Event),
    {noreply, State};

handle_cast({alarm, Alarm}, State) ->
	handle_alarm(Alarm),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({deliver, <<"event.", _Name/binary>>, _Props, Payload}, State) ->
    handle_event(binary_to_term(Payload)),
	{noreply, State};

handle_info({deliver, <<"alarm.", _Name/binary>>, _Props, Payload}, State) ->
    handle_alarm(binary_to_term(Payload)),
	{noreply, State};

handle_info({amqp, disconnected}, State) ->
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn}, State) ->
	{noreply, State#state{channel = open(Conn)}};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

prioritise_info({amqp, disconnected}, _State) ->
	10;
prioritise_info({amqp, reconnected, _}, _State) ->
	10;
prioritise_info(_, _State) ->
    0.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event(Event) ->
	logevent(Event),
    incre(event_received),
	case evabus_filter:filter(Event) of
	true ->
		incre(event_filtered),
		evabus_logger:log(filtered, Event);
	false ->
		evabus_correlator:analyze(Event)
	end.

handle_alarm(Alarm) ->
    incre(alarm_received),
	case evabus_filter:filter(Alarm) of
	true ->
		incre(alarm_filtered),
		evabus_logger:log(filtered, Alarm);
	false ->
		evabus_correlator:analyze(Alarm)
	end.

incre(Key) ->
	put(Key, get(Key)+1).

logevent(#event{name = avail_status}) ->
	ignore;
logevent(#event{name = ping_status}) ->
	ignore;
logevent(#event{name = snmp_status}) ->
	ignore;
logevent(#event{name = Name, sender = Sender, source = Source, severity = Severity}) ->
	?INFO("~p ~p event received", [Severity, Name]),
    ?INFO("from ~p/~p", [Sender, Source]).
