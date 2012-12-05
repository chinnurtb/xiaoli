%%%---------------------------------------------------------------------- 
%%% File    : evabus_correlator.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : event and alarm correlator
%%% Created : 29 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(evabus_correlator).

-author('ery.lee@gmail.com').

-include("event.hrl").

-include("alarm.hrl").

-include_lib("mit/include/mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(erlang, [send_after/3]).

%core api
-export([analyze/1]).

-export([start_link/0, info/0]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        prioritise_call/3,
        handle_cast/2, 
        handle_info/2, 
        prioritise_info/2,
        terminate/2, 
        code_change/3]).

-record(state, {}).

start_link() ->
	gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

%event or alarm
analyze(Eva) ->
	gen_server2:cast(?MODULE, {analyze, Eva}).

info() ->
	gen_server2:call(?MODULE, info).

init([]) ->
	%{Key, Event, AgingTimer}
	ets:new(history_event, [named_table]),
	ets:new(history_alarm, [named_table]),
	?INFO("~p is started.", [?MODULE]),
    {ok, #state{}}.

handle_call(info, _From, State) ->
	{reply, get(), State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

prioritise_call(info, _From, _State) ->
    10;

prioritise_call(_, _From, _State) ->
    0.

handle_cast({analyze, Event}, State) 
	when is_record(Event, event) ->
	case catch analyze(event, Event) of
	ignore -> 
		ignore;
	{ok, NewEvent} when is_record(NewEvent, event) -> 
		evabus_mapper:mapping(NewEvent);
	{ok, Events} when is_list(Events) ->
		[evabus_mapper:mapping(E) || E <- Events];
	{'EXIT', Reason} -> 
		?ERROR("~p~n~p", [Reason, Event])
	end,
    {noreply, State};

handle_cast({analyze, Alarm}, State)
	when is_record(Alarm, alarm) ->
	case catch analyze(alarm, Alarm) of
	ignore -> 
		ignore;
	{ok, NewAlarm} when is_record(NewAlarm, alarm) -> 
		evabus_store:save(NewAlarm);
	{'EXIT', Reason} -> 
		?ERROR("~p~n~p", [Reason, Alarm])
	end,
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({emit, fitap_offline, #event{source = Source} = Event}, State) ->
	evabus_mapper:mapping(Event),
	%update_fitap_status(Event),
	ets:delete(history_event, event_key(Source, fitap_offline)),
	{noreply, State};

handle_info({emit, event, Key}, State) ->
	Events = ets:lookup(history_event, Key),
	[evabus_mapper:mapping(E) || {_, E, _} <- Events],
	ets:delete(history_event, Key),
	{noreply, State};

%aging
handle_info({aging, event, Key}, State) ->
	ets:delete(history_event, Key),
	{noreply, State};

handle_info({aging, alarm, Key}, State) ->
	ets:delete(history_alarm, Key),
	{noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

prioritise_info(_Info, _State) ->
	0.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

analyze(event, #event{name = apOfflineTrap, source = Source} = Event) ->
	%update mit_ap immediately
	%update_fitap_status(Event),

	%delete cached fitap_offline
	case ets:lookup(history_event, event_key(Source, fitap_offline)) of
	[] -> 
		ok;
	[{Key, _Event, Ref}] -> 
		cancel(Ref),
		ets:delete(history_event, Key)
	end,

	%cache this trap
	TrapKey = event_key(Source, apOfflineTrap),
	TRef = send_after(600*1000, self(), {aging, event, TrapKey}),
	ets:insert(history_event, {TrapKey, Event, TRef}),
	{ok, Event};

analyze(event, #event{name = fitap_online, source = Source} = Event) ->
	%update fitap status
    %fix event not consistent issue
	%update_fitap_status(Event),

	%delete cached fitap_status
	case ets:lookup(history_event, event_key(Source, fitap_offline)) of
	[] -> 
		ok;
	[{Key, _Event, Ref}] -> 
		cancel(Ref),
		ets:delete(history_event, Key)
	end,
	{ok, Event};

analyze(event, #event{name = fitap_offline, source = Source, timestamp = Ts} = Event) ->
	CacheEvent = fun() ->
		TRef = send_after(600*1000, self(), {emit, fitap_offline, Event}), %600*
		ets:insert(history_event, {event_key(Source, fitap_offline), Event, TRef})
	end,
	case ets:lookup(history_event, event_key(Source, apOfflineTrap)) of
	[] ->
		%是否判断轮询的清除告警?
		%if alarm, cache for a while
		CacheEvent();
	[{_, #event{timestamp = TrapTs, severity = TrapSeverity}, _}] ->
		if
		Ts < TrapTs -> 
			?ERROR_MSG("trap arrived first, discard this fitap_offline event");
		true ->
			case TrapSeverity of
			clear ->
				?INFO_MSG("trap clear, fit_status offline later."),
				%cache for a while
				CacheEvent();
			_ -> 
				ignore
			end
		end
	end,
	ignore;

analyze(event, #event{name = '/Status/Ping', severity = Severity, sender = Dn} = Event) ->
	Key = event_key(Dn, '/Status/Ping'),
	Compress = evabus_setting:lookup('snmp.status.compress'),
	case {Compress, Severity} of
	{true, clear} ->
		ok; %do nothing.
	{true, _} ->
		SnmpKey = event_key(Dn, '/Status/Snmp'),
		case ets:lookup(history_event, SnmpKey) of
		[] -> ok;
		[{_, _SnmpEvent, Ref}] -> %drop 
			cancel(Ref),
			ets:delete(history_event, SnmpKey)
		end,
		TRef = send_after(300*1000, self(), {aging, event, Key}),
		ets:insert(history_event, {Key, Event, TRef});
	{_, _} -> 
		ok
	end,
	{ok, Event};

analyze(event, #event{name = '/Status/Snmp', severity = Severity, sender = Dn} = Event) ->
	Key = event_key(Dn, '/Status/Snmp'),
	Compress = evabus_setting:lookup('snmp.status.compress'),
	case {Compress, Severity} of
	{true, clear} ->
		case ets:lookup(history_event, Key) of
		[] -> ok; 
		[{_, _Old, TRef}] -> %clear old event
			cancel(TRef), ets:delete(history_event, Key)
		end,
		{ok, Event};
	{true, _} ->
		case ets:lookup(history_event, event_key(Dn, '/Status/Ping')) of
		[] -> %cache for a while
			TRef = send_after(300*1000, self(), {emit, event, Key}),
			ets:insert(history_event, {Key, Event, TRef}),
			ignore;
		[{_, #event{severity=clear}, _}] -> %send now
			{ok, Event};
		[{_, _PingEvent, _}] ->
			ignore
		end;
	{_, _} -> %don't compress
		{ok, Event}
	end;

analyze(event, #event{name = avail_status, sender = Dn, timestamp = _Ts, vars = Vars}) ->
	TimeAt = {datetime, {date(), time()}},
    Res = epgsql:update(main, nodes, [{updated_at, TimeAt}|Vars], {dn, Dn}),
    ?INFO("~p", [Res]),
	ignore;

analyze(event, Event) ->
	{ok, Event};

analyze(alarm, #alarm{alarm_name = apOfflineTrap, severity = Severity} = Alarm) ->
    if
    Severity == 0 ->
        clear_longoffline(Alarm);
    true ->
        ignore
    end,
	{ok, Alarm};

analyze(alarm, Alarm) ->
	{ok, Alarm}.

cancel(undefined) ->
	ok;

cancel(TRef) ->
	erlang:cancel_timer(TRef).

event_key(SenderOrSource, Name) ->
	{SenderOrSource, Name}.

clear_longoffline(#alarm{alarm_name = apOfflineTrap} = Alarm) ->
	LongOffline = evabus_setting:lookup('long.offline.alarm'),
	case LongOffline of
	true -> %clear LongOffline
		Summary = list_to_binary([Alarm#alarm.summary, ", AP上线时间: ",
			extbif:strftime(extbif:datetime(Alarm#alarm.timestamp))]),
		Event = #event{name = long_offline, 
					   sender = Alarm#alarm.agent,
					   source = Alarm#alarm.source,
					   evtkey = <<"long_offline">>,
					   severity = clear,
					   summary = Summary,
					   timestamp = extbif:timestamp(),
					   manager = node(),
					   from = evabus},
		evabus:send(Event);
	_ ->
		ignore
	end.

