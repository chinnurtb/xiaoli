%%%---------------------------------------------------------------------- 
%%% File    : evabus.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mapping event to alarm
%%% Created : 29 May 2012
%%% License : http://www.opengoss.com/license
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(evabus_mapper).

-include("event.hrl").

-include("alarm.hrl").

-include_lib("mit/include/mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([start_link/0, mapping/1]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).

-record(state, {}).

start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

mapping(Event) when is_record(Event, event) ->
	gen_server2:cast(?MODULE, {mapping, Event}).

init([]) ->
	{ok, #state{}}.

handle_call(Req, _From, State) ->
	{stop, {error, {badreq, Req}}, State}.

handle_cast({mapping, Event}, State) ->
	try do_mapping(Event) of
	{ok, Alarm} -> 
		evabus:send(Alarm)
	catch
	_:Err -> 
		?ERROR("~p", [Err]),
		?ERROR("~p", [erlang:get_stacktrace()]),
		?ERROR("~p", [Event])
	end,
	{noreply, State};

handle_cast(Msg, State) ->
	{stop, {error, {badmsg, Msg}}, State}.

handle_info(Info, State) ->
	{stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_mapping(#event{name = Name, %event class name
				  sender = Sender, %ipaddr or dn
				  source = Source, %event source
				  evtkey = EvtKey, %deduplicate key
				  severity = Severity, %severity
				  summary = Summary, %event summary
				  timestamp = Timestamp, %event timestamp
				  manager = Manager, %manager that generate this event
				  from = From, %syslog, trap, evabus or monitor
				  trapoid = _TrapOid, %only for trap
				  vars = Vars}) ->
	RaisedTime = {datetime, extbif:datetime(Timestamp)},
	Alarm = #alarm{alarm_key = EvtKey,
				   alarm_name = Name,
				   alarm_state = alarm_state(Severity),
				   severity = evabus:severity(Severity),
				   summary = Summary,
				   first_occurrence = RaisedTime,
				   last_occurrence = RaisedTime,
				   timestamp = Timestamp,
				   manager = Manager,
                   agent = Sender,
				   vars = Vars},
	{ok, enrich(Severity, RaisedTime,
			enrich(source, {From, Source},
				enrich(sender, {From, Sender},
						enrich(class, transform(Alarm)))))}.

enrich(class, #alarm{alarm_name = Name, severity = Severity} = Alarm) ->
	{ok, Class} = evabus_class:lookup(Name),
    Alias = get_value(alias, Class),
    ClassId = get_value(id, Class),
	%update severity
	Severity1 = 
	if
	Severity == 0 -> 0;
	true -> get_value(severity, Class)
	end,
    ProbableCause = get_value(probable_cause, Class),
	SpecificProblem = get_value(specific_problem, Class),
	Alarm#alarm{
           class_id = ClassId,
           alarm_alias = Alias,
		   severity = Severity1,
		   probable_cause = ProbableCause,
		   specific_problem = SpecificProblem}.

enrich(sender, {_, Sender}, Alarm) ->
    {ok, Node} = mit:lookup(Sender),
    Alarm#alarm{node_id = Node#node.id};

enrich(source, {_, undefined}, Alarm) -> 
	Alarm;

enrich(source, {_From, Source}, Alarm) ->
	case mit:lookup(Source) of
    {ok, Node} -> 
        Alarm#alarm{source = Node#node.dn,
                    source_class = Node#node.category};
    _ ->
        Alarm
    end;

enrich(clear, RaisedTime, Alarm) ->
	Alarm#alarm{alarm_state=3,
                cleared = 1,
				cleared_time = RaisedTime};

enrich(_, _, Alarm) ->
	Alarm.

transform(#alarm{alarm_name = fitap_online} = Alarm) ->
	Alarm#alarm{alarm_name = apOfflineTrap, 
				alarm_key = "apOfflineTrap",
				severity = 0};

transform(#alarm{alarm_name = fitap_offline} = Alarm) ->
	Alarm#alarm{alarm_name = apOfflineTrap, 
				alarm_key = "apOfflineTrap",
				severity = 4};

transform(Alarm) ->
	Alarm.

alarm_state(clear) -> 
	2;

alarm_state(_) -> 
	0.

