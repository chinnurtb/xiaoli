%%%----------------------------------------------------------------------
%%% File    : evabus_store.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : event and alarm store
%%% Created : 25 Feb 2008
%%% Updated : 09 Sep 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(evabus_store).

-author('ery.lee@gmail.com').

-include("alarm.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([start_link/0,
        updated/2,
		save/1]).

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

save(Alarm) when is_record(Alarm, alarm) ->
	gen_server2:cast(?MODULE, {save, Alarm}).

%FIXME: Later
updated(Record, _Where) when is_list(Record) ->
    unsupported.

init([]) ->
	?INFO_MSG("evabus db is started."),
    {ok, #state{}}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast({save, Alarm}, State) ->
	try do_save(Alarm)
	catch
	_:Error -> 
		?ERROR("~p ~p", [Error, erlang:get_stacktrace()]),
		?ERROR("~p", [Alarm])
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

do_save(#alarm{alarm_key = AlarmKey,
			   alarm_name = AlarmName,
			   source = AlarmSource,
			   severity = Severity} = Alarm) ->
	%%query events with the same type
    SavedRes = 
	case select(AlarmKey, AlarmSource) of
    {ok, OldAlarm} ->
        OldSeverity = proplists:get_value(severity, OldAlarm),
        case (Severity == 0) and (OldSeverity == 0) of
        true -> 
            {ok, ignore};
        false -> 
            do_update(Alarm, OldAlarm),
            if
            Severity == 0 -> 
                {ok, cleared};
            OldSeverity == 0 ->
                {ok, recured};
            true ->
                {ok, supressed}
            end
        end;
    false ->
        if
        Severity == 0 ->
            {ok, ignore};
        true ->
            try_insert(Alarm),
            {ok, inserted}
        end
	end,
    ShouldNotify =
    case SavedRes of
    {ok, cleared} -> true;
    {ok, recured} -> true;
    {ok, inserted} -> true;
	{ok, supressed} -> true;
    _ -> false
    end,
    case ( ShouldNotify ) of
    true ->
        todo;
        %evabus_forward:send(Alarm);
    false -> 
        ok
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
select(AlarmKey, AlarmSource) ->
	Where = {'and', {alarm_key, AlarmKey}, {source, AlarmSource}},
	select(Where).

select(Where) ->
    case epgsql:select(main, alarms, Where) of
    {ok, [Alarm|_]} -> {ok, Alarm};
    {ok, []} -> false
    end.

try_insert(#alarm{alarm_name = snmp_status, source = Source} = Alarm) ->
    case select("ping_status", Source) of
    {ok, _PingAlarm} -> ignore;
    false -> do_insert(Alarm)
    end;

try_insert(Alarm) ->
    do_insert(Alarm).

do_insert(Alarm) ->
	CreatedAt = UpdatedAt = {datetime, calendar:local_time()},
	Record = [{created_at, CreatedAt}, {updated_at, UpdatedAt} | evabus_alarm:record(Alarm)],
    case epgsql:insert(main, alarms, Record) of
    {error, Reason} -> 
        ?ERROR("failed to insert alarm: ~p, ~n~p", [Reason, Alarm]);
    _ ->
        ok
    end.

do_update(#alarm{class_id = ClassId,
                 alarm_name = AlarmName,
				 alarm_alias = AlarmAlias,
				 node_id = NodeId,
				 source = Source,
				 occur_count = Count,
				 probable_cause = ProbableCause,
				 severity = Severity,
				 summary = Summary,
				 last_occurrence = LastOccurrence,
				 cleared_time = ClearTime
				 } = _Alarm, OldRecord) ->

	Id = proplists:get_value(id, OldRecord),

	UpdatedAt = {datetime, calendar:local_time()},

	OldCount = get_value(occur_count, OldRecord),
	NewCount = 
	if 
	Severity > 0 -> Count + OldCount;
	true -> OldCount
	end,

	Record = [{class_id, ClassId},
			  {alarm_alias, AlarmAlias},
			  {node_id, NodeId},
			  {source, Source},
			  {occur_count, NewCount},
			  {severity, Severity},
			  {summary, Summary},
			  {last_occurrence, LastOccurrence},
			  {probable_cause, ProbableCause},
			  {updated_at, UpdatedAt}],

	Record1 = 
	if
	Severity == 0 ->
		[{clear_type, 0}, {clear_time, ClearTime}|Record];
	true ->
		Record
	end,

	case epgsql:update(main, alarms, Record1, {id, Id}) of
    {error, Reason} ->
        ?ERROR("failed to update: ~p ~n~p ~n~p", [AlarmName, Reason, Record1]);
    _ ->
        ok
    end.

