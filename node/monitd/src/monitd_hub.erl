%%%----------------------------------------------------------------------
%%% File    : monitd_hub.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : monitd data hub
%%% Created : 28 Dec. 2010
%%% Updated : 06 May. 2012
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(monitd_hub).

-author('ery.lee@gmail.com').

-import(proplists, [get_value/2]).

-include("event.hrl").

-include("metric.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/0,
		setup/1,
		emit/1]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).

%metric buffer to reduce amqp messages per seconds.
-record(state, {buffer_size, buffer, aging_time, errdb, channel}).

start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

setup({metrics, Metrics}) ->
	gen_server2:cast(?MODULE, {setup, metrics, Metrics}).

channel() ->
	gen_server2:call(?MODULE, channel).

emit(DataList) when is_list(DataList) ->
	Chan = channel(),
	[emit(Chan, element(1, E), E) || E <- DataList].

emit(_Chan, metric, Metric) ->
	journal(Metric),
	case catch diff(Metric) of
	{ok, Metric1} ->
		buffer(Metric1);
	{error, nolast} ->
		ignore;
	{error, empty} ->
		#metric{name=Grp, from=Ip, dn=Dn, timestamp=Ts} = Metric,
		?WARNING("empty metric: ~s:~s:~p@~s", [Dn, Grp, Ts, Ip]);
	{error, Error} ->
		?ERROR("diff error: ~p", [Error]),
		?ERROR("~p", [Metric]);
	{'EXIT', Reason} ->
		?ERROR("diff exit: ~p", [Reason]),
        ?ERROR("~p", [erlang:get_stacktrace()]),
		?ERROR("~p", [Metric])
	end;

emit(Chan, event, #event{name = Name} = Event) ->
	Payload = term_to_binary(Event),
	RoutingKey = "event."++atom_to_list(Name),
	amqp:publish(Chan, <<"oss.event">>, Payload, RoutingKey);

emit(Chan, Queue, Tup) ->
	amqp:send(Chan, Queue, term_to_binary(Tup, [compressed])).

journal(Metric) when is_record(Metric, metric) ->
	monitd_journal:write(Metric).

buffer(Metric) when is_tuple(Metric) ->
	gen_server2:cast(?MODULE, {buffer, Metric}).
	
lookup_type(Grp, Name) ->
	case ets:lookup(metric_type, {Grp, Name}) of
	[{_, Calc}] -> Calc;
	[] -> ?ERROR("metric_type undefined: {~p,~p}", [Grp, Name]), undefined
	end.
	
lookup_last({Ip, Dn, Grp, Name}) when is_binary(Dn)
	and is_atom(Grp) and is_atom(Name) ->
	case ets:lookup(metric_last, {Ip, Dn, Grp, Name}) of
	[{_, Ts, Val}] -> {Ts, Val};
	[] -> nolast
	end.

store_last({Ip, Dn, Grp, Ts, Lasts}) ->
	gen_server2:cast(?MODULE, {store_last, Ip, Dn, Grp, Ts, Lasts}).

init([]) ->
	random:seed(now()),
	Size =
	case application:get_env(buffer) of
	{ok, Val} -> Val;
	undefined -> 10
	end,
	Aging = 
	case application:get_env(aging) of
	{ok, Val2} -> Val2 * 3600;
	undefined -> 4 * 3600
	end,
	BufferSize = Size + random:uniform(Size),
    ets:new(metric_type, [set, protected, named_table]),
	ets:new(metric_last, [set, protected, named_table]),
	ets:new(metric_aging, [set, protected, named_table]),
	{ok, Conn} = connect(amqp),
	{ok, Errdb} = connect(errdb),
	erlang:send_after(3000, self(), buffer_commit),
    ?INFO("monitd_hub buffer size: ~p", [BufferSize]),
    ?INFO_MSG("monitd_hub is starting...[ok]"),
    {ok, #state{buffer_size = BufferSize, 
				buffer = [],
				aging_time=Aging,
				errdb = Errdb, 
				channel = open(Conn)}}.

connect(amqp) ->
	amqp:connect();

connect(errdb) ->
    case application:get_env(errdb) of
	{ok, Opts} -> errdb_client:start_link(Opts);
	undefined -> {ok, undefined}
	end.

open(Conn) ->
    {ok, Chan} = amqp:open_channel(Conn),
	Chan.

handle_call(channel, _From, #state{channel = Chan} = State) ->
	{reply, Chan, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast({setup, metrics, Records}, State) ->
    ?INFO("setup metrics: ~p.", [length(Records)]),
	Store = fun(Record) ->
		Grp = get_value(grp, Record),
		Name = get_value(name, Record),
		Calc = get_value(calc, Record), 
		Key = {atom(Grp), atom(Name)},
		ets:insert(metric_type, {Key, atom(Calc)})
	end,
	lists:foreach(Store, Records),
    {noreply, State};

handle_cast({store_last, Ip, Dn, Grp, Ts, Lasts}, #state{aging_time=Aging} = State) ->
	%store data
	Records =
	lists:map(fun({Name, Val}) ->
		Key = {Ip, Dn, Grp, Name}, {Key, Ts, Val}
	end, Lasts),
	ets:insert(metric_last, Records),
	%store aged timer
	TimerKey = {Ip, Dn, Grp},
	Names = [N || {N, _} <- Lasts],
	case ets:lookup(metric_aging, TimerKey) of
	[{_, Timer}] ->
		NewTimer = reset_timer(Timer, Aging, {TimerKey, Names}),
		ets:insert(metric_aging, {TimerKey, NewTimer});
	[] -> 
		Timer = reset_timer(undefined, Aging, {TimerKey, Names}),
		ets:insert(metric_aging, {TimerKey, Timer})
	end,
	{noreply, State};

handle_cast({buffer, Metric}, #state{buffer = Buffer,
	buffer_size = BufferSize} = State) ->
	case length(Buffer) >= BufferSize of
	true ->
		%TODO: check threshold
		emit_metrics(State, lists:reverse([Metric|Buffer])),
		{noreply, State#state{buffer = []}};
	false ->
		{noreply, State#state{buffer = [Metric|Buffer]}}
	end;

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(buffer_commit, #state{buffer = Buffer} = State) ->
	emit_metrics(State, lists:reverse(Buffer)),
	erlang:send_after(2000, self(), buffer_commit),
	{noreply, State#state{buffer = []}};

handle_info({amqp, disconnected}, State) ->
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn}, State) ->
	{noreply, State#state{channel = open(Conn)}};

handle_info({aging, {Ip, Dn, Grp}=TimerKey, Names}, State) ->
	?INFO("metric aging: ~p", [TimerKey]),
	[ets:delete(metric_last, {Ip, Dn, Grp, Name}) || Name <- Names],
	{noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

diff(#metric{name=Grp, from=Ip, dn=Dn, timestamp=Ts, data=Data} = Metric) 
	when is_atom(Grp) and is_binary(Dn) ->
	Diff = fun(Name, Val) -> 
		case lookup_last({Ip, Dn, Grp, Name}) of
		{LastTs, LastVal} ->
			{Name, calc(Name, LastTs, LastVal, Ts, Val)};
		nolast ->
			nolast
		end
	end,
	Fun = fun({Name, Val}, {Acc, LastAcc}) -> 
		Calc = lookup_type(Grp, Name),
		case Calc of
		counter ->
			{[Diff(Name, Val)|Acc], [{Name, Val}|LastAcc]};
		gauge ->
			{[{Name, Val}|Acc], LastAcc}
		end
	end,
	{NewData, Lasts} = lists:foldl(Fun, {[], []}, Data),
	if
	length(Lasts) == 0 -> ok;
	true -> store_last({Ip, Dn, Grp, Ts, Lasts})
	end,
	HasNoLast = lists:member(nolast, NewData),
	if
	length(NewData) == 0 -> {error, empty};
	HasNoLast -> {error, nolast};
	true -> {ok, Metric#metric{data=NewData}}
	end.

calc(Name, LastTs, _LastVal, Ts, _Val) when Ts =< LastTs ->
	throw({error, {interval_less_zero, Name}});
%FIX zero metric
%calc(Name, _LastTs, LastVal, _Ts, Val) when (Val == 0) and (LastVal > 0) ->
%	throw({error, {bad_zero_counter, Name, LastVal, Val}});
calc(_Name, LastTs, LastVal, Ts, Val) ->
	derive(v(Val) - v(LastVal)) / (Ts - LastTs).

derive(V) when V < 0 ->
    0;
derive(V) ->
    V.

v(I) when is_integer(I) ->
    I;
v(F) when is_float(F) ->
	F.

atom(B) when is_binary(B) ->
	list_to_atom(binary_to_list(B)).

emit_metrics(_State, []) ->
	ignore;

emit_metrics(#state{errdb = undefined, channel = Chan}, Metrics) ->
	Payload = term_to_binary(Metrics, [compressed]),
	amqp:send(Chan, <<"metric">>, Payload);

emit_metrics(#state{errdb = Errdb}, Metrics) ->
	lists:foreach(fun(#metric{name=Name, dn=Dn, timestamp=Ts, data=Data}) -> 
		Key = iolist_to_binary([Dn, ":", atom_to_list(Name)]),
		errdb_client:insert(Errdb, Key, Ts, Data)
	end, Metrics).

reset_timer(OldTimer, Aging, {TimerKey, Names}) ->
	cancel_timer(OldTimer),
	erlang:send_after(Aging*1000, self(), {aging, TimerKey, Names}).
	
cancel_timer(undefined) -> ok;

cancel_timer(Ref) -> (catch erlang:cancel_timer(Ref)).

