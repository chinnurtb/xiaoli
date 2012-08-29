%%%----------------------------------------------------------------------
%%% File    : statd.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Status daemon.
%%% Created : 08 May. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(statd).

-author('ery.lee@gmail.com').

-include("event.hrl").

-include_lib("elog/include/elog.hrl").

-import(statd_hub, [emit/1]).

-import(proplists, [get_value/2, get_value/3]).

-export([start_link/0, stats/0]).

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

stats() ->
	gen_server2:call(?MODULE, stats).

init([]) ->
	put(received, 0),
	{ok, Conn} = amqp:connect(),
    Chan = open(Conn),
	?INFO("~p is started.", [?MODULE]),
    {ok, #state{channel = Chan}}.

open(Conn) ->
	{ok, Chan} = amqp:open_channel(Conn),
	amqp:queue(Chan, <<"status">>),
	amqp:consume(Chan, <<"status">>),
	Chan.

handle_call(stats, _From, State) ->
	{reply, [{received, get(received)}], State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({deliver, <<"status">>, _Prop, Payload}, State) ->
	put(received, get(received) + 1),
	Work = fun() -> 
		try 
			handle_status(binary_to_term(Payload))
		catch
			_:Err -> ?ERROR("~p", [Err])
		end
	end,
	worker_pool:submit_async(Work),
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

handle_status({status, fitaps, AcDn, []}) ->
	?WARNING("empty fitaps status: ~s", [AcDn]);

handle_status({status, fitaps, AcDn, Aps}) ->
	{ok, [Ac]} = find_ac(AcDn),
	AcId = get_value(id, Ac),
	?INFO("~p fitaps status from ~s", [length(Aps), AcDn]),
	%apids found on ac
	ApIds = [ApId || {ApId, _} <- Aps],

	%apids found in db
	ApsInDb = aps_in_db(AcId, ApIds),
	ApIdsInDb = [ApId || {ApId, _} <- ApsInDb],

	{OfflineIds, UpdateIds, _} = compare(ApIdsInDb, ApIds),
    %?INFO("offlineIds: ~n~p", [OfflineIds]),
	[offline(Ac, ApId, get_value(ApId, ApsInDb)) || ApId <- OfflineIds],
	[changed(Ac, ApId, get_value(ApId, ApsInDb), get_value(ApId, Aps)) || ApId <- UpdateIds];

handle_status(Status) ->
	?ERROR("bad status:~p", [Status]).

find_ac(AcDn) ->
	emysql:select(mit_acs, [id, ac_dn, ip], {ac_dn , AcDn}).

find_aps(Where) ->
	Fields = [id, ac_id, ap_dn, ap_cn, ap_en, mac, managed_state, extra],
	emysql:select(mit_aps, Fields, Where).

aps_in_db(AcId, ApIds) ->
    {ok, ApsOfAc} = find_aps({ac_id, AcId}),
    ApIdsOfAc = [get_value(ap_en, Ap) || Ap <- ApsOfAc],
	OtherApIds = subtract(ApIds, ApIdsOfAc),
	{ok, ApsOfOtherAc} = 
	case OtherApIds of
	[] -> {ok, []};
	_ -> find_aps({'in', ap_en, OtherApIds})
	end,
    [{get_value(ap_en, Ap), Ap} || Ap <- ApsOfOtherAc ++ ApsOfAc].

offline(Ac, ApId, OldAp) ->
	AcIp = get_value(ip, Ac),
	Summary = summary(offline, AcIp, OldAp),
    ?INFO("offline ~s @ ~s", [ApId, AcIp]), 
	emit(#event{name = fitap_offline,
				sender = {ip, AcIp},
				severity = major,
				source = {fitap, ApId},
				summary = Summary,
				timestamp = extbif:timestamp(),
				manager = node(),
				from = statd}).

changed(Ac, ApId, OldAp, Ap) ->
	AcIp = get_value(ip, Ac),
	AcId = get_value(id, Ac),
	OldAcId = get_value(ac_id, OldAp),

	%compare
	Changed = [{ac_id, AcId}|Ap] -- OldAp,
	ChangedState = get_value(managed_state, Changed),
	ChangedExtra = get_value(extra, Changed),
	ChangedAcId = get_value(ac_id, Changed),

	case ChangedState of
	1 -> %change to online
		update(ApId, {extra, ChangedExtra}),
		Summary = summary(online, AcIp, OldAp),
		emit(#event{name = fitap_online,
					sender = {ip, AcIp},
					source = {fitap, ApId},
					severity = clear,
					summary = Summary, 
					timestamp = extbif:timestamp(),
				    manager = node(),
				    from = statd});
	0 -> %change to offline
		if
		OldAcId == AcId -> %AC无变化, 产生告警
			Summary = summary(offline, AcIp, OldAp),
			emit(#event{name = fitap_offline,
						sender = {ip, AcIp},
						source = {fitap, ApId},
						severity = major,
						summary = Summary, 
						timestamp = extbif:timestamp(),
						manager = node(),
						from = statd});
		true ->
			ignore
		end;
	undefined -> %no change
		NewState = get_value(managed_state, Ap),
		if 
		NewState == 1 -> %always online
			update(ApId, {ac_id, ChangedAcId}),
			update(ApId, {extra, ChangedExtra});
		true ->
			ignore
		end
	end.

update(_ApId, {_Attr, undefined}) ->
	ignore;

update(ApId, {Attr, Val}) ->
	emysql:update(mit_aps, [{Attr, Val}], {ap_en, ApId}).

summary(online, AcIp, Ap) ->
	summary("上线", AcIp, Ap);
summary(offline, AcIp, Ap) ->
	summary("下线", AcIp, Ap);
summary(State, AcIp, Ap) ->
	iolist_to_binary(["轮询比对发现AP从AC '", AcIp, "'", State,
	 "，AP标识: ", get_value(ap_en, Ap),
	 ", AP名称: ", get_value(ap_cn, Ap),
	 ", AP MAC: ", get_value(mac, Ap, <<>>)]).

subtract(LeftList, RightList) ->
	LeftSet = ordsets:from_list(LeftList),
	RightSet = ordsets:from_list(RightList),
	ordsets:subtract(LeftSet, RightSet).

compare(LeftList, RightList) ->
	LeftSet = ordsets:from_list(LeftList),
	RightSet = ordsets:from_list(RightList),
	Inter = ordsets:intersection(LeftSet, RightSet),
	Left = ordsets:subtract(LeftSet, Inter),
	Right = ordsets:subtract(RightSet, Inter),
	{Left, Inter, Right}.

