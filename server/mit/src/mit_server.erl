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
	amqp:queue(Channel, <<"entry">>),
	amqp:consume(Channel, <<"entry">>),
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
handle_info({deliver, <<"entry">>, _Props, Payload}, State) ->
	Entry = binary_to_term(Payload),
	%handle_entry(Entry),
	Fun = fun() ->
		try handle_entry(Entry) catch
		_:Err ->
			?ERROR("badentry error: ~p", [Err]),
			?ERROR("~p", [Entry])
		end
	end,
	worker_pool:submit_async(Fun),
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

handle_entry({entry, omc, OmcDn, Attrs}) ->
	mit_omc:update(OmcDn, Attrs);

handle_entry({entry, aclist, OmcDn, AcInfos}) ->
	mit_omc:upate(aclist, OmcDn, AcInfos);

handle_entry({entry, ac, AcDn, Attrs}) ->
	mit_ac:update(AcDn, Attrs);

handle_entry({entry, sw, SwDn, Attrs}) ->
	mit_sw:update(SwDn, Attrs);

handle_entry({entry, omcintfs, Dn, Intfs}) ->
	mit_omc:update(omcintfs, Dn, Intfs);

handle_entry({entry, intfs, Dn, []}) ->
	?WARNING("empty intfs from ~s", [Dn]);
	
handle_entry({entry, intfs, Dn, Intfs}) ->
	mit_intf:update(Dn, Intfs);

handle_entry({entry, fatap, ApDn, Attrs}) -> 
	case mit:lookup(ApDn) of
	{ok, _} ->
		Attrs1 = mit_dict:transform(ap, Attrs),
		mit_ap:update(fatap, ApDn, Attrs1);
	{false, _}  ->
		?ERROR("failed to lookup ap: ~s", [ApDn])
	end;

handle_entry({entry, fitap, ApDn, Attrs}) ->
	case mit:lookup(ApDn) of
	{ok, _} ->
		Attrs1 = mit_dict:transform(ap, Attrs),
		mit_ap:update(fitap, ApDn, Attrs1);
	{false, _} ->
		?ERROR("failed to lookup ap ~s", [ApDn])
	end;

handle_entry({entry, fitaps, AcDn, FitAps}) ->
	mit_ap:update(fitaps, AcDn, FitAps);

handle_entry({entry, ssids, ApDn, []}) ->
	?WARNING("empty ssids: ~s", [ApDn]);

handle_entry({entry, ssids, ApDn, Ssids}) ->
	mit_ssid:update(ApDn, Ssids);

handle_entry({entry, radios, ApDn, []}) ->
	?WARNING("empty radios form ~s", [ApDn]);

handle_entry({entry, radios, ApDn, Radios}) ->
	mit_radio:update(ApDn, Radios);

%for wlan opti
handle_entry({entry, apssid, _AcDn, Entry}) ->
	emysql:insert(monaps, Entry);

%for wlan opti
handle_entry({entry, ssidinfo, _AcDn, Entry}) ->
	emysql:insert(monapinfos, Entry);

handle_entry(Item) ->
	?ERROR("unexpected item: ~p", [Item]).
