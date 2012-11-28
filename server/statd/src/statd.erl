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

handle_status({status, onus, OltDn, []}) ->
	?WARNING("empty onus status: ~s", [OltDn]);

handle_status({status, onus, OltDn, Onus}) ->
    ?INFO("status of onus: ~s, ~p", [OltDn, length(Onus)]);

handle_status(Status) ->
	?ERROR("bad status:~p", [Status]).

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

