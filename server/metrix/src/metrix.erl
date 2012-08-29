%%%----------------------------------------------------------------------
%%% File    : metrix.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : metrix process
%%% Created : 18 Feb 2008
%%% Updated : 13 Sep 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(metrix).

-author('ery.lee@gmail.com').

-include("metric.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/1, name/1]).

-behavior(gen_server).

%%callback
-export([init/1, 
        handle_call/3, 
        prioritise_call/3,
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).

%amqp channel
-record(state, {channel, store}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Id) ->
    gen_server2:start_link({local, name(Id)}, ?MODULE, [Id], []).

name(Id) ->
	list_to_atom("metrix_" ++ integer_to_list(Id)).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Id]) ->
	put(metrix_deliver, 0),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
	{ok, Pid} = metrix_store:start_link(Id),
	?INFO("~p is started.", [name(Id)]),
    {ok, #state{channel = Channel, store = Pid}}.

open(C) ->
	{ok, Channel} = amqp:open_channel(C),
	amqp:queue(Channel, <<"metric">>),
	amqp:consume(Channel, <<"metric">>),
	Channel.

handle_call(status, _From, State) ->
	{reply, {ok, get()}, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

prioritise_call(status, _From, _) -> 10;

prioritise_call(_, _From, _) -> 0.

handle_cast({emit, Event}, #state{channel = Channel} = State) ->
    amqp:send(Channel, element(1, Event), term_to_binary(Event)),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({deliver, <<"metric">>, _Props, Payload}, #state{store=Pid} = State) ->
	Counter = get(metrix_deliver),
	put(metrix_deliver, Counter+1),
	case binary_to_term(Payload) of
	Metric when is_record(Metric, metric) ->
		metrix_store:insert(Pid, Metric);
	List when is_list(List) ->
		[metrix_store:insert(Pid, Metric) || Metric <- List];
	Item ->
		?ERROR("bad_metric_item: ~p", [Item])
	end,
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
