%%%----------------------------------------------------------------------
%%% File    : coord_db.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Create some mnesia db.
%%% Created : 19 Mar. 2011
%%% License : http://www.opengoss.com
%%% 
%%% Copyright (C) 2011, www.opengoss.com
%%%----------------------------------------------------------------------
-module(coord_db).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-include("coord.hrl").

-behavior(gen_server).

-export([start_link/0,
        reload/0,
		syncdb/0,
        dbquery/1]).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {tabs, channel}).

-define(TABLES, [
    sysoids,
    metrics,
    {miboids, {is_valid, 1}},
    monitors,
    modules,
    timeperiods
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

syncdb() ->
	gen_server:call(?MODULE, syncdb).

dbquery(Tab) ->
    gen_server:call(?MODULE, {dbquery, Tab}).

reload() ->
    gen_server:call(?MODULE, reload, 30000).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Tabs = load_from_db(),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
	?INFO_MSG("coord_db is started...[ok]"),
    {ok, #state{tabs = Tabs, channel=Channel}}.

open(Conn) ->
	{ok, Channel} = amqp:open_channel(Conn),
	amqp:topic(Channel, <<"sys.db">>),
	Channel.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(syncdb, _From, #state{tabs = Tabs, channel = Channel} = State) ->
	Publish = fun(Tab, Records) -> 
		Key = list_to_binary(["db.", atom_to_list(Tab)]),
		Payload = term_to_binary(Records, [compressed]),
		?INFO("~p publish ~p to sys.db", [Channel, Key]),
		amqp:publish(Channel, <<"sys.db">>, Payload, Key)
	end,
	[Publish(Tab, Records) || {Tab, Records} <- Tabs],
    {reply, ok, State};

handle_call(reload, _From, State) ->
    Tabs = load_from_db(),
    {reply, ok, State#state{tabs = Tabs}};
    
handle_call({dbquery, Tab}, _From, #state{tabs = Tabs} = State) ->
    Records = proplists:get_value(Tab, Tabs),
    {reply, {ok, Records}, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({amqp, disconnected}, State) ->
	?ERROR_MSG("amqp disconnected..."),
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn},State) ->
	{noreply, State#state{channel = open(Conn)}};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

load_from_db() ->
    lists:map(fun(Table) -> 
        case Table of
        {Tab, Where} -> 
            {ok, Records} = epgsql:select(main, Tab, Where),
            {Tab, Records};
        Tab when is_atom(Tab) ->
            {ok, Records} = epgsql:select(main, Tab),
            {Tab, Records}
        end
    end, ?TABLES).

