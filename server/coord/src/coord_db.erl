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
    %mnesia:create_table(meas_type, [{ram_copies, [node()]}, 
    %    {index, [name]}, {attributes, record_info(fields, meas_type)}]),
    %mnesia:add_table_copy(meas_type, node(), ram_copies),
    %handle_info(load_meas_types, state),
    Tabs = load_from_db(),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
	?INFO_MSG("coord_db is started...[ok]"),
    {ok, #state{tabs = Tabs, channel=Channel}}.

open(Conn) ->
	{ok, Channel} = amqp:open_channel(Conn),
	%amqp:queue(Channel,<<"rpc.db">>),
	%amqp:consume(Channel, <<"rpc.db">>),
	Res = amqp:topic(Channel, <<"oss.db">>),
	?INFO("~p", [Res]),
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
		?INFO("~p publish ~p to oss.db", [Channel, Key]),
		amqp:publish(Channel, <<"oss.db">>, Payload, Key)
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
    ?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?ERROR("badmsg: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%handle_info(load_meas_types, State) ->
%    {ok, Records} = emysql:select({monet_meas_types, {is_valid, 1}}),
%	lists:foreach(fun(Record) -> 
%		{value, Id} = dataset:get_value(id, Record),
%		{value, Name} = dataset:get_value(name, Record),
%		{value, EventName} = dataset:get_value(event_name, Record),
%		{value, ObjectClass} = dataset:get_value(object_class, Record),
%		{value, ThreshMajor} = dataset:get_value(threshold_major, Record),
%		{value, MajorMsg} = dataset:get_value(major_message, Record),
%		{value, ThreshWarning} = dataset:get_value(threshold_warning, Record),
%		{value, WarningMsg} = dataset:get_value(warning_message, Record),
%		mnesia:dirty_write(#meas_type{id = Id,
%            name = Name, 
%            event_name = EventName,
%            object_class=ObjectClass, 
%            threshold_major=parse_thresh(ThreshMajor), 
%            major_message=MajorMsg, 
%            threshold_warning = parse_thresh(ThreshWarning), 
%            warning_message = WarningMsg})
%	end, Records),
%    erlang:send_after(300*1000, self(), load_meas_types),
%    {noreply, State};

handle_info({amqp, disconnected}, State) ->
	?ERROR_MSG("amqp disconnected..."),
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn},State) ->
	{noreply, State#state{channel = open(Conn)}};


handle_info({deliver, <<"rpc.db">>, Props, Payload}, 
	#state{channel = Channel} = State) ->

    Reply = coord_db:dbquery(binary_to_term(Payload)),
    ReplyTo = proplists:get_value(reply_to, Props),
    ReqId = proplists:get_value(correlation_id, Props),
    amqp:reply(Channel, ReplyTo, ReqId, Reply),
	{noreply, State};

handle_info(Info, State) ->
    ?ERROR("badinfo: ~p", [Info]),
    {stop, {error, {badinfo, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

load_from_db() ->
    {ok, Metrics} = emysql:select(metrics),
    {ok, MibOids} = emysql:select({mib_oids, [mib, grp, name, oid], {is_valid, 1}}),
    {ok, DiscoMods} = emysql:select(disco_mods),
    {ok, MonitorMods} = emysql:select(monitor_mods),
    [{metrics, Metrics}, {mib_oids, MibOids}, 
    {disco_mods, DiscoMods}, {monitor_mods, MonitorMods}].

parse_thresh(Threshold) ->
    {ok, Exp} = prefix_exp:parse(Threshold),
    Exp.

