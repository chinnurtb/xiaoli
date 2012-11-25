%%%----------------------------------------------------------------------
%%% File    : monitd.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : monitor daemon.
%%% Created : 27 Dec 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(monitd).

-import(proplists, [get_value/2,
                    get_value/3]).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/1]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        prioritise_info/2,
        terminate/2, 
        code_change/3]).

-record(state, {shards, queue, channel}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Env) ->
    gen_server2:start_link({global, ?MODULE}, ?MODULE, [Env], []).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Env]) ->
    chash_pg:create(monitd_coord),
    Cities = string:tokens(get_value(shards, Env, "#"), ","),
    Shards = ["shard." ++ C || C <- Cities],
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn, Shards),
	Queue = atom_to_binary(node()),
    erlang:send_after(300 * 1000, self(), check_host),
    ?INFO_MSG("monitd is started...[ok]"),
    {ok, #state{shards = Shards, queue = Queue, channel = Channel}}.

open(Conn, Shards) ->
	{ok, Ch} = amqp:open_channel(Conn),
	{ok, NodeQ} = amqp:queue(Ch, node()),

    %db topic 
	amqp:fanout(Ch, <<"sys.db">>),
	amqp:bind(Ch, <<"sys.db">>, NodeQ, <<"#">>),

    %watch topic
	amqp:topic(Ch, <<"sys.watch">>),
	amqp:bind(Ch, <<"sys.watch">>, NodeQ, <<"ping">>),
	Presence = {node(), node, available, extlib:appvsn(), <<"startup">>},
	amqp:publish(Ch, <<"sys.watch">>, term_to_binary(Presence), <<"presence">>),
    
    %shard topic and queues
    amqp:topic(Ch, <<"sys.shard">>),
    lists:foreach(fun(Shard) -> 
        {ok, Q} = amqp:queue(Ch, Shard),
        amqp:bind(Ch, <<"sys.shard">>, Q, Shard),
        amqp:consume(Ch, Q)
    end, Shards),

    %shard join
    JoinMsg = {join, node(), Shards},
    amqp:publish(Ch, <<"sys.shard">>, term_to_binary(JoinMsg), "join.shard"),

	erlang:send_after(10000, self(), heartbeat),

	amqp:consume(Ch, NodeQ),

	Ch.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}, 
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({deliver, <<"shard.", _/binary>>, _Props, Payload}, State) ->
    handle_task(binary_to_term(Payload), State),
    {noreply, State};
    
handle_info({deliver, Queue, _Props, Payload}, #state{queue = Queue} = State) ->
    handle_task(binary_to_term(Payload), State),
    {noreply, State};

handle_info({deliver, <<"task">>, _, Payload}, #state{channel = Channel} = State) ->
    case binary_to_term(Payload) of
    {discover, Dn, Entry} ->
		unicast(Dn, {discover, Dn, Entry}),
        Reply = term_to_binary({discovered, Dn, node()}),
        amqp:send(Channel, <<"task.reply">>, Reply);
	{rediscover, Dn, Entry} -> %%disco by local server.
		disco_server:discover(Dn, Entry),
		Reply = term_to_binary({discovering, Dn, node()}),
        amqp:send(Channel, <<"task.reply">>, Reply);
    {disco, update, Dn, Entry} ->
		unicast(Dn, {disco, update, Dn, Entry});
    {undiscover, Dn} ->
		unicast(Dn, {undiscover, Dn});
    {auto_discover, Dn, Task} ->
		disco_server:auto_discover(Dn, Task);
    {reset, Dn, Entry} ->
		%FIXME: how to handle reset?
		unicast(Dn, {reset, Dn, Entry});
    {async_task, Id, AsyncTask} ->
        ?INFO("async_task: ~p, ~p", [Id, AsyncTask]);
    Term ->
        ?ERROR("unknown payload: ~p", [Term])
    end,
	{noreply, State};

handle_info({deliver, <<"ping">>, _, _}, #state{shards = Shards, channel = Ch} = State) ->
    Presence = {node(), node, available, extlib:appvsn(), <<"alive">>},
    amqp:publish(Ch, <<"sys.watch">>, term_to_binary(Presence), <<"presence">>),
    amqp:publish(Ch, <<"sys.shard">>, term_to_binary({join, node(), Shards}), "join.shard"),
    {noreply, State};

handle_info({deliver, <<"table.", Tab/binary>>, _, Payload}, State) ->
	?INFO("dbtab: ~s, payload: ~p", [Tab, size(Payload)]),
	{Table, Records} = binary_to_term(Payload),
    case Table of
	miboids ->
	   mib_registry:setup({miboids, Records});
    metrics ->
	   monitd_hub:setup({metrics, Records});
	sysoids ->
	   monitd_disco:setup({sysoids, Records});
    _ ->
        ignore
    end,
    broadcast(Table, Records),
    {noreply, State};

handle_info({amqp, disconnected}, State) ->
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn}, #state{shards = Shards} = State) ->
	{noreply, State#state{channel = open(Conn, Shards)}};

handle_info(heartbeat, #state{channel = Channel} = State) ->
	Hearbeat = {node(), <<"monitd is alive.">>, []},
    amqp:send(Channel, <<"heartbeat">>, term_to_binary(Hearbeat)),
    erlang:send_after(10000, self(), heartbeat),
    {noreply, State};

handle_info(check_host, #state{channel = Channel} = State) ->
	case agent:host_info() of
	{ok, HostInfo, Metrics} ->
		monitd_hub:emit(Metrics),
		Payload = term_to_binary({host, HostInfo}, [compressed]),
        amqp:send(Channel, <<"host">>, Payload);
	{error, Error} ->
		?ERROR("~p", [Error])
	end,
	erlang:send_after(300*1000, self(), check_host),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

prioritise_info({deliver, <<"ping">>, _, _}, _State) ->
    10;

prioritise_info(heartbeat, _State) ->
    10;

prioritise_info({deliver, <<"db.", _Tab/binary>>, _, _}, _State) ->
    9;

prioritise_info(_, _State) ->
    0.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_task({node, Coord, {monitor, Node}}, #state{channel=Ch}) ->
    ?INFO("monitor ~s from ~s", [Node#node.dn, Coord]),
    unicast(Node#node.dn, {monitor, Node}),
    Reply = {node, node(), {monitored, Node#node.dn}},
    amqp:send(Ch, Coord, term_to_binary(Reply));

handle_task({node, Coord, {update, Node}}, _State) ->
    ?INFO("update ~s from ~s", [Node#node.dn, Coord]),
    unicast(Node#node.dn, {update, Node});

handle_task({node, Coord, {delete, Dn}}, _State) ->
    ?INFO("delete ~s from ~s", [Dn, Coord]),
    unicast(Dn, {unmonitor, Dn});

%TODO: spawn process
handle_task({task, Coord, {ping, Id, Host}}, #state{channel=Ch}) ->
    Output = check_ping:cmd(Host),
    Reply = term_to_binary({'ping.reply', Id, Output}),
    amqp:send(Ch, Coord, Reply);

%TODO: spawn process
handle_task({task, Coord, {snmp, Id, Args}}, #state{channel=Ch}) ->
    [Host, Community] = binary:split(Args, [<<" ">>], [global]),
    {Status, Summary} = check_snmp:run(Host, Community),
    Output = list_to_binary([Status, "\n", Summary]),
    Reply = term_to_binary({'snmp.reply', Id, Output}),
    amqp:send(Ch, Coord, Reply);

handle_task(Task, _State) ->
    ?ERROR("unexpected task: ~p", [Task]).

unicast(Key, Task) ->
	chash_pg:get_pid(monitd_coord, Key) ! Task.

broadcast(Table, Records) ->
    Pids = chash_pg:get_pids(monitd_coord),
    [Pid ! {Table, Records} || Pid <- Pids].

atom_to_binary(A) ->
	list_to_binary(atom_to_list(A)).

