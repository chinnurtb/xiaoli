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

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-export([start_link/1,
		task_reply/1]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        prioritise_info/2,
        terminate/2, 
        code_change/3 ]).

-record(state, {shard, queue, channel, monitor_types}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Env) ->
    gen_server2:start_link({global, ?MODULE}, ?MODULE, [Env], []).

task_reply(Reply) ->
	gen_server2:cast({global, ?MODULE}, {task_reply, Reply}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Env]) ->
    chash_pg:create(monitd_coord),
    Shard = proplists:get_value(shard, Env),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn, Shard),
	Queue = atom_to_binary(node()),
    erlang:send_after(300 * 1000, self(), check_host),
    erlang:system_monitor(self(), [{long_gc, 500},
		{large_heap, 10000000}, busy_port]),
    ?INFO_MSG("monitd is started...[ok]"),
    {ok, #state{shard = Shard, queue = Queue, channel = Channel}}.

open(Conn, Shard) ->
	{ok, Channel} = amqp:open_channel(Conn),
	amqp:topic(Channel, <<"oss.db">>),
	amqp:topic(Channel, <<"sys.watch">>),
	%declare and bind queues
	amqp:queue(Channel, <<"task">>),
	amqp:consume(Channel, <<"task">>),
	{ok, Q} = amqp:queue(Channel, node()),
	amqp:bind(Channel, <<"sys.watch">>, Q, <<"ping">>),
	amqp:bind(Channel, <<"oss.db">>, Q, <<"db.*">>),
	amqp:consume(Channel, Q),

	%send presence
	Presence = {node(), node, available, extlib:appvsn(), <<"startup">>},
	amqp:send(Channel, <<"presence">>, term_to_binary(Presence)),
	amqp:send(Channel, <<"shard">>, term_to_binary({node(), Shard})),
	erlang:send_after(10000, self(), heartbeat),
	Channel.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast({task_reply, Reply}, #state{channel = Channel} = State) ->
    amqp:send(Channel, <<"task.reply">>, Reply),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}, 
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({deliver, Queue, Props, Payload}, #state{queue = Queue} = State) ->
	handle_info({deliver, <<"task">>, Props, Payload}, State),
    {noreply, State};

handle_info({deliver, <<"task">>, _, Payload}, #state{channel = Channel} = State) ->
    case binary_to_term(Payload) of
    {monitor, Dn, Entry} ->
		dispatch(Dn, {monitor, Dn, Entry}),
        Reply = {monitored, Dn, node()},
        amqp:send(Channel, <<"task.reply">>, term_to_binary(Reply));
    {monitor, update, Dn, Entry} ->
		dispatch(Dn, {monitor, update, Dn, Entry}); 
    {unmonitor, Dn} ->
		%TODO: unmonitor reply?
		dispatch(Dn, {unmonitor, Dn});
    {discover, Dn, Entry} ->
		dispatch(Dn, {discover, Dn, Entry}),
        Reply = term_to_binary({discovered, Dn, node()}),
        amqp:send(Channel, <<"task.reply">>, Reply);
	{rediscover, Dn, Entry} -> %%disco by local server.
		disco_server:discover(Dn, Entry),
		Reply = term_to_binary({discovering, Dn, node()}),
        amqp:send(Channel, <<"task.reply">>, Reply);
    {disco, update, Dn, Entry} ->
		dispatch(Dn, {disco, update, Dn, Entry});
    {undiscover, Dn} ->
		dispatch(Dn, {undiscover, Dn});
    {auto_discover, Dn, Task} ->
		disco_server:auto_discover(Dn, Task);
    {reset, Dn, Entry} ->
		%FIXME: how to handle reset?
		dispatch(Dn, {reset, Dn, Entry});
    {async_task, Id, AsyncTask} ->
        ?INFO("async_task: ~p, ~p", [Id, AsyncTask]),
        {value, Name} = dataset:get_value(name, AsyncTask),
        spawn(fun() -> execute_async_task(Name, Id, AsyncTask) end);
    Term ->
        ?ERROR("unknown payload: ~p", [Term])
    end,
	{noreply, State};

handle_info({deliver, <<"ping">>, _, _}, #state{shard = Shard, channel = Channel} = State) ->
    Presence = {node(), node, available, extlib:appvsn(), <<"alive">>},
    amqp:send(Channel, <<"presence">>, term_to_binary(Presence)),
    amqp:send(Channel, <<"shard">>, term_to_binary({node(), Shard})),
    {noreply, State};

handle_info({deliver, <<"db.", _Tab/binary>> = Key, _, Payload}, State) ->
	DbTab = binary_to_atom(Key),
	?INFO("dbtab: ~s, payload: ~p", [DbTab, size(Payload)]),
	Records = binary_to_term(Payload),
	if
	DbTab == 'db.mib_oids' ->
		mib_registry:initialize({mib_oids, Records});
	DbTab == 'db.metrics' ->
		monitd_hub:initialize({metrics, Records});
	DbTab == 'db.disco_mods' ->
		disco_server:initialize({disco_mods, Records});
	true -> 
		ignore
	end,
	broadcast(DbTab, Records),
    {noreply, State};

handle_info({async_task_result, _Id, _Status, _Result} = Reply, 
	#state{channel = Channel} = State) ->
    amqp:send(Channel, <<"task.reply">>, term_to_binary(Reply)),
    {noreply, State};

handle_info({amqp, disconnected}, State) ->
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn}, #state{shard = Shard} = State) ->
	{noreply, State#state{channel = open(Conn, Shard)}};

handle_info(heartbeat, #state{channel = Channel} = State) ->
	Hearbeat = {node(), <<"monitd is alive.">>, []},
    amqp:send(Channel, <<"heartbeat">>, term_to_binary(Hearbeat)),
    erlang:send_after(10000, self(), heartbeat),
    {noreply, State};

handle_info({monitor, GcPid, long_gc, Info}, State) ->
    ?ERROR("long_gc: ~p, info: ~p", [GcPid, Info]),
    {noreply, State};

handle_info({monitor, GcPid, large_heap, Info}, State) ->
    ?ERROR("large_heap: ~p, gcpid: ~p", [GcPid, Info]),
    {noreply, State};

handle_info({monitor, SusPid, busy_port, Port}, State) ->
    ?ERROR("busy_port: ~p, port: ~p", [SusPid, Port]),
    ?ERROR("busy port: ~p", [erlang:port_info(Port)]), 
    ?ERROR("busy process: ~p", [erlang:process_display(SusPid, backtrace)]),
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

execute_async_task(<<"ping">>, Id, AsyncTask) ->
    {value, Host} = dataset:get_value(args, AsyncTask),
    Output = check_ping:cmd(Host),
    ?MODULE ! {async_task_result, Id, "COMPLETED", Output};

execute_async_task(<<"snmp">>, Id, AsyncTask) ->
    {value, Args} = dataset:get_value(args, AsyncTask),
    [Host, Community] = binary:split(Args, [<<" ">>], [global]),
    {Status, Summary} = check_snmp:run(binary_to_list(Host), Community),
    Output = list_to_binary([Status, "\n", Summary]),
    ?MODULE ! {async_task_result, Id, "COMPLETED", Output};

execute_async_task(<<"telnet">>, Id, AsyncTask) ->
    ?INFO("telnet task: ~p", [AsyncTask]),
    {value, Args} = dataset:get_value(args, AsyncTask),
    {value, Script} = dataset:get_value(attachment, AsyncTask),
    Script1 = string:strip(binary_to_list(Script), both, $\n),
    [Host, User, Password] = binary:split(Args, [<<",">>], [global]),
    %TODO: SUPPORT AUTELEN ONLY NOW
    {Status, Summary} = 
    case wifioss_telnet:connect(autelan_telnet, 
        binary_to_list(Host), binary_to_list(User), 
        binary_to_list(Password)) of
    {ok, Client} ->
        {ok, Reply} = wifioss_telnet:cmd(Client, "enable"),
        {ok, Reply1} = wifioss_telnet:cmd(Client, Script1),
        {"COMPLETED", Reply ++ Reply1};
    Error -> 
        ?ERROR("failt to connect ~p: ~p", [Host, Error]),
        {"ERROR", "failt to telnet."}
    end,
    ?MODULE ! {async_task_result, Id, Status, Summary};
    
execute_async_task(Name, _, _AsyncTask) ->
    ?ERROR("unknown async_task: ~p", [Name]).

dispatch(Key, Task) ->
	Pid = chash_pg:get_pid(monitd_coord, Key),
	Pid ! Task.

broadcast(Tab, Records) ->
	[Pid ! {Tab, Records} || Pid <- chash_pg:get_pids(monitd_coord)].

binary_to_atom(B) ->
	list_to_atom(binary_to_list(B)).

atom_to_binary(A) ->
	list_to_binary(atom_to_list(A)).

