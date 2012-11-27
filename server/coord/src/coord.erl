%%%----------------------------------------------------------------------
%%% File    : coord.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Dispatch tasks and monitor node.
%%% Created : 22 Oct 2008
%%% Updated : 23 Oct 2009
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(coord).

-include("coord.hrl").

-include_lib("mit/include/mit.hrl").

-include_lib("elog/include/elog.hrl").

-include_lib("amqp_client/include/amqp.hrl").

-import(erlang, [send_after/3]).

-import(proplists, [get_value/2]).

-export([start_link/1,
        status/0,
        lookup/1,
        shards/0,
        presences/0]).

-export([dispatch/1]).

-behavior(gen_server).

%%callback
-export([init/1, 
        handle_call/3, 
        prioritise_call/3,
        handle_cast/2, 
        handle_info/2, 
        prioritise_info/2,
        terminate/2, 
        code_change/3]).

-record(state, {channel, queue}).

-define(TIMEOUT, 600000).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Opts) ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

status() ->
    gen_server2:call(?MODULE, status).

lookup(Dn) ->
    gen_server2:call(?MODULE, {lookup, Dn}).

shards() ->
    gen_server2:call(?MODULE, shards).

presences() ->
    gen_server2:call(?MODULE, presences).

dispatch(Task) ->
    gen_server2:cast(?MODULE, {dispatch, Task}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([_Opts]) ->
    mnesia:create_table(shard, [
        {ram_copies, [node()]},
        {attributes, record_info(fields, shard)}]),
    mnesia:create_table(presence, [
        {ram_copies, [node()]}, {index, [class]},
        {attributes, record_info(fields, presence)}]),
    mnesia:create_table(dispatch, [
        {ram_copies, [node()]}, {index, [shard]}, 
        {attributes, record_info(fields, dispatch)}]),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    Queue = list_to_binary(atom_to_list(node())),
    State = #state{channel = Channel, queue=Queue},
    handle_info(ping, State),
    ?INFO_MSG("coord is started...[ok]"),
    {ok, State}.

open(Conn) ->
	{ok, Ch} = amqp:open_channel(Conn),
	{ok, Q} = amqp:queue(Ch, node()),

    %mit topic
	amqp:topic(Ch, <<"mit.event">>),
	amqp:bind(Ch, <<"mit.event">>, Q, <<"#">>),

    %watch topic
    amqp:topic(Ch, "sys.watch"),
	amqp:bind(Ch, "sys.watch", Q, <<"host">>),
    amqp:bind(Ch, "sys.watch", Q, <<"presence">>),
    amqp:bind(Ch, "sys.watch", Q, <<"heartbeat">>),

    %shard topic
    amqp:topic(Ch, "sys.shard"),
    amqp:bind(Ch, "sys.shard", Q, <<"*.shard">>),

    %consume
	amqp:consume(Ch, Q),
	Ch.

handle_call(status, _From, State) ->
    Reply = [{dispatch, mnesia:table_info(dispatch, size)}],
    {reply, {ok, Reply}, State};

handle_call({lookup, Dn}, _From, State) ->
    {reply, mnesia:dirty_read(dispatch, iolist_to_binary(Dn)), State};

handle_call(shards, _From, State) ->
    Keys = mnesia:dirty_all_keys(shard),
    Shards = lists:flatten([mnesia:dirty_read(shard, Key) || Key <- Keys]),
    {reply, Shards, State};

handle_call(presences, _From, State) ->
    Nodes = mnesia:dirty_all_keys(presence),
    Presences = lists:flatten([mnesia:dirty_read(presence, N) || N <- Nodes]),
    {reply, Presences, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

prioritise_call(status, _From, _State) ->
    10;
prioritise_call({dispatches, _Dn}, _From, _State) ->
    10;
prioritise_call({dispatch, _}, _From, _State) ->
    10;
prioritise_call(shards, _From, _State) ->
    10;
prioritise_call(_, _From, _State) ->
    5.

handle_cast({dispatch, {monitor, #node{dn = Dn} = Node}}, 
    #state{channel = Ch} = State) ->
    Payload = term_to_binary({node, node(), {monitor, Node}}),
    Timer = send_after(?TIMEOUT, self(), {timeout, {node, monitor, Dn}}),
    case mnesia:dirty_read(dispatch, Dn) of
    [] -> %still not be dispatched
        ShardQueue = list_to_binary(["shard.", Node#node.city]),
        ?INFO("dispatch node ~s to ~s ", [Dn, ShardQueue]),
        amqp:publish(Ch, <<"sys.shard">>, Payload, ShardQueue), 
        mnesia:dirty_write(#dispatch{dn = Dn, tref=Timer});
    [#dispatch{shard = ShardNode, tref = TRef} = Dispatch] ->  
        %has been dispatched
        cancel_timer(TRef),
        ?INFO("dispatch node ~s to ~s ", [Dn, ShardNode]),
        amqp:send(Ch, atom_to_list(ShardNode), Payload),
        mnesia:dirty_write(Dispatch#dispatch{tref=Timer})
    end,
    {noreply, State};

handle_cast({dispatch, {update, Node}}, #state{channel = Ch} = State) ->
    #node{dn=Dn, city=City} = Node,
    case mnesia:dirty_read(dispatch, Dn) of
    [] -> %still not be dispatched
        Timer = send_after(?TIMEOUT, self(), {timeout, {node, monitor, Dn}}),
        Payload = term_to_binary({node, node(), {monitor, Node}}),
        ShardQueue= list_to_binary(["shard.", City]),
        ?INFO("dispatch node ~s to ~s", [Dn, ShardQueue]),
        amqp:publish(Ch, <<"sys.shard">>, Payload, ShardQueue), 
        mnesia:dirty_write(#dispatch{dn=Dn, tref=Timer}); 
    [#dispatch{shard = undefined}] ->
        %shold pending for a while
        ?ERROR("shard is undefined for ~p when updating", [Dn]);
    [#dispatch{shard = ShardNode}] ->
        ?INFO("dispatch 'update ~s' to ~s", [Dn, ShardNode]),
        Payload = term_to_binary({node, node(), {update, Node}}),
        amqp:send(Ch, atom_to_list(ShardNode), Payload)
    end,
    {noreply, State};

handle_cast({dispatch, {delete, Dn}}, #state{channel = Ch} = State) ->
    Payload = term_to_binary({node, node(), {delete, Dn}}),
    case mnesia:dirty_read(dispatch, Dn) of
    [] ->
        ignore;
    [#dispatch{shard = undefined}] ->
        ?ERROR("shard is undefined for ~p when deleted.", [Dn]),
        ignore;
    [#dispatch{shard = ShardNode}] ->
        ?INFO("dispatch 'delete ~s' to ~s", [Dn, ShardNode]),
        amqp:send(Ch, atom_to_list(ShardNode), Payload)
    end,
    mnesia:dirty_delete(dispatch, Dn),
    {noreply, State};

handle_cast({dispatch, Task},  State) ->
    ?ERROR("unknown task: ~p", [Task]),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({deliver, <<"mit.deleted">>, _Props, Payload}, State) ->
    {deleted, Node} = binary_to_term(Payload),
    dispatch({delete, Node}),
    {noreply, State};

handle_info({deliver, <<"mit.inserted">>, _Props, Payload}, State) ->
    {inserted, Node} = binary_to_term(Payload), 
    dispatch({monitor, Node}),
    {noreply, State};

handle_info({deliver, <<"mit.updated">>, _Props, Payload}, State) ->
    {updated, Node} = binary_to_term(Payload), 
    dispatch({update, Node}),
    {noreply, State};

handle_info(ping, #state{channel = Channel} = State) ->
	amqp:publish(Channel, <<"sys.watch">>, <<"ping">>, <<"ping">>),
    send_after(60000, self(), ping),
	{noreply, State};

handle_info({deliver, <<"host">>, _, Payload}, State) ->
    case binary_to_term(Payload) of
    {host, HostInfo} ->
        DateTime = {datetime, date(), time()},
        %%save hostinfo into db
        {value, JID} = dataset:get_value(jid, HostInfo),
        case epgsql:select(main, servers, [id], {jid, JID}) of
            {ok, [_Record|_]} ->
                epgsql:update(main, servers, [{updated_at, DateTime} | HostInfo], {jid, JID});
            {ok, []} ->
                epgsql:insert(main, servers, [{created_at, DateTime}, {updated_at, DateTime} | HostInfo]);
            {error, Reason} ->
                ?ERROR("~p",[Reason])
        end;
    Term ->
        ?ERROR("bad hostinfo: ~p", [Term])
    end,
	{noreply, State};

handle_info({deliver, <<"presence">>, _, Payload}, State) ->
    case binary_to_term(Payload) of
	{Node, Class, Status, Vsn, Summary} ->
		handle_presence({Node, Class, Status, Summary}),
        case mnesia:dirty_read(presence, Node) of
        [Presence] ->
            cancel_timer(Presence#presence.tref);
        [] ->
            ok
        end,
        Tref = send_after(?TIMEOUT, self(), {offline, Node}),
        mnesia:dirty_write(#presence{node = Node, class = Class, 
            status = Status, vsn = Vsn, summary = Summary, tref = Tref});
	Term ->
		?ERROR("error presence: ~p", [Term])
	end,
	{noreply, State};

handle_info({deliver, <<"heartbeat">>, _, Payload}, State) ->
    Heartbeat = binary_to_term(Payload),
    case Heartbeat of
    {Node, Summary, Metrics} ->
        case mnesia:dirty_read(presence, Node) of
        [Presence] ->
            cancel_timer(Presence#presence.tref),
            Tref = send_after(?TIMEOUT, self(), {offline, Node}),
            mnesia:dirty_write(Presence#presence{node = Node, summary = Summary, 
                metrics = Metrics, tref = Tref});
        [] ->
            ?ERROR("no presence found for ~s", [Node])
        end;
    _ ->
        ?ERROR("bad heartbeat: ~p", [Heartbeat])
    end,
	{noreply, State};

handle_info({deliver, Queue, _, Payload}, #state{queue=Queue} = State) ->
    case binary_to_term(Payload) of
    {node, FromShard, {monitored, Dn}} ->
        ?INFO("monitored ~s by ~s", [Dn, FromShard]),
        case mnesia:dirty_read(dispatch, Dn) of
        [#dispatch{shard = OldShard} = Dispatch] ->
            if
            OldShard == undefined -> 
                ok;
            OldShard == FromShard -> 
                ok;
            true -> 
                ?ERROR("tow shard for one dn: ~s", [Dn]),
                ?ERROR("oldshard: ~s, newshard: ~s", [OldShard, FromShard])
            end,
            cancel_timer(Dispatch#dispatch.tref),
            mnesia:dirty_write(Dispatch#dispatch{shard = FromShard, tref = undefined}); 
        [] -> 
            ?ERROR("bad_shard_reply for ~s", [Dn])
        end;
    BadTerm ->
        ?ERROR("badterm: ~p", [BadTerm])
    end,
    {noreply, State};

handle_info({deliver, <<"join.shard">>, _, Payload}, State) ->
    case binary_to_term(Payload) of
    {join, Node, Queues} ->
        mnesia:dirty_write(#shard{node=Node, queue=Queues, count=0});
    BadTerm ->
        ?ERROR("error shard: ~p", [BadTerm])
    end,
    {noreply, State};

handle_info({offline, Node}, State) ->
    ?ERROR("~s is offline", [Node]),
    case mnesia:dirty_read(presence, Node) of
    [Presence] ->
        handle_offline(Presence);
    [] ->
        ok
    end,
    mnesia:dirty_delete(presence, Node),
    {noreply, State};

handle_info({amqp, disconnected}, State) ->
	?INFO("amqp is disconnected...", []),
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn}, State) ->
	?INFO("amqp is reconnected...", []),
	{noreply, State#state{channel = open(Conn)}};

handle_info({timeout, {node, monitor, Dn}}, State) ->
    ?ERROR("dispatch timeout: ~s", [Dn]),
    mnesia:dirty_delete(dispatch, Dn),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

prioritise_info({deliver, <<"presence">>, _, _}, _State) ->
    10;

prioritise_info({deliver, <<"heartbeat">>, _, _}, _State) ->
    9;

prioritise_info(_, _State) ->
    0.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cancel_timer(undefined) ->
    ok;

cancel_timer(Ref) ->
    erlang:cancel_timer(Ref).

handle_presence({Node, node, unavailable, _Summary}) ->
    DateTime = {datetime, date(), time()},
    epgsql:update(main, servers, [{presence, 0}, {updated_at, DateTime}], {jid, Node});

handle_presence({Node, node, available, _Summary}) ->
    DateTime = {datetime, date(), time()},
    epgsql:update(main, servers, [{presence, 1}, {updated_at, DateTime}], {jid, Node});

handle_presence(_) ->
	ignore.

handle_offline(#presence{node = Node, class = node}) ->
    DateTime = {datetime, date(), time()},
    epgsql:update(main, servers, [{presence, 0}, {updated_at, DateTime}], {jid, Node}),
    Dispatches = mnesia:dirty_index_read(dispatch, Node, #dispatch.shard),
    [mnesia:dirty_delete(dispatch, D#dispatch.dn) || D <- Dispatches],
	?ERROR("shard offline: ~p", [Node]),
    mnesia:dirty_delete(shard, Node);

handle_offline(_Presence) ->
    ignore.


