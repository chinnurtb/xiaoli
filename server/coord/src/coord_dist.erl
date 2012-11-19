%%%----------------------------------------------------------------------
%%% File    : coord_dist.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : distribute tasks to nodes.
%%% Created : 24 Arg 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(coord_dist).

-author('ery.lee@gmail.com').

-include("coord.hrl").

-include_lib("mit/include/mit.hrl").

-include_lib("elog/include/elog.hrl").

-include_lib("amqp_client/include/amqp.hrl").

-import(erlang, [send_after/3]).

-import(proplists, [get_value/2]).

-export([start_link/1,
		offline/1,
        shards/0,
        status/0,
        dispatches/1,
        dispatch/1]).

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

-record(state, {channel, shards}).

-define(HOUR, 3600000).

start_link(Env) ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [Env], []).

status() ->
    gen_server2:call(?MODULE, status).

shards() ->
    gen_server2:call(?MODULE, shards).

dispatches(Dn) ->
    gen_server2:call(?MODULE, {dispatches, Dn}).

offline(Presence) ->
	gen_server2:cast(?MODULE, {offline, Presence}).

dispatch(Node) when is_record(Node, node) ->
    gen_server2:call(?MODULE, {dispatch, node, Node});

dispatch(Task) ->
    gen_server2:call(?MODULE, {dispatch, task, Task}).
    
init([Env]) ->
    mnesia:create_table(dispatch, [
        {ram_copies, [node()]}, {index, [dn, node]}, 
        {attributes, record_info(fields, dispatch)}]),
	{ok, Conn} = amqp:connect(),
    Ch = open(Conn),
    ?INFO_MSG("coord_dist is started...[ok]"),
    {ok, #state{channel = Ch, shards = []}}.

open(Conn) ->
    %open channel and declare queue
	{ok, Ch} = amqp:open_channel(Conn),
	{ok, Q} = amqp:queue(Ch, node()),

	%shards
	amqp:queue(Ch, <<"shard">>),
	amqp:consume(Ch, <<"shard">>),

    %mit topic
	amqp:topic(Ch, <<"mit.event">>),
	amqp:bind(Ch, <<"mit.event">>, Q, <<"#">>),
    amqp:topic(Ch, <<"mit.node">>),
    amqp:bind(Ch, <<"mit.node">>, Q, <<"node.dist">>), 

	%tasks
	amqp:queue(Ch, <<"task">>),
	amqp:queue(Ch, <<"task.reply">>),
	amqp:consume(Ch, <<"task.reply">>),

	%async tasks from webport
	amqp:queue(Ch, <<"async_task">>),
	amqp:consume(Ch, <<"async_task">>),

	amqp:consume(Ch, Q),

	Ch.

handle_call(shards, _From, #state{shards = Shards} = State) ->
    {reply, Shards, State};

handle_call({dispatches, Dn}, _From, State) ->
    Dispatches = mnesia:dirty_index_read(dispatch, extbif:to_binary(Dn), #dispatch.dn),
    {reply, Dispatches, State};

handle_call(status, _From, State) ->
    Reply = [{dispatch, mnesia:table_info(dispatch, size)}],
    {reply, {ok, Reply}, State};

handle_call({dispatch, node, Node}, _From, 
    #state{channel = Ch, shards = Shards} = State) ->
    Payload = term_to_binary({node, Node}),
    Dn = Node#node.rdn,
    Timer = send_after(?HOUR, self(), {timeout, {node, Node#node.rdn}}),
    NewState = 
    case mnesia:dirty_read(dispatch, {node, Node#node.rdn}) of
    [] -> %still not be dispatched
        RouteKey = list_to_binary(["node.", Node#node.city]),
        ?INFO("dispatch node ~s to ~s ", [Node#node.rdn, RouteKey]),
        amqp:publish(Ch, <<"mit.node">>, Payload, RouteKey), 
        mnesia:dirty_write(#dispatch{id = {node, Dn}, dn = Dn, tref=Timer}),
        State;
    [#dispatch{node = N, tref = TRef} = Dispatch] ->  %has been dispatched
        cancel_timer(TRef),
        ?INFO("dispatch node ~s to ~s ", [Node#node.rdn, N]),
        amqp:send(Ch, atom_to_list(N), Payload),
        mnesia:dirty_write(Dispatch#dispatch{tref=Timer}),
        State
    end,
    {reply, ok, NewState};

handle_call({dispatch, {update, Node}}, _From, #state{channel = Ch} = State) ->
    %dispatch monitor update task
    Dn = Node#node.rdn,
    ?INFO("dispatch 'update ~s' task", [Node#node.rdn]),
    Payload = term_to_binary({node, update, Node}),
    case mnesia:dirty_read(dispatch, {node, Node#node.rdn}) of
    [] -> %still not be dispatched
        RouteKey = list_to_binary(["node.", Node#node.city]),
        amqp:publish(Ch, <<"mit.node">>, Payload, RouteKey), 
        mnesia:dirty_write(#dispatch{id = {node, Dn}, dn = Dn, tref=undefined}); %TODO: FIX LATER
    [#dispatch{node = undefined}] ->
        ?WARNING("node is undefined for ~p", [Node#node.rdn]);
    [#dispatch{node = N}] ->
        amqp:send(Ch, atom_to_list(N), Payload)
    end,
    {reply, ok, State};

handle_call({dispatch, {delete, Dn}}, _From, #state{channel = Ch} = State) ->
    case mnesia:dirty_read(dispatch, {node, Dn}) of
    [] ->
        ignore;
    [#dispatch{node = undefined}] ->
        ignore;
    [#dispatch{node = N}] ->
        ?INFO("dispatch 'delete ~s' to monitor node: ~p", [Dn, N]),
        Payload = term_to_binary({node, delete, Dn}),
        amqp:send(Ch, atom_to_list(N), Payload)
    end,
    Dispatches = mnesia:dirty_index_read(dispatch, Dn, #dispatch.dn),
    [mnesia:dirty_delete(dispatch, D#dispatch.id) || D <- Dispatches],
    {reply, ok, State};

handle_call({dispatch, Event}, _From, State) ->
    ?ERROR("unexpected dispatch event: ~p", [Event]),
    {reply, ok, State};

handle_call(Req, _From, State) ->
    {reply, {error, {badreq, Req}}, State}.

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

handle_cast({offline, #presence{node=Node, type=node}}, #state{shards = Shards} = State) ->
    %%TODO: should redispatch all the tasks.
    Tasks = mnesia:dirty_index_read(dispatch, Node, #dispatch.node),
    lists:foreach(fun(T) -> 
        mnesia:dirty_delete(dispatch, T#dispatch.id)
    end, Tasks),
	?ERROR("shard offline: ~p", [Node]),
    Shards1 = lists:keydelete(Node, 1, Shards), 
	{noreply, State#state{shards = Shards1}};

handle_cast({offline, Presence}, State) ->
	?ERROR("offline ~p", [Presence]),   
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({deliver, <<"mit.deleted">>, _Props, Payload}, State) ->
    case binary_to_term(Payload) of
	{deleted, Dn} ->
		?INFO("mit deleted: ~s", [Dn]),
		spawn(fun() -> dispatch({delete, Dn}) end);
	BadTerm -> 
		?ERROR("badterm: ~p", [BadTerm])
	end,
    {noreply, State};

handle_info({deliver, <<"mit.inserted">>, _Props, Payload}, State) ->
    case binary_to_term(Payload) of
	{inserted, Dn, Entry} -> 
		?INFO("mit inserted: ~s", [Dn]),
		spawn(fun() -> dispatch(Entry) end);
	BadTerm ->
		?ERROR("badterm: ~p", [BadTerm])
	end,
    {noreply, State};

handle_info({deliver, <<"mit.updated">>, _Props, Payload}, State) ->
    case binary_to_term(Payload) of
	{updated, #node{rdn=Dn, attrs = Attrs}} ->
		?INFO("mit updated: ~s", [Dn]);
        %redispatch 
	BadTerm ->
		?ERROR("badterm: ~p", [BadTerm])
	end,
    {noreply, State};

handle_info({deliver, <<"task.reply">>, _, Payload}, State) ->
    handle_reply(binary_to_term(Payload), State),
	{noreply, State};

handle_info({deliver, <<"shard">>, _, Payload}, #state{shards = Shards} = State) ->
    case binary_to_term(Payload) of
    {Node, Scope} ->
        Shards1 =
        case lists:keysearch(Node, 1, Shards) of
        {value, _} -> Shards;
        false -> [{Node, Scope, 0}|Shards]
        end,
        {noreply, State#state{shards = Shards1}};
    Term ->
        ?ERROR("error shard: ~p", [Term]),
        {noreply, State}
    end;

handle_info({timeout, Id}, State) ->
    ?ERROR("dispatch timeout: ~p", [Id]),
    mnesia:dirty_delete(dispatch, Id),
    {noreply, State};

handle_info({amqp, disconnected}, State) ->
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn}, State) ->
	{noreply, State#state{channel = open(Conn)}};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

prioritise_info({deliver, <<"shard">>, _, _}, _State) ->
    10;

prioritise_info({deliver, <<"task.reply">>, _, _Payload}, _State) ->
    5;
    
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

handle_reply({managed, Dn, Node}, _State) ->
    ?INFO("~s is monitored by ~s", [Dn, Node]),
    case mnesia:dirty_read(dispatch, {node, Dn}) of
    [#dispatch{node = OldNode} = Dispatch] ->
        if 
        OldNode == undefined -> ok;
        OldNode == Node -> ok;
        true -> ?ERROR("tow nodes for one dn: ~s~noldnode: ~s, newnode: ~s", [Dn, OldNode, Node])
        end,
        cancel_timer(Dispatch#dispatch.tref),
        mnesia:dirty_write(Dispatch#dispatch{node = Node, tref = undefined}); 
    [] -> 
        ?ERROR("bad_monitor_reply for ~s", [Dn])
    end;

handle_reply(Reply, _State) ->
    ?ERROR("badreply: ~p", [Reply]).

