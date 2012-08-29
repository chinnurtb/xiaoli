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

-record(dispatch, {id, dn, node, tref}).

-record(state, {channel, is_sharded, shards}).

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

dispatch(#entry{dn = Dn, attrs = Attrs} = Entry) ->
    case is_device(Entry) of
    true -> 
        dispatch({discover, Dn, Attrs}),
        case need_monitor(Entry) of
        true ->
            dispatch({monitor, Dn, Attrs});
        false ->
            ?INFO("dont monitor ~s", [Dn]),
            ok
        end;
    false -> 
        ok
    end;

dispatch(Task) ->
    gen_server2:call(?MODULE, {dispatch, Task}).
    
init([Env]) ->
    mnesia:create_table(dispatch, [
        {ram_copies, [node()]}, {index, [dn, node]}, 
        {attributes, record_info(fields, dispatch)}]),
    IsSharded = proplists:get_value(is_sharded, Env, false),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    ?INFO_MSG("coord_dist is started...[ok]"),
    {ok, #state{channel = Channel, is_sharded = IsSharded, shards = []}}.

open(Conn) ->
	{ok, Channel} = amqp:open_channel(Conn),
	%shards
	amqp:queue(Channel, <<"shard">>),
	amqp:consume(Channel, <<"shard">>),

	%tasks
	amqp:queue(Channel, <<"task">>),
	amqp:queue(Channel, <<"task.reply">>),
	amqp:consume(Channel, <<"task.reply">>),

	%async tasks from webport
	amqp:queue(Channel, <<"async_task">>),
	amqp:consume(Channel, <<"async_task">>),

	{ok, Q} = amqp:queue(Channel, node()),
	amqp:topic(Channel, <<"oss.mit">>),
	amqp:bind(Channel, <<"oss.mit">>, Q, <<"#">>),
	amqp:consume(Channel, Q),

	Channel.

handle_call(shards, _From, #state{shards = Shards} = State) ->
    {reply, Shards, State};

handle_call({dispatches, Dn}, _From, State) ->
    Dispatches = mnesia:dirty_index_read(dispatch, extbif:to_binary(Dn), #dispatch.dn),
    {reply, Dispatches, State};

handle_call(status, _From, State) ->
    Reply = [{dispatch, mnesia:table_info(dispatch, size)}],
    {reply, {ok, Reply}, State};

handle_call({dispatch, {discover, Dn, Entry}}, _From, #state{channel = Channel,
    is_sharded = IsSharded, shards = Shards} = State) ->
    ?INFO("dispatch 'discover ~s' task", [Dn]),
    Payload = term_to_binary({discover, Dn, Entry}),
    Timer = send_after(?HOUR, self(), {timeout, {disco, Dn}}),
    NewState = 
    case mnesia:dirty_read(dispatch, {disco, Dn}) of
    [] -> %still not be discovered
        case IsSharded of
        false ->
            amqp:send(Channel, <<"task">>, Payload),
            mnesia:dirty_write(#dispatch{id = {disco, Dn}, dn = Dn, tref=Timer}),
            State;
        true ->
            case find_shard_for_disco(binary_to_list(Dn), Shards) of
            {Node, Scope, DiscoTasks, MonTasks} ->
                amqp:send(Channel, atom_to_list(Node), Payload),
                mnesia:dirty_write(#dispatch{id = {disco, Dn}, dn = Dn, node = Node, tref=Timer}),
                NewShard = {Node, Scope, DiscoTasks+1, MonTasks},
                NewShards = lists:keyreplace(Node, 1, Shards, NewShard),
                State#state{shards = NewShards};
            false ->
                ?ERROR("no shard for ~p", [Dn]),
                State
            end
        end;
    [#dispatch{node = N, tref = TRef} = Dispatch] ->  %has been monitored
        cancel_timer(TRef),
        amqp:send(Channel, atom_to_list(N), Payload),
        mnesia:dirty_write(Dispatch#dispatch{tref=Timer}),
        State
    end,
    {reply, ok, NewState};

handle_call({dispatch, {reset, Dn, Entry}}, _From, #state{channel = Channel, 
    is_sharded = IsSharded, shards = Shards} = State) ->
    ?INFO("dispatch 'reset ~s' task", [Dn]),
    Payload = term_to_binary({reset, Dn, Entry}),
	NewState = 
    case IsSharded of
    false ->
        amqp:send(Channel, <<"task">>, Payload),
		State;
    true ->
        case find_shard_for_disco(binary_to_list(Dn), Shards) of
        {Node, Scope, DiscoTasks, MonTasks} ->
            amqp:send(Channel, atom_to_list(Node), Payload),
            NewShard = {Node, Scope, DiscoTasks+1, MonTasks},
            NewShards = lists:keyreplace(Node, 1, Shards, NewShard),
            State#state{shards = NewShards};
        false ->
            ?ERROR("no shard for ~p", [Dn]),
            State
        end
    end,
	{reply, ok, NewState};

handle_call({dispatch, {monitor, Dn, Entry}}, _From, #state{channel = Channel,
    is_sharded = IsSharded, shards = Shards} = State) ->
    ?INFO("dispatch 'monitor ~s' task", [Dn]),
    Payload = term_to_binary({monitor, Dn, Entry}),
    Timer = send_after(?HOUR, self(), {timeout, {monitor, Dn}}),
    NewState = 
    case mnesia:dirty_read(dispatch, {monitor, Dn}) of
    [] -> %still not be monitored
        case IsSharded of
        false ->
            amqp:send(Channel, <<"task">>, Payload), 
            mnesia:dirty_write(#dispatch{id = {monitor, Dn}, dn = Dn, tref=Timer}),
            State;
        true ->
            case find_shard_for_monitor(binary_to_list(Dn), Shards) of
            {Node, Scope, DiscoTasks, MonTasks} ->
                amqp:send(Channel, atom_to_list(Node), Payload),
                mnesia:dirty_write(#dispatch{id = {monitor, Dn},
					dn = Dn, node = Node, tref=Timer}),
                NewShard = {Node, Scope, DiscoTasks, MonTasks+1},
                NewShards = lists:keyreplace(Node, 1, Shards, NewShard),
                State#state{shards = NewShards};
            false ->
                ?ERROR("no shard for ~p", [Dn]),
                State
            end
        end;
    [#dispatch{node = N, tref = TRef} = Dispatch] ->  %has been monitored
        cancel_timer(TRef),
        amqp:send(Channel, atom_to_list(N), Payload),
        mnesia:dirty_write(Dispatch#dispatch{tref=Timer}),
        State
    end,
    {reply, ok, NewState};

handle_call({dispatch, {update, Dn, Entry}}, _From, #state{channel = Channel} = State) ->
    %fix bug #858
    %dispatch monitor update task
    ?INFO("dispatch 'update ~s' task", [Dn]),
    case mnesia:dirty_read(dispatch, {monitor, Dn}) of
    [] -> %still not be monitored, monitor it????
        spawn(fun() -> dispatch({monitor, Dn, Entry}) end);
    [#dispatch{node = undefined}] ->
        ?WARNING("node is undefined: ~p", [{monitor, Dn}]);
    [#dispatch{node = Node}] ->
        amqp:send(Channel, atom_to_list(Node), 
            term_to_binary({monitor, update, Dn, Entry}))
    end,
    %dispatch disco update task
    case mnesia:dirty_read(dispatch, {disco, Dn}) of
    [] ->
        ?WARNING("no dispatch found: ~p", [{disco, Dn}]);
    [#dispatch{node = undefined}] ->
        ?WARNING("node is undefined: ~p", [{disco, Dn}]);
    [#dispatch{node = DiscoNode}] ->
        amqp:send(Channel, atom_to_list(DiscoNode), 
            term_to_binary({disco, update, Dn, Entry}))
    end,
    {reply, ok, State};

handle_call({dispatch, {delete, Dn}}, _From, #state{channel = Channel} = State) ->
    case mnesia:dirty_read(dispatch, {disco, Dn}) of
    [] ->
        ignore;
    [#dispatch{node = undefined}] ->
        ignore;
    [#dispatch{node = Node1}] ->
        ?INFO("dispatch 'delete ~s' to disco node: ~p", [Dn, Node1]),
        amqp:send(Channel, atom_to_list(Node1),
			term_to_binary({undiscover, Dn}))
    end,
    case mnesia:dirty_read(dispatch, {monitor, Dn}) of
    [] ->
        ignore;
    [#dispatch{node = undefined}] ->
        ignore;
    [#dispatch{node = Node2}] ->
        ?INFO("dispatch 'delete ~s' to monitor node: ~p", [Dn, Node2]),
        amqp:send(Channel, atom_to_list(Node2), term_to_binary({unmonitor, Dn}))
    end,
    Tasks = mnesia:dirty_index_read(dispatch, Dn, #dispatch.dn),
    [mnesia:dirty_delete(dispatch, T#dispatch.id) || T <- Tasks],
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
	{updated, Dn, #entry{attrs = Attrs} = Entry} ->
		?INFO("mit updated: ~s", [Dn]),
		case is_device(Entry) of
		true -> 
			DiscoveryState = get_value(discovery_state, Attrs),
			Task = 
			case DiscoveryState of
			9 -> {reset, Dn, Attrs}; %reset
			2 -> {discover, Dn, Attrs}; %rediscover
			_ -> {update, Dn, Attrs} %update
			end,
			spawn(fun() -> dispatch(Task) end);
		false -> 
			ok
		end;
	BadTerm ->
		?ERROR("badterm: ~p", [BadTerm])
	end,
    {noreply, State};

handle_info({deliver, <<"async_task">>, _Props, Payload}, #state{channel = Channel,
	is_sharded = IsSharded, shards = Shards} = State) ->
    ?INFO("async task from webport: ~p", [Payload]),
    case binary_to_list(Payload) of
    "taskid:" ++ S ->
        Id = list_to_integer(S),
        case emysql:select({async_tasks, {id, Id}}) of
        {ok, []} ->
            ?ERROR("cannot found async_task: ~p", [Id]);
        {ok, [AsyncTask]} ->
            {value, Dn} = dataset:get_value(mo, AsyncTask),
            {value, Timeout} = dataset:get_value(timeout, AsyncTask),
            Timer = send_after(Timeout*1000, self(), {timeout, {async_task, Id}}),
            Queue = 
            case IsSharded of
            false ->
                <<"task">>;
            true ->
                case find_shard_for_disco(binary_to_list(Dn), Shards) of %TODO: should refactor name of '_for_disco'
                {Node, _, _, _} ->
                    atom_to_list(Node);
                false ->
                    ?ERROR("no shard for ~p", [Dn]),
                    false
                end
            end,
            case Queue of
            false ->
                ?ERROR("no available node for async_task: ~p", [Id]); %TODO: should update mysql
            _ ->
                amqp:send(Channel, Queue, term_to_binary({async_task, Id, AsyncTask})), 
                mnesia:dirty_write(#dispatch{id = {async_task, Id}, dn = Dn, tref=Timer}),
                DateTime = {datetime, {date(), time()}},
                emysql:update(async_tasks, [{executed_at, DateTime}], {id, Id})
            end;
        {error, Reason} ->
            ?ERROR("~p", [Reason])
        end;
    _ ->
        ?ERROR("unexpected async_task: ~p", [Payload])
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
        false -> [{Node, Scope, 0, 0}|Shards]
        end,
        {noreply, State#state{shards = Shards1}};
    Term ->
        ?ERROR("error shard: ~p", [Term]),
        {noreply, State}
    end;

handle_info({timeout, {async_task, Id}}, State) ->
    DateTime = {datetime, {date(), time()}},
    emysql:update(async_tasks, [{status, <<"TIMEOUT">>}, {finished_at, DateTime}], {id, Id}),
    mnesia:dirty_delete(dispatch, Id),
    {noreply, State};

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

handle_reply({async_task_result, Id, Status, Result} = R, _State) ->
    case mnesia:dirty_read(dispatch, {async_task, Id}) of
    [Dispatch] ->
        ?ERROR("async_task_reply: ~p, ~p", [Status, Result]),
        cancel_timer(Dispatch#dispatch.tref),
        mnesia:dirty_delete(dispatch, Dispatch#dispatch.id),
        DateTime = {datetime, {date(), time()}},
        Result1 = emysql:escape(Result),
        Record = [{status, Status}, {result, Result1}, {finished_at, DateTime}],
        Res = emysql:update(async_tasks, Record, {id, Id}),
        ?ERROR("~p", [Res]);
    [] -> 
        ?ERROR("unexpected reply: ~p", [R])
    end;

handle_reply({discovered, Dn, Node}, _State) ->
    ?INFO("~s is discovering by ~s", [Dn, Node]),
    case mnesia:dirty_read(dispatch, {disco, Dn}) of
    [Dispatch] ->
        cancel_timer(Dispatch#dispatch.tref),
        mnesia:dirty_write(Dispatch#dispatch{node = Node, tref=undefined}); 
    [] -> 
        ?ERROR("bad_disco_reply for ~s", [Dn])
    end;

handle_reply({monitored, Dn, Node}, _State) ->
    ?INFO("~s is monitored by ~s", [Dn, Node]),
    case mnesia:dirty_read(dispatch, {monitor, Dn}) of
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

find_shard_for_disco(Dn, Shards) ->
    find_shard_for_disco(Dn, Shards, false).

find_shard_for_disco(_Dn, [], Ret) ->
    Ret;
find_shard_for_disco(Dn, [{_, Scope, _, _} = Shard | Shards], false) ->
    case scope_match(Scope, Dn) of
    true ->
        find_shard_for_disco(Dn, Shards, Shard);
    false ->
        find_shard_for_disco(Dn, Shards, false)
    end;
find_shard_for_disco(Dn, [{_, Scope, DiscoTasks1, _} = Shard | Shards], {_, _, DiscoTasks2, _} = LastShard) ->
    case scope_match(Scope, Dn) of
    true ->
        if
        DiscoTasks1 < DiscoTasks2 ->
            find_shard_for_disco(Dn, Shards, Shard);
        true ->
            find_shard_for_disco(Dn, Shards, LastShard)
        end;
    false ->
        find_shard_for_disco(Dn, Shards, LastShard)
    end.

find_shard_for_monitor(Dn, Shards) ->
    find_shard_for_monitor(Dn, Shards, false).

find_shard_for_monitor(_Dn, [], Ret) ->
    Ret;
find_shard_for_monitor(Dn, [{_, Scope, _, _} = Shard | Shards], false) ->
    case scope_match(Scope, Dn) of
    true ->
        find_shard_for_monitor(Dn, Shards, Shard);
    false ->
        find_shard_for_monitor(Dn, Shards, false)
    end;
find_shard_for_monitor(Dn, [{_, Scope, _, MonTasks1} = Shard | Shards], {_, _, _, MonTasks2} = LastShard) ->
    case scope_match(Scope, Dn) of
    true ->
        if
        MonTasks1 < MonTasks2 ->
            find_shard_for_monitor(Dn, Shards, Shard);
        true ->
            find_shard_for_monitor(Dn, Shards, LastShard)
        end;
    false ->
        find_shard_for_monitor(Dn, Shards, LastShard)
    end.

scope_match({endwith, Suffix}, Dn) ->
    lists:suffix(Suffix, Dn);
scope_match({endwithout, Suffix}, Dn) ->
    (not lists:suffix(Suffix, Dn));
scope_match({startwith, Prefix}, Dn) ->
    lists:prefix(Prefix, Dn);
scope_match({startwithout, Prefix}, Dn) ->
    (not lists:prefix(Prefix, Dn)).
    
is_device(#entry{class = ObjectClass}) ->
    lists:member(<<"ossIpDevice">>, binary:split(ObjectClass, <<"/">>, [global])).

need_monitor(#entry{attrs = Attrs}) ->
    {value, ApFit} = dataset:get_value(ap_fit, Attrs, 0),
    {value, DiscoverState} = dataset:get_value(discovery_state, Attrs, 0),
    case {ApFit, DiscoverState} of
    {2, ?UNDISCOVERED} ->
        false;
    {2, _} ->
        true;
    {1, _} ->
        true;
    {_, ?DISCOVERED} ->
        true;
    {_, ?REDISCOVER} ->
        true;
    _ ->
        false
    end.

