%%----------------------------------------------------------------------
%%% File    : monitd_oscmd.erl
%%% Author  : ery.lee@gmail.com
%%% Purpose : execute external command.
%%% Created : 13 Jan 2010 
%%% License : http://www.monit.cn/
%%%
%%% Copyright (C) 2007-2010, www.monit.cn
%%%----------------------------------------------------------------------
-module(monitd_oscmd).

-include_lib("elog/include/elog.hrl").

-export([start_link/1, 
        run/1, 
        stop/0]).

-behaviour(gen_server).

-export([init/1, 
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 code_change/3,
		 terminate/2]).

-record(running_cmd, {ref, pid, from, cmd}).

-record(state, {pool_size, waiting_queue}).

start_link(PoolSize) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PoolSize], []).

stop() ->
	gen_server:call(?MODULE, stop).

run(Cmd) ->
    gen_server:call(?MODULE, {run, Cmd}, 30000).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([PoolSize]) ->
    process_flag(trap_exit, true),
    mnesia:create_table(running_cmd,
        [{ram_copies, [node()]}, {index, [pid]},
         {attributes, record_info(fields, running_cmd)}]),
    mnesia:add_table_copy(running_cmd, node(), ram_copies),
    ?INFO_MSG("monitd_oscmd is started...[ok]"),
	{ok, #state{pool_size = PoolSize, waiting_queue = queue:new()}}.

%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({run, Cmd}, From, State) ->
    NewState = run_cmd(From, Cmd, State),
    {noreply, NewState};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?ERROR("badmsg: ~p", [Msg]),
    {noreply, State}.
    
handle_info({cmd_out, Ref, Output}, State) ->
    case mnesia:dirty_read(running_cmd, Ref) of
    [#running_cmd{from = From} = RunningCmd] ->
        gen_server:reply(From, Output),
        mnesia:sync_dirty(fun() -> mnesia:delete_object(RunningCmd) end);
    [] ->
        ?WARNING("cannot find running_cmd: ~p", [Ref])
    end,
    {noreply, check_waiting(State)};

handle_info({'EXIT', Pid, Reason}, State) ->
    case mnesia:dirty_index_read(running_cmd, Pid, #running_cmd.pid) of
    [] ->
        ok;
    Cmds ->
        lists:foreach(fun(#running_cmd{from = From} = Cmd) -> 
            gen_server:reply(From, {error, {'EXIT', Reason}}),
            mnesia:sync_dirty(fun() -> mnesia:delete_object(Cmd) end)
        end, Cmds)
    end,
    {noreply, check_waiting(State)};

handle_info(Info, State) ->
    ?ERROR("badinfo: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

run_cmd(From, Cmd, #state{pool_size = PoolSize, 
        waiting_queue = Queue} = State) ->
    Size = mnesia:table_info(running_cmd, size),
    if
    Size >= PoolSize -> %%add to waiting queue
        NewQueue = queue:in({From, Cmd}, Queue),
        Len = queue:len(NewQueue), 
        if 
        Len > 20 -> ?WARNING("oscmd waiting queue len: ~p", [Queue]);
        true -> ok
        end,
        State#state{waiting_queue = NewQueue};
    true -> %%spawn and run
        Ref = make_ref(),
        Pid = spawn_link(fun() -> 
                Output = os:cmd(extbif:to_list(Cmd)),
                ?MODULE ! {cmd_out, Ref, string:strip(Output, right, $\n)}
        end),
        mnesia:sync_dirty(fun() -> 
            mnesia:write(#running_cmd{ref = Ref, pid = Pid, from = From, cmd = Cmd})        
        end),
        State
    end.

check_waiting(#state{waiting_queue = Queue} = State) ->
    case queue:out(Queue) of
    {{value, {From, Cmd}}, Q2} ->
        run_cmd(From, Cmd, State#state{waiting_queue = Q2});
    {empty, _} ->
        State
    end.

