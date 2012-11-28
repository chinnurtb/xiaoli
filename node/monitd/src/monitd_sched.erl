%%%----------------------------------------------------------------------
%%% File    : monitd_sched.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Monitor task scheduler
%%% Created : 24 Dec 2009
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(monitd_sched).

-include("mit.hrl").

-include("monitor.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([start_link/0,
        setup/1]).

-export([schedule/2, 
         schedule/3, 
         unschedule/1, 
         reschedule/2, 
         update_task/2, 
         clear_tasks/0,
         task_num/0]).

-behavior(gen_server).

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

setup({timeperiods, Periods}) ->
    gen_server:call(?MODULE, {setup, timeperiods, Periods}).

schedule(Task, Interval) ->
    schedule(Task, Interval, 0).

schedule(Task, Interval, Delay) ->
    gen_server:cast(?MODULE, {schedule, Task, Interval, Delay}).

unschedule(TaskId) ->
    gen_server:cast(?MODULE, {unschedule, TaskId}).

reschedule(Task, Interval) ->
    gen_server:cast(?MODULE, {reschedule, Task, Interval}).

update_task(TaskId, Period) when is_integer(Period) ->
    gen_server:cast(?MODULE, {update_period, TaskId, Period});

update_task(TaskId, Args) when is_list(Args) ->
    gen_server:cast(?MODULE, {update_args, TaskId, Args}).

clear_tasks() ->
    gen_server:cast(?MODULE, clear_tasks).

task_num() ->
    ets:info(monitor_task, size).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Result = mnesia:create_table(xxxxxx, [{ram_copies, [node()]},
        {attributes, record_info(fields, timeperiod)}]),
    ?INFO("~p", [Result]),
    ets:new(timeperiod, [set, named_table, protected, {keypos, 2}]),
    ets:new(monitor_task, [set, named_table, protected, {keypos, 2}]),
    erlang:send_after(300 * 1000, self(), introspect),
    ?INFO_MSG("monitd_sched is started...[ok]"),
    {ok, state}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({setup, timeperiods, Records}, _From, State) ->
    lists:foreach(fun(R) -> 
        Hour = split(get_value(hour, R, <<"*">>)),
        DayOfMonth = split(get_value(dayofmonth, R, <<"*">>)),
        Month = split(get_value(month, R, <<"*">>)),
        DayOfWeek = split(get_value(dayofweek, R, <<"*">>)),
        TimePeriod = #timeperiod{
            id = get_value(id, R),
            name = get_value(name, R),
            hour = Hour,
            dayofmonth = DayOfMonth,
            month = Month,
            dayofweek = DayOfWeek
        },
        ?INFO("~p", [TimePeriod]),
        ets:insert(timeperiod, TimePeriod)
    end, Records),
    {reply, ok, State};

handle_call(Req, _From, State) ->
    {reply, {badreq, Req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({schedule, #monitor_task{id = TaskId} = Task, Interval, Delay}, State) -> 
    case ets:lookup(monitor_task, TaskId) of
    [_] ->
        ?ERROR("Task has been scheduled: ~p", [Task]);
    [] ->
        NextSchedAt = extbif:timestamp() + Interval + Delay,
        %?INFO("schedule task: ~p, next_sched_at: ~p", [TaskId, NextSchedAt]),
        TRef = erlang:send_after((Interval + Delay) * 1000, self(), {run, TaskId}),
        ets:insert(monitor_task, Task#monitor_task{tref = TRef, next_sched_at = NextSchedAt})
    end,
    {noreply, State};

handle_cast({unschedule, TaskId}, State) ->
    case ets:lookup(monitor_task, TaskId) of
    [#monitor_task{tref = TRef} = _Task] ->
        cancel_timer(TRef),
        ets:delete(monitor_task, TaskId);
    [] ->
        ?WARNING("cannot find task: ~p", [TaskId])
    end,
    {noreply, State};

%%TODO: should refactor later.
handle_cast({reschedule, #monitor_task{id = TaskId} = Task, Interval}, State) ->
    case ets:lookup(monitor_task, TaskId) of
    [OldTask] -> %%TODO: task period, args maybe updated by user!!! so we should not update args and period!
        Args = OldTask#monitor_task.args,
        Period = OldTask#monitor_task.period,
        NextSchedAt = extbif:timestamp() + Interval,
        cancel_timer(OldTask#monitor_task.tref),
        TRef = erlang:send_after(Interval * 1000, self(), {run, TaskId}),
        ets:insert(monitor_task, Task#monitor_task{period = Period, args = Args, tref = TRef, next_sched_at = NextSchedAt});
    [] ->
        ?WARNING("cannot find task: ~p", [TaskId])
    end,
    {noreply, State};

handle_cast({update_period, TaskId, Period}, State) ->
    case ets:lookup(monitor_task, TaskId) of
    [Task] ->
        ets:insert(monitor_task, Task#monitor_task{period = Period});
    [] ->
        ?WARNING("cannot find task: ~p", [TaskId])
    end,
    {noreply, State};

handle_cast({update_args, TaskId, Args}, State) ->
    ?INFO("monitd_sched update :~p, ~p",[TaskId, Args]),
    case ets:lookup(monitor_task, TaskId) of
    [Task] ->
        ets:insert(monitor_task, Task#monitor_task{args = Args});
    [] ->
        ?WARNING("cannot find task: ~p", [TaskId])
    end,
    {noreply, State};

handle_cast(clear_tasks, State) ->
    clear_task(ets:first(monitor_task)),
    ets:delete_all_objects(monitor_task),
    {noreply, State};

handle_cast(Msg, State) ->
    ?ERROR("unexpected msg: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({run, TaskId}, State) ->
    case ets:lookup(monitor_task, TaskId) of
    [Task] -> 
        spawn(fun() ->
            case in_timeperiod(Task) of
            true ->
                run_task(Task);
            false ->
                skip_task(Task)
            end
        end);
    [] ->
        ?WARNING("task '~p' is deleted", [TaskId])
    end,
    {noreply, State};

handle_info(introspect, State) ->
    Size = ets:info(monitor_task, size),
    {_, QueueLen} = process_info(self(), message_queue_len),
    if
    QueueLen > 5 ->
        ?WARNING("MonitorTask: size=~p, queue_len: ~p", [Size, QueueLen]);
    true ->
        ok
    end,
    erlang:send_after(300 * 1000, self(), introspect),
    {noreply, State};
    
handle_info(Info, State) ->
    ?ERROR("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(monitor_task),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

run_task(#monitor_task{id = Id, period = Period, next_sched_at = NextSchedAt} = Task) ->
    SchedTime = extbif:timestamp(),
    try monitor_task:run(Task) of
    {ok, NewTask} ->
        FinishTime = extbif:timestamp(),
        Duration = FinishTime - SchedTime,
        Latency = SchedTime - NextSchedAt,
        %?WARNING("sched latency: ~p", [Latency]),
        Interval = Period - Duration,
        Interval1 = 
        if 
        Interval =< 0 ->
            ?WARNING("duration is longer than period: ~p, taskid: ~p", [Duration, Id]),
            Period;
        Interval =< 30 ->
            Period;
        Interval < 60 ->
            ?WARNING("interval is too short: ~p, duration: ~p, taskid: ~p", [Interval, Duration, Id]),
            Interval;
        true ->
            Interval
        end,
        NewTask1 = NewTask#monitor_task{duration = Duration, latency = Latency, last_sched_at = SchedTime},
        reschedule(NewTask1, Interval1)
    catch
    _:Error ->
        ?ERROR("monitd_sched_catch : ~p ~n Task: ~p ~n ~p", [Error, Task, erlang:get_stacktrace()])
    end.

skip_task(#monitor_task{period=Period} = Task) ->
    reschedule(Task, Period).

clear_task('$end_of_table') ->
    ok;

clear_task(TaskId) -> 
    case ets:lookup(monitor_task, TaskId) of
    [#monitor_task{tref=TRef} = _Task] -> 
        cancel_timer(TRef);
    [] -> 
        ok
    end,
    clear_task(ets:next(monitor_task, TaskId)).

cancel_timer(undefined) ->
    ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

%=========================================================
% Timeperiod
%=========================================================
in_timeperiod(#monitor_task{node=#node{tpid = undefined}}) ->
    true;
in_timeperiod(#monitor_task{node = Node}) ->
    case ets:lookup(timeperiod, Node#node.tpid) of
    [] -> true;
    [TP] ->
        date_in_timeperiod(TP) and time_in_timeperiod(TP)
    end.

date_in_timeperiod(#timeperiod{dayofmonth=DayOfMonth, 
    month=Month, dayofweek=DayOfWeek}) ->
    {_Y, M ,D} = date(),
    Day = calendar:day_of_the_week(date()),
    Day1 =
    if
    Day == 7 -> 0;
    true -> Day
    end,
    in_(M, Month) and in_(D, DayOfMonth) and in_(Day1, DayOfWeek).

time_in_timeperiod(#timeperiod{hour=Hour}) ->
    {H, _M, _S} = time(),
    in_(H, Hour).

in_(_I, ['*']) -> true;
in_(I, L) -> lists:member(I, L).


split(B) when is_binary(B) ->
    L = string:tokens(binary_to_list(B), ","),
    lists:map(
        fun("*") -> '*';
           (S) -> list_to_integer(S)
    end, L).

