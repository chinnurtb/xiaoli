%%%----------------------------------------------------------------------
%%% File    : monitd_coord.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : monitd coordinator
%%% Created : 27 Dec 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(monitd_coord).

-author('ery.lee@gmail.com').

-include("monitor.hrl").

-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1, appvsn/0]).

-import(erlang, [send_after/3]).

-export([start_link/0]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        prioritise_info/2,
        terminate/2, 
        code_change/3 ]).

-record(monitor_mod, {node_class, node_exp, monitors, mib}).

-record(state, {shard, queue, channel, monitor_types}).

start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    chash_pg:create(?MODULE),
    chash_pg:join(?MODULE, self()),
    ets:new(monitor_mod, [bag, named_table, protected, {keypos, 2}]),
    ets:new(monitor_table, [bag, named_table, protected]),
    ?INFO_MSG("monitd_coord is started...[ok]"),
    {ok, #state{}}.
    
monitor_mod(Record) ->
    monitor_mod(Record, #monitor_mod{}).

monitor_mod([], MonitorMod) ->
    MonitorMod;

monitor_mod([{node_class, NodeClass}|T], MonitorMod) ->
    monitor_mod(T, MonitorMod#monitor_mod{node_class = NodeClass});

monitor_mod([{node_attrs, S}|T], MonitorMod) ->
    NodeExp =
    case S of
    <<"all">> -> 
        all;
    _ -> 
        {ok, Exp} = prefix_exp:parse(S),
        Exp
    end,
    monitor_mod(T, MonitorMod#monitor_mod{node_exp = NodeExp});

monitor_mod([{monitors, S}|T], MonitorMod) ->
    Tokens = string:tokens(binary_to_list(S), ","),
    Monitors = 
    lists:map(fun(Token) -> 
        [Mod, Period] = string:tokens(Token, "="),
        {list_to_atom(Mod), list_to_integer(Period)}
    end, Tokens), 
    monitor_mod(T, MonitorMod#monitor_mod{monitors = Monitors});

monitor_mod([{mib, S}|T], MonitorMod) ->
   Mib = list_to_atom(binary_to_list(S)), 
   monitor_mod(T, MonitorMod#monitor_mod{mib = Mib});

monitor_mod([_|T], MonitorMod) ->
    monitor_mod(T, MonitorMod).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
	Error = {badreq, Req},
    {stop, {error, Error}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}, 
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({monitor, Dn, Entry}, State) ->
	sched_monitors(Dn, Entry, State),
    {noreply, State};

handle_info({disco, update, Dn, Entry}, State)  ->
	disco_server:update(Dn, Entry),
    {noreply, State};

handle_info({monitor, update, Dn, Entry}, State) ->
	update_monitors({Dn, Entry}, State),
    {noreply, State};

handle_info({unmonitor, Dn}, State) ->
	unmonitor(Dn),
    {noreply, State};

handle_info({discover, Dn, Entry}, State) ->
	disco_server:discover(Dn, Entry),
    {noreply, State};

handle_info({auto_discover, Dn, Task}, State) ->
	disco_server:auto_discover(Dn, Task),
    {noreply, State};

handle_info({undiscover, Dn}, State) ->
	disco_server:undiscover(Dn),
    {noreply, State};

handle_info({reset, Dn, Entry}, State) ->
	disco_server:discover(Dn, Entry),
    {noreply, State};

handle_info({'db.mib_oids', Oids}, State) ->
    mib_registry:initialize({mib_oids, Oids}),
    {noreply, State};

handle_info({'db.metrics', Metrics}, State) ->
    monitd_hub:initialize({metrics, Metrics}),
    {noreply, State};

handle_info({'db.disco_mods', Mods}, State) ->
    disco_server:initialize({disco_mods, Mods}),
    {noreply, State};

handle_info({'db.monitor_mods', Mods}, State) ->
    ?INFO("initialize monitor_mods: ~p", [length(Mods)]),
    [ets:insert(monitor_mod, monitor_mod(Mod)) || Mod <- Mods],
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

prioritise_info({'db.metrics', _}, _State) ->
    9;

prioritise_info(_, _State) ->
    0.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_monitors({Dn, Entry}, State) ->
    sched_monitors(Dn, Entry, State).

sched_monitors(Dn, Entry, _State) ->
    {value, ObjectClass} = dataset:get_value(objectClass, Entry),
    ObjectClasses = [Class || Class <- binary:split(ObjectClass, <<"/">>, [global])],
    MonitorMods = lists:flatten([ets:lookup(monitor_mod, Class) || Class <- ObjectClasses]),
    Mods = find_monitors(attrs(Entry), MonitorMods),
    lists:foreach(fun(#monitor_mod{mib = Mib, monitors = Monitors}) ->
        Args = [{mib, Mib}|Entry],
        lists:foreach(fun({Module, Period}) -> 
            TaskId = "mod=" ++ atom_to_list(Module) ++ "," ++ binary_to_list(Dn),
            case ets:lookup(monitor_task, TaskId) of
            [_Task] ->
                monitd_sched:update_task(TaskId, Args);
            [] ->
                Task = #monitor_task{id = TaskId, period = Period * 60, mod = Module, args = Args, handler = fun monitd_hub:emit/1, created_at = extbif:timestamp()},
                monitd_sched:schedule(Task, Period * 60, random:uniform(Period * 60)),
                ets:insert(monitor_table, {Dn, Module})
            end
        end, Monitors)
    end, Mods).

find_monitors(Attrs, Mods) ->
    find_monitors(Attrs, Mods, []).

find_monitors(_Attrs, [], Acc) ->
    lists:flatten(Acc);

find_monitors(Attrs, [#monitor_mod{node_exp = all} = Mod | Mods], Acc) ->
    find_monitors(Attrs, Mods, [Mod|Acc]);

find_monitors(Attrs, [#monitor_mod{node_exp = Exp} = Mod | Mods], Acc) ->
    case prefix_exp:eval(Exp, Attrs) of
    true -> find_monitors(Attrs, Mods, [Mod|Acc]);
    false -> find_monitors(Attrs, Mods, Acc)
    end.
    
attrs(Entry) ->
    attrs(Entry, []).
attrs([], Attrs) ->
    Attrs;
attrs([{N,V}|Entry], Attrs) when is_binary(V) ->
    attrs(Entry, [{N, binary_to_list(V)}|Attrs]);
attrs([{N,V}|Entry], Attrs) ->
    attrs(Entry, [{N,V}|Attrs]).

unmonitor(Dn) ->
    %find monitors of this entry
    case ets:lookup(monitor_table, Dn) of
    [] ->
        ok;
    Mods ->
        lists:foreach(fun({_Dn, Mod}) ->
            TaskId = "mod=" ++ atom_to_list(Mod) ++ "," ++ binary_to_list(Dn),
            monitd_sched:unschedule(TaskId),
            ets:delete(monitor_table, Dn)
        end, Mods)
    end.

