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


-include("mit.hrl").

-include("monitor.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

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

-record(monitor, {category, vendor, sysoid, match, mib, module, period, retries, timeout}).

-record(state, {shard, queue, channel, monitors}).

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
    ?INFO_MSG("monitd_coord is started...[ok]"),
    {ok, #state{}}.
    
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
handle_info({monitor, #node{dn=Dn}=Node}, State) ->
    ?INFO("sched to monitor ~s", [Dn]),
    monitd_disco:discover(Node),
	sched_monitors(Node, State),
    {noreply, State};

handle_info({update, #node{dn=Dn}=Node}, State)  ->
    ?INFO("update node: ~s", [Dn]),
	monitd_disco:update(Node),
	update_monitors(Node, State),
    {noreply, State};

handle_info({unmonitor, Dn}, State) ->
	unmonitor(Dn),
    {noreply, State};

handle_info({discover, Node}, State) ->
	monitd_disco:discover(Node),
    {noreply, State};

handle_info({undiscover, Dn}, State) ->
	monitd_disco:undiscover(Dn),
    {noreply, State};

handle_info({miboids, Oids}, State) ->
    mib_registry:setup({miboids, Oids}),
    {noreply, State};

handle_info({metrics, Metrics}, State) ->
    monitd_hub:setup({metrics, Metrics}),
    {noreply, State};

handle_info({sysoids, Mods}, State) ->
    monitd_disco:setup({sysoids, Mods}),
    {noreply, State};

handle_info({dict_status, Records}, State) ->
    monitd_disco:setup({dict_status, Records}),
    {noreply, State};

handle_info({timeperiods, Records}, State) ->
    monitd_sched:setup({timeperiods, Records}),
    {noreply, State};

handle_info({monitors, Records}, State) ->
    Monitors = [#monitor{
                category=b2a(get_value(category, R)),
                vendor=b2a(get_value(vendor, R)),
                sysoid=get_value(sysoid, R),
                match=matchexp(get_value(match, R)),
                mib=b2a(get_value(mib, R)),
                module=b2a(get_value(module, R)),
                period=get_value(period, R),
                retries=get_value(retries, R),
                timeout=get_value(timeout, R)} 
    || R <- Records],
    ?INFO("setup monitors: ~p", [length(Monitors)]),
    {noreply, State#state{monitors=Monitors}};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

prioritise_info({metrics, _}, _State) ->
    9;

prioritise_info(_, _State) ->
    0.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_monitors(Node, State) ->
    sched_monitors(Node, State).

sched_monitors(#node{dn=Dn}=Node, #state{monitors=Monitors}) ->
    MonitorsOfNode = find_match(Node, Monitors),
    lists:foreach(fun(#monitor{mib = Mib, module=Mod, period=Period, retries=Retries, timeout=Timeout}) ->
        Args = [{mib, Mib},{period, Period},{retries, Retries},{timeout, Timeout}],
        TaskId = "mod=" ++ atom_to_list(Mod) ++ "," ++ binary_to_list(Dn),
        case monitd_sched:lookup(TaskId) of
        [_Task] ->
            monitd_sched:update_task(TaskId, {Node, Args});
        [] ->
            Task = #monitor_task{id = TaskId, dn=Dn, 
                period = Period * 60, mod = Mod, 
                node=Node, args = Args,
                handler = fun monitd_hub:emit/1,
                created_at = extbif:timestamp()},
            monitd_sched:schedule(Task, Period * 60, random:uniform(Period * 60))
        end
    end, MonitorsOfNode).

find_match(_Node, Monitors) ->
    Monitors.

%find_monitors(Attrs, [#monitor_mod{node_exp = Exp} = Mod | Mods], Acc) ->
%    case prefix_exp:eval(Exp, Attrs) of
%    true -> find_monitors(Attrs, Mods, [Mod|Acc]);
%    false -> find_monitors(Attrs, Mods, Acc)
%    end.
    
unmonitor(Dn) ->
    %find monitors of this entry
    monitd_sched:unschedule({dn, Dn}). 

matchexp(undefined) -> 
    undefined;
matchexp(<<>>) ->
    undefined;
matchexp(S) -> 
    {ok, Exp} = prefix_exp:parse(S),
    Exp.

b2a(undefined) ->
    undefined;
b2a(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B)).


