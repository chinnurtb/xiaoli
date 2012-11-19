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

-author('ery.lee@gmail.com').

-include("coord.hrl").

-include_lib("elog/include/elog.hrl").

-include_lib("amqp_client/include/amqp.hrl").

-import(erlang, [send_after/3]).

-export([start_link/0, 
        presences/0,
        status/0]).

-behavior(gen_server).

%%callback
-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        prioritise_info/2,
        terminate/2, 
        code_change/3]).

-record(state, {channel}).

-define(TIMEOUT, 600000).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

status() ->
    gen_server2:call(?MODULE, status).

presences() ->
    gen_server2:call(?MODULE, presences).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    mnesia:create_table(presence, [
        {ram_copies, [node()]}, {index, [type]},
        {attributes, record_info(fields, presence)}]),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    State = #state{channel = Channel},
    handle_info(ping, State),
    ?INFO_MSG("coord is started...[ok]"),
    {ok, State}.

open(Conn) ->
	{ok, Channel} = amqp:open_channel(Conn),
	amqp:queue(Channel,<<"host">>),
	amqp:queue(Channel,<<"presence">>),
	amqp:queue(Channel,<<"heartbeat">>),
	amqp:topic(Channel,<<"sys.watch">>),

	amqp:consume(Channel, <<"host">>),
	amqp:consume(Channel, <<"presence">>),
	amqp:consume(Channel, <<"heartbeat">>),

	Channel.

handle_call(presences, _From, State) ->
    Nodes = mnesia:dirty_all_keys(presence),
    Reply = lists:flatten([mnesia:dirty_read(presence, N) || N <- Nodes]),
    {reply, Reply, State};

handle_call(status, _From, State) ->
    %Reply = [{dispatch, mnesia:table_info(dispatch, size)}],
    {reply, {ok, []}, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(ping, #state{channel = Channel} = State) ->
	amqp:publish(Channel, <<"sys.watch">>, <<"ping">>, <<"ping">>),
    send_after(60000, self(), ping),
	{noreply, State};

handle_info({deliver, <<"host">>, _, Payload}, State) ->
    case binary_to_term(Payload) of
    {host, HostInfo} ->
        DateTime = {datetime, {date(), time()}},
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
	{Node, Type, Status, Vsn, Summary} ->
		handle_presence({Node, Type, Status, Summary}),
        case mnesia:dirty_read(presence, Node) of
        [Presence] ->
            cancel_timer(Presence#presence.tref);
        [] ->
            ok
        end,
        Tref = send_after(?TIMEOUT, self(), {offline, Node}),
        mnesia:dirty_write(#presence{node = Node, type = Type, 
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

handle_info({offline, Node}, State) ->
    ?ERROR("~s is offline", [Node]),
    case mnesia:dirty_read(presence, Node) of
    [Presence] ->
        coord_dist:offline(Presence),
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
    DateTime = {datetime, {date(), time()}},
    epgsql:update(main, servers, [{presence, 0}, {updated_at, DateTime}], {jid, Node});

handle_presence({Node, node, available, _Summary}) ->
    DateTime = {datetime, {date(), time()}},
    epgsql:update(main, servers, [{presence, 1}, {updated_at, DateTime}], {jid, Node});

handle_presence(_) ->
	ignore.

handle_offline(#presence{node = Node, type = node}) ->
    DateTime = {datetime, {date(), time()}},
    epgsql:update(main, servers, [{presence, 0}, {updated_at, DateTime}], {jid, Node});

handle_offline(_Presence) ->
    ignore.

