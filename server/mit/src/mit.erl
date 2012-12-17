%%%----------------------------------------------------------------------
%%% File    : mit.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : management information tree
%%% Created : 21 Feb 2008
%%% Updated : 07 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-include_lib("epgqueue/include/pgqueue.hrl").

-import(proplists, [
		get_value/2,
		get_value/3]).

-export([start_link/0,
        mode/0]). 

-export([alldn/0,
		lookup/1,
		children/1,
		insert/1,
        update/1,
        delete/1]).

%debug method
-export([info/0]).

-behavior(gen_server).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(state, {modules}).

start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

mode() -> 
    case mnesia:system_info(extra_db_nodes) of
    [] -> master;
    _ -> slave
    end.

info() ->
	gen_server2:call(?MODULE, info).

alldn() ->
	mnesia:dirty_all_keys(node).

lookup(Dn) when is_binary(Dn) or is_list(Dn) ->
    case mnesia:dirty_read(node, iolist_to_binary(Dn)) of
    [Node] -> {ok, Node};
    [] -> {false, Dn}
    end;

lookup({id, Id}) when is_integer(Id) ->
	index_lookup(Id, #mit_node.id);

lookup({ip, Ip}) when is_binary(Ip) or is_list(Ip) ->
    index_lookup(iolist_to_binary(Ip), #mit_node.ip).

index_lookup(Idx, Pos) ->
    case mnesia:dirty_index_read(mit_node, Idx, Pos) of
    [Node] ->
        {ok, Node};
	[Node|_] = Nodes ->
		?ERROR("more than one nodes: ~p", [Nodes]),
		{ok, Node};
    [] ->
        {false, Idx}
    end.

children(ParentDn) when is_list(ParentDn) or is_binary(ParentDn) ->
	mnesia:dirty_index_read(mit_node, iolist_to_binary(ParentDn), #mit_node.parent).

delete(Dn) ->
	gen_server2:cast(?MODULE, {delete, Dn}).

update(Node) when is_record(Node, mit_node) ->
    gen_server2:cast(?MODULE, {update, Node}).

insert(Node) when is_record(Node, mit_node) ->
    gen_server2:cast(?MODULE, {insert, Node}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case mode() of
    master -> %master node
        %clear mit queue
		epgqueue:clear('mit.node'),
        mnesia:create_table(mit_node, [
            {ram_copies, [node()]}, {index, [id, ip]},
            {attributes, record_info(fields, mit_node)}]),
        {ok, Modules} = application:get_env(modules),
        lists:foreach(fun({Cat, Mod}) ->
            {ok, Nodes} = Mod:load(),
            ?INFO("load ~p ~p...", [length(Nodes), Cat]),
            [mnesia:dirty_write(N) || N <- Nodes]
        end, Modules),
        epgqueue:subscribe('mit.node', self()),
        ?INFO_MSG("mit(master) is started...[ok]"),
        {ok, #state{}};
    slave -> %slave node
        mnesia:add_table_copy(mit_node, node(), ram_copies),
        ?INFO_MSG("mit(slave) is started...[ok]"),
        {ok, #state{}}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(info, _From, State) ->
	{reply, mnesia:table_info(mit_node, all), State};

handle_call(Req, _From, State) ->
    {stop, {badreq, Req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({insert, Node}, State) ->
    insert_node(Node),
	{noreply, State};

handle_cast({update, Node}, State) ->
    update_node(Node),
	{noreply, State};

handle_cast({delete, Dn}, State) ->
    delete_node(Dn),
	{noreply, State};

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(#pgqevent{name = <<"node.inserted">>, data=Dn}, State) ->
    ?INFO("node inserted: ~s", [Dn]),
    Mod = mit_meta:module(mit_node:cat(Dn)),
    case Mod:load(Dn) of
    {ok, [Node]} ->
        insert_node(Node);
    {error, Err} ->
        ?ERROR("~p", [Err])
    end,
    {noreply, State};

handle_info(#pgqevent{name = <<"node.updated">>, data=Dn}, State) ->
    ?INFO("node updated: ~s", [Dn]),
    Mod = mit_meta:module(mit_node:cat(Dn)),
    case Mod:load(Dn) of
    {ok, [Node]} ->
        update_node(Node);
    {error, Err} ->
        ?ERROR("~p", [Err])
    end,
    {noreply, State};

handle_info(#pgqevent{name = <<"node.deleted">>, data=Dn}, State) ->
    delete_node(Dn),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {badinfo, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

insert_node(Node) ->
	mnesia:dirty_write(Node),
	mit_event:notify(inserted, Node).

update_node(Node) ->
	mnesia:dirty_write(Node),
	mit_event:notify(updated, Node).

delete_node(Dn) ->
    case mnesia:dirty_read(mit_node, Dn) of
    [Node] -> 
        mnesia:dirty_delete(mit_node, Dn),
        mit_event:notify(deleted, Node);
    _ ->
        ignore
    end.


