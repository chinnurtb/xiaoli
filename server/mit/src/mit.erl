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

-record(state, {}).

-define(AREA_SQL, 
    "select t.id, t.rdn, t.name, t.alias, t.area_type as type, "
    "t2.rdn as parent "
    "from areas t "
    "left join areas t2 on t.parent_id = t2.id;").

-define(NODE_SQL, 
    "select t.id, t.rdn, t.name, t.alias, t.addr as ip, t.area_id, "
    "t1.name as category, "
    "t2.name as vendor, "
    "t3.name as model, "
    "t4.rdn as area, "
    "t4.cityid, "
    "t5.name as city "
    "from nodes t "
    "left join categories t1 on t1.id = t.category_id "
    "left join vendors t2 on t2.id = t.vendor_id "
    "left join models t3 on t3.id = t.model_id "
    "left join areas t4 on t4.id = t.area_id "
    "left join areas t5 on t5.id = t4.cityid;").

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

lookup(Rdn) when is_binary(Rdn) or is_list(Rdn) ->
    case mnesia:dirty_read(node, iolist_to_binary(Rdn)) of
    [Node] -> {ok, Node};
    [] -> {false, Rdn}
    end;

lookup({id, Id}) when is_integer(Id) ->
	index_lookup(Id, #node.id);

lookup({ip, Ip}) when is_binary(Ip) or is_list(Ip) ->
    index_lookup(Ip, #node.ip).

index_lookup(Idx, Pos) ->
    case mnesia:dirty_index_read(node, Idx, Pos) of
    [Node] -> 
        {ok, Node};
	[Node|_] = Nodes ->
		?ERROR("more than one nodes: ~p", [Nodes]),
		{ok, Node};
    [] ->
        {false, Idx}
    end.

children(ParentDn) when is_list(ParentDn) or is_binary(ParentDn) ->
	mnesia:dirty_index_read(node, ParentDn, #node.parent).

delete(Rdn) ->
	gen_server2:cast(?MODULE, {delete, Rdn}).

update(Node) when is_record(Node, node) ->
    gen_server2:cast(?MODULE, {update, Node}).

insert(Node) when is_record(Node, node) ->
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
		epgqueue:clear(mit),
        create_mit_areas(),
        create_mit_nodes(),
        load_mit_areas(),
        load_mit_nodes(),
        epgqueue:subscribe(mit, self());
    slave -> %slave node
        ok
    end,
    copy_table(area),
    copy_table(node),
    ?INFO_MSG("mit is started."),
    {ok, #state{}}.

copy_table(Table) ->
    mnesia:add_table_copy(Table, node(), ram_copies).

create_mit_areas() ->
    mnesia:create_table(area, [
        {ram_copies, [node()]}, 
        {index, [id, parent]}, 
        {attributes, record_info(fields, area)}]).

load_mit_areas() ->
    {ok, Areas} = epgsql:squery(main, ?AREA_SQL),
    lists:foreach(fun(A) -> 
        mnesia:dirty_write(#area{
            rdn = get_value(rdn, A),
            id = get_value(id, A),
            cityid = get_value(cityid, A),
            parent = get_value(parent, A),
            name = get_value(name, A),
            alias = get_value(alias, A),
            type = get_value(type, A)})
    end, Areas).

create_mit_nodes() ->
    mnesia:create_table(node, [
        {ram_copies, [node()]}, 
        {index, [id, ip]},
        {attributes, record_info(fields, node)}]).

load_mit_nodes() ->
    {ok, Nodes} = epgsql:squery(main, ?NODE_SQL),
    lists:foreach(fun(N) -> 
        mnesia:dirty_write(node, #node{
            rdn = get_value(rdn, N),
            id = get_value(id, N),
            ip = get_value(ip, N),
            cat = get_value(category, N),
            parent = get_value(parent, N),
            city = get_value(city, N),
            cityid = get_value(cityid, N),
            name = get_value(name, N),
            alias = get_value(alias, N),
            attrs = N})
    end, Nodes).

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
    Reply = [mnesia:table_info(node, all),
             mnesia:table_info(area, all)],
	{reply, Reply, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({insert, #node{rdn = Dn, ip = Ip} = Node}, State) ->
	mnesia:dirty_write(Node),
	mit_event:notify({inserted, Dn, Node}),
	{noreply, State};

handle_cast({update, #node{rdn = Dn, ip = Ip} = Node}, State) ->
	mnesia:dirty_write(Node),
	mit_event:notify({updated, Dn, Node}),
	{noreply, State};

handle_cast({delete, Dn}, State) ->
	mnesia:dirty_delete(node, Dn),
	mit_event:notify({deleted, Dn}),
	{noreply, State};

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(#pgqevent{name = <<"mit.inserted">>, data=Event}, State) ->
    ?INFO("mit inserted: ~p", [Event]),
    {noreply, State};

handle_info(#pgqevent{name = <<"mit.updated">>, data=Event}, State) ->
    ?INFO("mit updated: ~p", [Event]),
    {noreply, State};

handle_info(#pgqevent{name = <<"mit.deleted">>, data=Event}, State) ->
    %spawn(fun() -> on_mit_changes(Changes) end),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

on_mit_changes(Changes) ->
	Fun = fun(Change) -> 
        Dn = get_value(dn, Change),
        Oper = get_value(oper_type, Change),
		?INFO("~s is ~s", [Dn, opertext(Oper)]),
		try on_mit_change(Oper, Dn, Change)
		catch _:Err -> 
			?ERROR("on_mit_change error: ~p~n~p", [Err, Change])
		end
    end,
    lists:foreach(Fun, Changes).

on_mit_change(_, undefined, Change) ->
    ?ERROR("dn is undefined: ~p.", [Change]);

on_mit_change(?MIT_ADDED, Dn, Change) -> 
    DeviceType = get_value(device_type, Change),
    Entry = find_entry(DeviceType, Dn),
	insert(Entry);

on_mit_change(?MIT_UPDATED, Dn, Change) ->
    DeviceType = get_value(device_type, Change),
    Entry = find_entry(DeviceType, Dn),
	update(Entry);

on_mit_change(?MIT_DELETED, Dn, _Change) ->
    case lookup(Dn) of
	{ok, _Entry} ->
		delete(Dn);
    {false, _} ->
		?ERROR("cannot find ~s to delete.", [Dn])
    end;

on_mit_change(?MIT_MOVED, Dn, Change) ->
    OldDn = get_value(old_dn, Change),
	?ERROR("mit change moved: olddn = ~s, dn = ~s", [OldDn, Dn]),
    DeviceType = get_value(device_type, Change),
    Entry = find_entry(DeviceType, Dn),
	delete(OldDn),
	insert(Entry);

on_mit_change(Oper, Dn, _Change) ->
    ?ERROR("badoper: ~p ~p", [Oper, Dn]).

find_entry(1, Dn) ->
	case mit_ap:read({dn, Dn}) of
	{ok, [Record]} -> 
		mit_ap:entry(Record);
	{error, Reason} ->
		throw({error, Reason})
	end;

find_entry(2, Dn) ->
	case mit_sw:read({dn, Dn}) of
	{ok, [Record]} ->
		mit_sw:entry(Record);
	{error, Reason} ->
		throw({error, Reason})
	end;

find_entry(3, Dn) ->
	case mit_ac:read({dn, Dn}) of
	{ok, [Record]} -> 
		mit_ac:entry(Record);
	{error, Reason} ->
		throw({error, Reason})
	end.


opertext(1) -> "added.";
opertext(2) -> "deleted.";
opertext(3) -> "updated.";
opertext(4) -> "moved";
opertext(_) -> "badoper".
