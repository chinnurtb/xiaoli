%%%----------------------------------------------------------------------
%%% File    : mit.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : distributed management information tree
%%% Created : 21 Feb 2008
%%% Updated : 07 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [
		get_value/2,
		get_value/3]).

-export([start_link/0,
		stop/0]). 

-export([bdn/1,
		rdn/1]).

-export([device_type/1]).

-export([entry/2,
		alldn/0,
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

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

info() ->
	gen_server2:call(?MODULE, info).

%% @spec () -> ok
%% @doc Stop the mit server.
stop() ->
	gen_server2:cast(?MODULE, stop).

bdn(Dn) ->
    mit_dn:bdn(Dn).

rdn(Dn) ->
    mit_dn:rdn(Dn).

device_type(ap) -> 1;
device_type(fitap) -> 1;
device_type(fatap) -> 1;
device_type(<<"/top/ossIpDevice/ossWirelessAccessPoint">>) -> 1 ;
device_type(sw) -> 2;
device_type(<<"/top/ossIpDevice/ossIpSwitch">>) -> 2 ;
device_type(ac) -> 3;
device_type(<<"/top/ossIpDevice/ossWirelessAccessController">>) -> 3 ;
device_type(Type)  -> ?ERROR("bad_device_type: ~p", [Type]), 0.

alldn() ->
	mnesia:dirty_all_keys(entry).

lookup(Dn) when is_binary(Dn) or is_list(Dn) ->
    case mnesia:dirty_read(entry, iolist_to_binary(Dn)) of
    [Entry] -> {ok, Entry};
    [] -> {false, Dn}
    end;

lookup({ip, Ip}) when is_binary(Ip) or is_list(Ip) ->
    case mnesia:dirty_read(ip2dn, iolist_to_binary(Ip)) of
    [#ip2dn{dn =Dn}] -> lookup(Dn);
    [] -> {false, {ip, Ip}}
    end;

lookup({fitap, ApId}) when is_binary(ApId) or is_list(ApId) ->
	lookup({cn, {fitap, iolist_to_binary(ApId)}});

lookup({cn, Cn}) when is_tuple(Cn) ->
	index_lookup(Cn, #entry.cn);

lookup({uid, Uid}) when is_tuple(Uid)->
	index_lookup(Uid, #entry.uid).

index_lookup(Idx, Pos) ->
    case mnesia:dirty_index_read(entry, Idx, Pos) of
    [Entry] -> 
        {ok, Entry};
	[Entry|_] = Entries ->
		?ERROR("more than one entries: ~p", [Entries]),
		{ok, Entry};
    [] ->
        {false, Idx}
    end.

children(ParentDn) when is_list(ParentDn) or is_binary(ParentDn) ->
	mnesia:dirty_index_read(entry, ParentDn, #entry.parent).

delete(Dn) ->
	gen_server2:cast(?MODULE, {delete, Dn}).

update(Entry) when is_record(Entry, entry) ->
    gen_server2:cast(?MODULE, {update, Entry}).

insert(Entry) when is_record(Entry, entry) ->
    gen_server2:cast(?MODULE, {insert, Entry}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    case mnesia:system_info(extra_db_nodes) of
    [] -> %master node
        %clear change logs
		emysql:delete(mit_devices_changed),

        mnesia:create_table(entry, [{ram_copies, [node()]}, 
			{index, [uid, cn, parent]}, 
            {attributes, record_info(fields, entry)}]),

        mnesia:create_table(ip2dn, [{ram_copies, [node()]}, 
            {attributes, record_info(fields, ip2dn)}]),

		BootSteps = [{Name, Mod, Fun, Descr, Dep} 
						|| {Mod, [{Name, Fun, Descr, Dep}]} 
							<- extlib:module_with_attrs(mit, mit_boot_load)],

		[put({boot_step, element(1, Step)}, Step) || Step <- BootSteps],

		[boot_load_step(Step) || Step <- BootSteps],

        erlang:send_after(60 * 1000, self(), sync_mit_changes);
    _ -> %slave node
        ok
    end,
    mnesia:add_table_copy(entry, node(), ram_copies),
	%used by trap for example
    mnesia:add_table_copy(ip2dn, node(), ram_copies),
    ?INFO_MSG("mit is started."),
    {ok, #state{}}.

boot_load_step({Name, Mod, Fun, Descr, Dep}) ->
	case get({boot_load, Name}) of
	true ->
		ok;
	_ ->
		DepLoaded = get({boot_load, Dep}),
		if
		Dep =:= undefined ->
			ok;
		DepLoaded ->
			ok;
		true ->
			boot_load_step(get({boot_step, Dep}))
		end,
		?INFO("~s", [Descr]),
		Mod:Fun(),
		?INFO_MSG("done."),
		put({boot_load, Name}, true)
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
	Info = get(),
	{reply, Info, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({insert, #entry{dn = Dn, ip = Ip} = Entry}, State) ->
    if
    Ip == undefined -> ignore;
    true -> mnesia:dirty_write(ip2dn, #ip2dn{ip = Ip, dn = Dn})
    end,
	mnesia:dirty_write(Entry),
	mit_event:notify({inserted, Dn, Entry}),
	{noreply, State};

handle_cast({update, #entry{dn = Dn, ip = Ip} = Entry}, State) ->
    if
    Ip == undefined -> ignore;
    true -> mnesia:dirty_write(ip2dn, #ip2dn{ip = Ip, dn = Dn})
    end,
	mnesia:dirty_write(Entry),
	mit_event:notify({updated, Dn, Entry}),
	{noreply, State};

handle_cast({delete, Dn}, State) ->
    case lookup(Dn) of
    {ok, #entry{ip = undefined}} ->
        ingore;
    {ok, #entry{ip = Ip}} ->
        mnesia:dirty_delete(ip2dn, Ip);
    {false, _} ->
        ?WARNING("~p is not found", [Dn])
    end,
	mnesia:dirty_delete(entry, Dn),
	mit_event:notify({deleted, Dn}),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(sync_mit_changes, State) ->
	{ok, Changes} = emysql:select(mit_devices_changed),
    spawn(fun() -> on_mit_changes(Changes) end),
	Ids = [get_value(id, Change) || Change <- Changes],
	emysql:delete(mit_devices_changed, {'in', id, Ids}),
	erlang:send_after(5 * 1000, self(), sync_mit_changes),
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
		[delete(Child) || #entry{dn = Child} <- children(Dn)],
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

find_entry(?MIT_AP, Dn) ->
	case mit_ap:read({dn, Dn}) of
	{ok, [Record]} -> 
		mit_ap:entry(Record);
	{error, Reason} ->
		throw({error, Reason})
	end;

find_entry(?MIT_SW, Dn) ->
	case mit_sw:read({dn, Dn}) of
	{ok, [Record]} ->
		mit_sw:entry(Record);
	{error, Reason} ->
		throw({error, Reason})
	end;

find_entry(?MIT_AC, Dn) ->
	case mit_ac:read({dn, Dn}) of
	{ok, [Record]} -> 
		mit_ac:entry(Record);
	{error, Reason} ->
		throw({error, Reason})
	end;

find_entry(?MIT_OMC, Dn) ->
	case mit_omc:read({dn, Dn}) of
	{ok, [Record]} -> 
		mit_omc:entry(Record);
	{error, Reason} ->
		throw({error, Reason})
	end.

entry(fitap, Record) ->
    {value, AcId} = dataset:get_value(ac_id, Record),
	{ok, #entry{dn = Dn}} = lookup({uid, {ac, AcId}}),
	Entry = entry2(fitap, Record),
	Entry#entry{parent = Dn, ip = undefined};

entry(Type, Record) ->
	entry2(Type, Record).
		
entry2(Type, Record) ->
    {value, Id} = dataset:get_value(id, Record),
    {value, Dn} = dataset:get_value(dn, Record),
    {value, Ip} = dataset:get_value(ip, Record),
    {value, Cn} = dataset:get_value(cn, Record),
    {value, Text} = dataset:get_value(text, Record, <<"">>),
    {value, Class} = dataset:get_value(objectClass, Record),
    {value, OperState} = dataset:get_value(oper_state, Record),
    #entry{dn = Dn,
		  cn = {Type, Cn},
		  uid = {Type, Id},
		  ip = Ip,
		  text = Text,
		  class = Class,
		  site = bdn(Dn),
          oper_state = OperState,
		  attrs = Record}.

opertext(1) -> "added.";
opertext(2) -> "deleted.";
opertext(3) -> "updated.";
opertext(4) -> "moved";
opertext(_) -> "badoper".
