%%%----------------------------------------------------------------------
%%% File    : disco_server.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Discovery task executor.
%%% Created : 22 Oct. 2008
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2010, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(disco_server).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-include("mib/rfc1213.hrl").

-import(extbif, [to_list/1]).

-behavior(gen_server).

%%api
-export([start_link/0, 
		initialize/1,
        status/0,
        discover/2,
        undiscover/1,
        update/2,
        auto_discover/2,
        report/1]).

%%callback
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-record(disco_mod, {sysoid, category, 
    vendor, model, module, mib}).

%%--------------------------------------------------------------------
%% @spec start_link() ->  Result
%%   Result = {ok,Pid} | ignore | {error,Error}
%% @doc calls gen_server:start_link
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

initialize({disco_mods, Mods}) ->
	gen_server:cast(?MODULE, {initialize, disco_mods, Mods}).

status() ->
    gen_server:call(?MODULE, status).

discover(Dn, Entry) ->
    gen_server:cast(?MODULE, {discover, Dn, Entry}).

undiscover(Dn) ->
    gen_server:cast(?MODULE, {undiscover, Dn}).

update(Dn, Entry) ->
    gen_server:cast(?MODULE, {update, Dn, Entry}).

auto_discover(Dn, Task) ->
    gen_server:cast(?MODULE, {auto_discover, Dn, Task}).
report({failure, Dn, Entry}) -> 
    gen_server:cast(?MODULE, {failure, Dn, Entry});

report({success, Dn, EntryList}) ->
    gen_server:cast(?MODULE, {success, Dn, EntryList}).

%%--------------------------------------------------------------------
%% @hidden
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    ets:new(disco_mod, [set, named_table, protected, {keypos, 2}]),
    ets:new(disco_table, [set, named_table]),
    ets:new(failed_disco_table, [set, named_table]),
    erlang:send_after(1000, self(), execute),
    ?INFO_MSG("disco_server is starting...[ok]"),
    {ok, state}.

handle_call(status, _From, State) ->
    Reply = [{pending, ets:info(disco_table, size)},
        {failed, ets:info(failed_disco_table, size)}],
    {reply, Reply, State};
    
handle_call(Req, _From, State) ->
    ?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

handle_cast({initialize, disco_mods, Records}, State) ->
    ?INFO("initialize disco_mods: ~p", [length(Records)]),
    [ets:insert(disco_mod, disco_mod(Record)) || Record <- Records],
    {noreply, State};

handle_cast({discover, Dn, Entry}, State) ->
    ets:insert(disco_table, {Dn, Entry}),
    {noreply, State};

handle_cast({update, Dn, Entry}, State) ->
    case ets:lookup(disco_table, Dn) of
    [{Dn, _}] -> 
        ets:insert(disco_table, {Dn, Entry});
    [] -> 
        ok
    end,
    case ets:lookup(failed_disco_table, Dn) of
    [{Dn, _}] -> 
        ets:insert(failed_disco_table, {Dn, Entry});
    [] -> 
        ok
    end,
    {noreply, State};

handle_cast({auto_discover, Dn, Task}, State) ->
    {value, IpStart} = dataset:get_value(ip_start, Task),
    {value, IpEnd} = dataset:get_value(ip_end, Task),
    {value, Community0} = dataset:get_value(snmp_r, Task),
    Community = to_list(Community0),
    IpStart1 = ip2int(to_list(IpStart)),
    IpEnd1 = ip2int(to_list(IpEnd)),
    ?INFO("autodisco: ~p ~p ~p", [IpStart, IpEnd, Community]),
    spawn(fun() -> 
        auto_discover(Dn, IpStart1, IpEnd1, Community, Task)
    end),
    {noreply, State};

handle_cast({undiscover, Dn}, State) ->
    ets:delete(disco_table, Dn),
    ets:delete(failed_disco_table, Dn),
    {noreply, State};

handle_cast({failure, Dn, Entry}, State) ->
    ets:insert(failed_disco_table, {Dn, Entry}),
    {noreply, State};

handle_cast({success, Dn, EntryList}, State) ->
    ets:delete(failed_disco_table, Dn),
    monitd_hub:emit(EntryList),
    {noreply, State};

handle_cast(Msg, State) ->
    ?ERROR("badmsg: ~p", [Msg]),
    {noreply, State}.

handle_info(execute, State) ->
    First = ets:first(disco_table),
    case First of
    '$end_of_table' ->
        pass;
     Dn ->
        [{_Dn, Entry}] = ets:lookup(disco_table, Dn),
        spawn(fun() -> execute({Dn, Entry}) end),
        ets:delete(disco_table, Dn)
    end,
    erlang:send_after(1000, self(), execute),
    {noreply, State};

handle_info(rediscovery, State) ->
    ?INFO_MSG("begin to rediscovery..."),
    AgentPid = self(),
    spawn(fun() -> 
            rediscover(ets:first(disco_table)),
            erlang:send_after(15*60*1000, AgentPid, rediscovery)
          end),
    {noreply, State};

handle_info(Info, State) ->
	?ERROR("badinfo: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

rediscover('$end_of_table') ->
    ok;

rediscover(Dn) ->
    [{Dn, Entry}] = ets:lookup(disco_table, Dn),
    execute({Dn, Entry}),
    ?INFO("rediscovering ~s", [Dn]),
    rediscover(ets:next(disco_table, Dn)).

auto_discover(Dn, IpStart, IpEnd, Community, Task) ->
    {value, Id} = dataset:get_value(id, Task),
    StartTime = calendar:local_time(),
    L = pmap(fun(I) -> 
        Ip = int2ip(I),
        try auto_discover(Dn, Ip, Community) of
            {ok, Entry} ->
                {ok, Entry};
            {error, Error} ->
                {error, Error}
        catch 
            _:Error ->
                ?ERROR("~p", [Error]),
                {error, Error}
        end
    end, lists:seq(IpStart, IpEnd)),
    EndTime = calendar:local_time(),
    TaskRes = [{start_time, StartTime}, {end_time, EndTime}],
    monitd:task_reply({task_result, Id, TaskRes}),
	[monitd_hub:emit(Entry) || {ok, Entry} <- L].
    
%% {error, Reason} or {ok, Entry}
auto_discover(Dn, Ip, Community) ->
	case snmp_mapping:discovery(Ip, ?System, [{community, Community}]) of
	{ok, Values} ->
        {value, SysName} = dataset:get_value(sysName, Values),
        {value, SysDescr} = dataset:get_value(sysDescr, Values),
        {value, SysObjectID} = dataset:get_value(sysObjectID, Values),
        SysOid = mib_oid:new(SysObjectID),
        Attrs = [{sysdescr, SysDescr}, {sysname, SysName}, {sysoid, SysOid}],
		case find_mod(SysOid) of 
		[{_, Category, Vendor, Type, _Mod}] -> 
            {TypeStr, VendorStr, CategoryStr} = disco_worker:get_type(SysOid, Type, Vendor, Category),
			Attrs1 = [{ap_ip, Ip}, {ap_cn, SysName}, {discovery_state, 0}, {vendor, atom_to_list(VendorStr)}, {type, TypeStr} | Attrs],
			{ok, {autodisco, CategoryStr, "cn=" ++ Ip ++ "," ++ Dn, Attrs1}};
		[] ->
			?WARNING("No disco mod found for: ~p, ip: ~p, sysdescr: ~p", [SysOid, Ip, SysDescr]),
            {error, no_disco_mod};
		{error, Reason} ->
			{error, Reason}
		end;
	{error, Error} -> 
		{error, Error}
	end.

ip2int(Ip) ->
    [I1,I2,I3,I4] = [ list_to_integer(S) || S <- string:tokens(Ip, ".")],
    (I1 bsl 24) + (I2 bsl 16) + (I3 bsl 8) + I4.

int2ip(I) ->
    string:join([ integer_to_list(X band 16#FF) || X <- [(I bsr 24), (I bsr 16), (I bsr 8), I] ], ".").

pmap(F, L) ->
    Parent = self(),
    Pids = [spawn(fun() ->
                Parent ! {self(), F(X)}
            end) || X <- L],
    [receive {Pid, Res} -> Res end || Pid <- Pids].

to_oid_str(L) when is_list(L) ->
	string:join([integer_to_list(X) || X <- L], ".");

to_oid_str(_) ->
	"".

execute({Dn, Entry}) ->
  %%TODO:???
  {value, ApFit} = dataset:get_value(ap_fit, Entry, 0),
  {value, OperState} = dataset:get_value(oper_state, Entry, 2),
  {value, DiscoState} = dataset:get_value(discovery_state, Entry, 1),
  case {ApFit, OperState, DiscoState} of
    {_, 2, _} ->
		ignore;
    {_, _, 9} ->
		?ERROR("reset is not supported", []);	
    {1, _, _} ->
      case catch discover(fitap, Dn, Entry) of
        {ok, EntryList} ->
          report(Dn, EntryList);
        {error, Error} ->
          report({failure, Dn, Entry}),
          ?WARNING("Failed to discover ~p: ~p", [Dn, Error]);
        {'EXIT', Reason} ->
          ?ERROR("Exception occurred when discover ~p: ~n ~p ~n ~p", [Dn, Reason, Entry]);
        Throw ->
          ?ERROR("throw: ~p, dn: ~p", [Throw, Dn])
      end;
    _ ->
      case catch discover(entry, Dn, Entry) of
        {ok, EntryList} -> 
          report(Dn, EntryList);
        {error, Error} ->
          report({failure, Dn, Entry}),
          ?ERROR("Failed to discover ~p: ~p", [Dn, Error]);
        {'EXIT', Reason} ->
          ?ERROR("Exception occurred when discover ~p: ~n ~p ~n ~p", [Dn, Reason, Entry]);
        Throw ->
          ?ERROR("throw: ~p, dn: ~p", [Throw, Dn])
      end
  end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
discover(fitap, Dn, Entry) ->
	Ip = to_list(proplists:get_value(ip, Entry)),
	?INFO("begin to discover fitap ~s: ~s", [Dn, Ip]),
	Community = to_list(proplists:get_value(snmpCommunity, Entry, <<"public">>)),
	{SnmpStatus, _} = check_snmp:run(Ip, Community),
	discover(SnmpStatus, fitap, Dn, Ip, [{community, Community}], Entry);

discover(entry, Dn, Entry) ->
	Ip = to_list(proplists:get_value(ip, Entry)),
	?INFO("begin to discover ~s", [Ip]),

	{ok, AvailEvents, _} = check_avail:run(Entry),
	monitd_hub:emit(AvailEvents),

	Community = to_list(proplists:get_value(snmpCommunity, Entry, <<"public">>)),
	{SnmpStatus, _} = check_snmp:run(Ip, Community),

	Agent = [{community, Community}],
	discover(SnmpStatus, entry, Dn, Ip, Agent, Entry).

discover("SNMP OK", entry, Dn, Ip, AgentData, _Entry) ->
  case snmp_mapping:discovery(Ip, ?System, AgentData) of
    {ok, Values} ->
      {value, SysName} = dataset:get_value(sysName, Values),
      {value, SysDescr} = dataset:get_value(sysDescr, Values),
      {value, SysObjectID} = dataset:get_value(sysObjectID, Values),
      SysOid = to_oid_str(SysObjectID),
      Attrs = [{sysdescr, SysDescr}, {sysname, SysName}, {sysoid, SysOid}],
      case find_mod(list_to_binary(SysOid)) of 
        [#disco_mod{category=Category, vendor=Vendor, model=Type, module=Mod, mib=Mib}] -> 
          Attrs1 = [{ap_cn, SysName}, {discovery_state, 1}, {vendor, atom_to_list(Vendor)} | Attrs],
          {ok, SpecAttrs, DataList} = Mod:disco(Dn, Ip, AgentData, [{mib, Mib}]),
          ?INFO("DiscoMod: ~s , vendor: ~s, Ip: ~s",[Mod, Vendor, Ip]),
		  Attrs2 = 
		  case lists:keysearch(type, 1, SpecAttrs) of
		  {value, _} ->
			Attrs1 ++ SpecAttrs;
		  false ->
			[{type, atom_to_list(Type)} | Attrs1 ++ SpecAttrs]
		  end,
          %?INFO("Attrs2:~p, Ip:~p~n",[Attrs2, Ip]),
          {ok, [{entry, Category, Dn, Attrs2} | DataList]};
        [] ->
          ?ERROR("No disco mod for: ~s, ip: ~s, sysdescr: ~s", [SysOid, Ip, SysDescr]),
          {error, no_disco_mod};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Error} -> 
      {error, Error}
  end;

discover("SNMP OK", fitap, Dn, AcIp, Agent, Entry) ->
    {value, SysOid} = dataset:get_value(sysoid, Entry), 
	case find_mod(SysOid) of
	[#disco_mod{mib=Mib}] ->
		{ok, SpecAttrs, DataList} = disco_fitap:disco(Dn, AcIp, Agent, [{mib, Mib}|Entry]),
		{ok, [{entry, fitap, Dn, [{discovery_state, 1}|SpecAttrs]}|DataList]};
	[] ->
		?ERROR("No disco fitap mod found for: ~p, acip: ~p", [SysOid, AcIp]),
		{error, no_disco_mod};
	{error, Reason} ->
		?ERROR("~p", [Reason]),
		{error, Reason}
	end;

discover("SNMP problem", _type, _Dn, Ip, _AgentData, _Entry) ->
  {error, "SNMP problem: " ++ Ip};

discover(SnmpStatus, _type, _Dn, _Ip, _AgentData, _Entry) ->
  {error, "Unknown SnmpStatus: " ++ SnmpStatus}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
report(Dn, [E1, E2, E3 | T]) ->
  report({success, Dn, [E1, E2, E3]}),
  report(Dn, T);

report(Dn, [E1, E2 | T]) ->
  report({success, Dn, [E1, E2]}),
  report(Dn, T);

report(Dn, [E1 | T]) ->
  report({success, Dn, [E1]}),
  report(Dn, T);

report(_Dn, []) ->
  ok.

find_mod(SysOid) ->
    ets:lookup(disco_mod, SysOid).

disco_mod(Record) ->
    disco_mod(Record, #disco_mod{}).

disco_mod([], DiscoMod) ->
    DiscoMod;

disco_mod([{sysoid, SysOid}|T], DiscoMod) ->
    disco_mod(T, DiscoMod#disco_mod{sysoid = SysOid});

disco_mod([{category, Category}|T], DiscoMod) ->
    disco_mod(T, DiscoMod#disco_mod{category = extbif:binary_to_atom(Category)});

disco_mod([{vendor, Vendor}|T], DiscoMod) ->
    disco_mod(T, DiscoMod#disco_mod{vendor = extbif:binary_to_atom(Vendor)});

disco_mod([{model, Model}|T], DiscoMod) ->
    disco_mod(T, DiscoMod#disco_mod{model = extbif:binary_to_atom(Model)});

disco_mod([{module, Mod}|T], DiscoMod) ->
    disco_mod(T, DiscoMod#disco_mod{module = extbif:binary_to_atom(Mod)});

disco_mod([{mib, Mib}|T], DiscoMod) ->
    disco_mod(T, DiscoMod#disco_mod{mib = extbif:binary_to_atom(Mib)});

disco_mod([_H|T], DiscoMod) ->
    disco_mod(T, DiscoMod). 

