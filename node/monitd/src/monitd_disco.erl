%%%----------------------------------------------------------------------
%%% File    : monitd_disco.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Discovery task executor.
%%% Created : 22 Oct. 2008
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(monitd_disco).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-include("mib/rfc1213.hrl").

-import(extbif, [to_list/1]).

-behavior(gen_server).

%%api
-export([start_link/0, 
		setup/1,
        status/0,
        discover/2,
        undiscover/1,
        update/2,
        report/1]).

%%callback
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-record(sysmod, {sysoid, category, vendor, model, module, mib}).

%%--------------------------------------------------------------------
%% @spec start_link() ->  Result
%%   Result = {ok,Pid} | ignore | {error,Error}
%% @doc calls gen_server:start_link
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

setup({sysoids, Sysoids}) ->
	gen_server:cast(?MODULE, {setup, sysoids, Sysoids}).

status() ->
    gen_server:call(?MODULE, status).

discover(Dn, Entry) ->
    gen_server:cast(?MODULE, {discover, Dn, Entry}).

undiscover(Dn) ->
    gen_server:cast(?MODULE, {undiscover, Dn}).

update(Dn, Entry) ->
    gen_server:cast(?MODULE, {update, Dn, Entry}).

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
    ets:new(sysoid, [set, named_table, protected, {keypos, 2}]),
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

handle_cast({setup, sysoids, Records}, State) ->
    ?INFO("setup sysoids: ~p", [length(Records)]),
    %TODO: 
    %[ets:insert(sysmod, sysmod(Record)) || Record <- Records],
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
        [#sysmod{category=Category, vendor=Vendor, model=Type, module=Mod, mib=Mib}] -> 
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
          {error, no_sysmod};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Error} -> 
      {error, Error}
  end;

discover("SNMP OK", fitap, Dn, AcIp, Agent, Entry) ->
    {value, SysOid} = dataset:get_value(sysoid, Entry), 
	case find_mod(SysOid) of
	[#sysmod{mib=Mib}] ->
		{ok, SpecAttrs, DataList} = disco_fitap:disco(Dn, AcIp, Agent, [{mib, Mib}|Entry]),
		{ok, [{entry, fitap, Dn, [{discovery_state, 1}|SpecAttrs]}|DataList]};
	[] ->
		?ERROR("No disco fitap mod found for: ~p, acip: ~p", [SysOid, AcIp]),
		{error, no_sysmod};
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
    ets:lookup(sysmod, SysOid).

sysmod(Record) ->
    sysmod(Record, #sysmod{}).

sysmod([], DiscoMod) ->
    DiscoMod;

sysmod([{sysoid, SysOid}|T], DiscoMod) ->
    sysmod(T, DiscoMod#sysmod{sysoid = SysOid});

sysmod([{category, Category}|T], DiscoMod) ->
    sysmod(T, DiscoMod#sysmod{category = extbif:binary_to_atom(Category)});

sysmod([{vendor, Vendor}|T], DiscoMod) ->
    sysmod(T, DiscoMod#sysmod{vendor = extbif:binary_to_atom(Vendor)});

sysmod([{model, Model}|T], DiscoMod) ->
    sysmod(T, DiscoMod#sysmod{model = extbif:binary_to_atom(Model)});

sysmod([{module, Mod}|T], DiscoMod) ->
    sysmod(T, DiscoMod#sysmod{module = extbif:binary_to_atom(Mod)});

sysmod([{mib, Mib}|T], DiscoMod) ->
    sysmod(T, DiscoMod#sysmod{mib = extbif:binary_to_atom(Mib)});

sysmod([_H|T], DiscoMod) ->
    sysmod(T, DiscoMod). 

