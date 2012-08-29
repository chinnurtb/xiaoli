%%%----------------------------------------------------------------------
%%% File    : mib_record.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mib record framework
%%% Created : 18 Jun. 2012
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mib_record).

-include_lib("elog/include/elog.hrl").

-export([group/1,
        group/2,
        table/1,
        table/2,
        table/3,
        table/4,
		entry/2,
		entry/3]).

group([]) ->
	fun nullfun/2;

group(Oids) ->
    group(Oids, fun mib_mapper:null_mapping/1).

group([], _) ->
	fun nullfun/2;

group([Oid|_] = Oids, MapFun) when is_function(MapFun) ->
    fun(Ip, Agent) ->
        case sesnmp:get_group(Ip, Oids, Agent) of
        {ok, Record} ->
            MapFun(dropbad(Record, onbadfun(Ip, Oids)));
        {error, Reason} ->
			onerror(Agent, Reason, fun() -> 
				?WARNING("SnmpError. Reason: ~p, Host: ~p~nOid: ~p", [Reason, Ip, Oid]) 
			end)
        end
    end.

table([]) ->
	fun nullfun/2;

table(Oids) ->
    table(Oids, fun mib_mapper:null_mapping/1).

table([], _) ->
	fun nullfun/2;

table(Oids, MapFun) when is_function(MapFun) ->
    table(Oids, MapFun, fun nofilter/1).

table([], _, _) ->
	fun nullfun/2;

table(Oids, MapFun, Filter) when is_function(MapFun)
    and is_function(Filter) ->
    table(Oids, MapFun, Filter, fun noreduce/1).

table([], _,_,_) ->
	fun nullfun/2;

table([Oid|_] = Oids, MapFun, Filter, ReduceFun) when is_function(MapFun)
    and is_function(Filter) and is_function(ReduceFun) ->
    fun(Ip, Agent) ->
        case sesnmp:get_table(Ip, Oids, Agent) of
        {ok, Records} ->
            ReduceFun([MapFun(dropbad(Record, onbadfun(Ip, Oids))) 
				|| Record <- Records, Filter(Record)]);
		{error,{timeout,{ReqId, _}} = Reason} ->
			onerror(Agent, Reason, fun() -> 
				?WARNING("SnmpTimeout. ReqId: ~p, Host: ~p~nOid: ~p", 
					[ReqId, Ip, Oid])
			end);
        {error, Reason} ->
			onerror(Agent, Reason, fun() -> 
				?WARNING("SnmpError. Reason: ~p, Host: ~p~nOid: ~p", 
					[Reason, Ip, Oid]) 
			end)
        end
    end.

entry([], _IdxOid) ->
	fun nullfun/2;

entry(Oids, IdxOid) ->
    entry(Oids, IdxOid, fun mib_mapper:null_mapping/1).

entry([], _IdxOid, _) ->
	fun nullfun/2;

entry([Oid|_] = Oids, IdxOid, MapFun) when is_function(MapFun) ->
    fun(Ip, Agent) ->
        case sesnmp:get_entry(Ip, Oids, IdxOid, Agent) of
        {ok, Record} ->
            MapFun(dropbad(Record, onbadfun(Ip, Oids)));
        {error, Reason} ->
			onerror(Agent, Reason, fun() -> 
				?WARNING("SnmpError. Reason: ~p, Host: ~p~nOid: ~p, OidIdx: ~s", 
					[Reason, Ip, Oid, mib_oid:to_str(IdxOid)]) 
			end)
        end
    end.

nullfun(Ip, _Agent) ->
	?WARNING("nullfun for: ~p", [Ip]),
	[].

nofilter(_) -> 
    true.

noreduce(L) -> 
    L.

onerror(Agent, Reason, ErrFun) ->
	case proplists:is_defined(error, Agent) of
	true ->
		{error, Reason};
	false ->
		ErrFun(), []
	end.
onbadfun(Ip, Oids) ->
	fun(BadFields) ->
		?WARNING("noSuchInstance found. Host: ~p, BadFields: ~n~p", [Ip, BadFields]),
		[?INFO("~s oid: ~s", [Name, mib_oid:to_str(Oid)]) || {Name, Oid} <- Oids]
	end.

dropbad(Record, OnBadFun) ->
	{Record1, BadFields} = 
	lists:foldl(fun({N, V}, {Acc, BadAcc}) -> 
		if
		V == noSuchInstance -> 
			{Acc, [N|BadAcc]};
		V == noSuchObject ->
			{Acc, [N|BadAcc]};
		true ->
			{[{N, V}|Acc], BadAcc}
		end
	end, {[], []}, Record),
	case length(BadFields) of
	0 -> ok;
	_ -> OnBadFun(BadFields)
	end,
	lists:reverse(Record1).


