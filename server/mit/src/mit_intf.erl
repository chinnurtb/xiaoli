%%%----------------------------------------------------------------------
%%% File    : mit_intf.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit interface
%%% Created : 25 Arg 2009
%%% updated : 18 Arg 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_intf).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([update/2]).

-define(FIELDS, [id,
				 ifIndex,
				 ifAdminStatus,
				 ifOperStatus,
				 ifMtu,
				 ifDescr,	
				 ifPhysAddress,
				 ifSpeed,
				 ifType]). 

update(Dn, NewIntfs) ->
	case mit:lookup(Dn) of
	{ok, #entry{uid = {Class, DeviceId}}} ->
		DeviceType = mit:device_type(Class),
		Where = {'and', {device_type, DeviceType}, {device_id, DeviceId}},
		case emysql:select(mit_intfs, ?FIELDS, Where) of
		{ok, Records} ->
			OldIntfs = [{get_value(ifIndex, Record), Record} || Record <- Records],			
			
			NewIdxList = [Idx || {Idx, _} <- NewIntfs],	
			OldIdxList = [Idx || {Idx, _} <- OldIntfs],

			{Added, Updated, Deleted} = extlib:list_compare(NewIdxList, OldIdxList),
			lists:foreach(fun(Idx) -> 
				NewIntf = get_value(Idx, NewIntfs),
				Record = [{dn, mit_dn:ifdn(Idx, Dn)}, 
						  {device_type, DeviceType}, 
						  {device_id, DeviceId} | NewIntf],
				case emysql:insert(mit_intfs, Record) of
				{error, Err} -> ?ERROR("~p", [Err]);
				_ -> ok
				end
			end, Added),

			DeletedIds = [get_value(id, get_value(Idx, OldIntfs)) || Idx <- Deleted], 
			case DeletedIds of
			[] -> 
				ignore;
			_ ->
				?INFO("deleted intfs ~p from ~s", [DeletedIds, Dn]),
				case emysql:delete(mit_intfs, {'in', id, DeletedIds}) of
				{error, Err} -> ?ERROR("~p", [Err]);
				_ -> ok
				end
			end,

			lists:foreach(fun(Idx) -> 
				NewIntf = get_value(Idx, NewIntfs),
				OldIntf = get_value(Idx, OldIntfs),
				Id = get_value(id, OldIntf),
				Changed = NewIntf -- OldIntf,
				case Changed of
				[] -> 
					ignore;
				_ ->
					case emysql:update(mit_intfs, Changed, {id, Id}) of
					{error, Err2} -> ?ERROR("~p", [Err2]);
					_ -> ok
					end
				end
			end, Updated);
		{error, Error} ->
			?ERROR("~p", [Error])
		end;
	{false, _} ->
		?ERROR(" ~p not found", [Dn])
	end.
