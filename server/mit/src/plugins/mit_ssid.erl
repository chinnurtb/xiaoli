%%%----------------------------------------------------------------------
%%% File    : mit_ssid.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : ap ssid store
%%% Created : 25 Arg 2009
%%% updated : 18 Arg 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_ssid).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([update/2]).

-define(FIELDS, [id,
				cn,
				ap_id,
				ssidIndex,
				text,
				radio_no,
				ssidEnabled,
				ssidHidden,
				staIsolate,
				dot11Auth,
				security,
				authenMode,
				securityCiphers,
				vlanId,
				maxSimultUsers]).

update(ApDn, NewSsids) ->
	case mit:lookup(ApDn) of
	{ok, #entry{uid = {_, ApId}}} ->
		{ok, Records} = emysql:select(mit_ssids, ?FIELDS, {ap_id, ApId}), 
		OldSsids = [{get_value(ssidIndex, Record), Record} || Record <- Records],

		NewIdxList = [Idx || {Idx, _} <- NewSsids],	
		OldIdxList = [Idx || {Idx, _} <- OldSsids],
		
		{Added, Updated, Deleted} = extlib:list_compare(NewIdxList, OldIdxList),
		lists:foreach(fun(Idx) ->
			NewSsid = get_value(Idx, NewSsids),
			Record = [{ap_id, ApId}, {dn, mit_dn:ssidDn(Idx, ApDn)}, 
					  {objectClass, <<"/top/ossSsidIndex">>} | NewSsid],
			case emysql:insert(mit_ssids, Record) of
			{error, Reason} -> ?ERROR("~p", [Reason]);
			_ -> ok
			end
		end, Added),

		DeletedIds = [get_value(id, get_value(Idx, OldSsids)) || Idx <- Deleted], 
		case DeletedIds of
		[] -> 
			ignore;
		_ ->
			?INFO("deleted ssids ~p from ~s", [DeletedIds, ApDn]),
			case emysql:delete(mit_ssids, {'in', id, DeletedIds}) of
			{error, Err} -> ?ERROR("~p", [Err]);
			_ -> ok
			end
		end,

		lists:foreach(fun(Idx) -> 
			NewSsid = get_value(Idx, NewSsids),
			OldSsid = get_value(Idx, OldSsids),
			Id = get_value(id, OldSsid),
			Changed = NewSsid -- OldSsid,
			case Changed of
			[] -> 
				ignore;
			_ ->
				case emysql:update(mit_ssids, Changed, {id, Id}) of
				{error, Err2} -> ?ERROR("~p", [Err2]);
				_ -> ok
				end
			end
		end, Updated);
	{false, _}  ->
		?ERROR("ap '~p' not found", [ApDn])
	end.

