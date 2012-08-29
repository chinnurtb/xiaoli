%%%----------------------------------------------------------------------
%%% File    : mit_radio.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit interface
%%% Created : 25 Arg 2009
%%% updated : 18 Arg 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------

-module(mit_radio).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([update/2]).

-define(FIELDS, [id,
				 ap_id,
				 radioAdminStatus,
				 radioOperStatus,
				 radioIndex,
				 radioPlus,
				 radioPeriod,
				 radioDTIM,
				 radioRTS,
				 radioSlice,	
				 radioModel,	
				 radioChannel,
				 radioPower,
				 radioStatus,
				 radio_no]). 


update(ApDn, NewRadios) ->
	case mit:lookup(ApDn) of
	{ok, #entry{uid = {_, ApId}}} ->
		{ok, Records} = emysql:select(mit_radios, ?FIELDS, {ap_id, ApId}),
		OldRadios = [{get_value(radioIndex, R), R} || R <- Records],

		NewIdxList = [Idx || {Idx, _} <- NewRadios],	
		OldIdxList = [Idx || {Idx, _} <- OldRadios],

		{Added, Updated, Deleted} = extlib:list_compare(NewIdxList, OldIdxList),

		%Added
		lists:foreach(fun(Idx) -> 
			NewRadio = get_value(Idx, NewRadios),
			Cn = list_to_binary("radioIndex=" ++ integer_to_list(Idx)),
			Record = [{objectClass, <<"/top/ossRadioIndex">>},
					  {dn, mit_dn:radioDn(Idx, ApDn)},
					  {ap_id, ApId}, {cn, Cn} | NewRadio],
			case emysql:insert(mit_radios, Record) of
			{error, Err} -> ?ERROR("~p", [Err]);
			_ -> ok
			end
		end, Added),

		%Updated
		lists:foreach(fun(Idx) -> 
			NewRadio = get_value(Idx, NewRadios),
			OldRadio = get_value(Idx, OldRadios),
			Id = get_value(id, OldRadio),
			Changed = NewRadio -- OldRadio,
			case Changed of
			[] -> 
				ignore;
			_ ->
				case emysql:update(mit_radios, Changed, {id, Id}) of
				{error, Err} -> ?ERROR("~p", [Err]);
				_ -> ok
				end
			end
		end, Updated),

		%Deleted
		DeletedIds = [get_value(id, get_value(Idx, OldRadios)) || Idx <- Deleted], 
		case DeletedIds of
		[] -> 
			ignore;
		_ ->
			?INFO("deleted radios ~p from ~s", [DeletedIds, ApDn]),
			case emysql:delete(mit_radios, {'in', id, DeletedIds}) of
			{error, Err} -> ?ERROR("~p", [Err]);
			_ -> ok
			end
		end;
	{false, _}  ->
		?ERROR("ap '~p' not found", [ApDn])
	end.

