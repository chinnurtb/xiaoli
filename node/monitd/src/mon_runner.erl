%%%----------------------------------------------------------------------
%%% File    : mon_runner.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : general monintor runner  
%%% Created : 11 Aug 2009
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mon_runner).

-import(dataset, [get_value/2, get_value/3]).

-import(extbif, [to_list/1, timestamp/0]).

-export([run/2,
		 runomc/2]).

run(Args, Fun) ->
	{value, Dn} = get_value(dn, Args),
	{value, Ip} = get_value(ip, Args),
	{value, Community} = get_value(snmpCommunity, Args, <<"public">>),
	Agent= [{community, to_list(Community)}],
    Fun(Dn, to_list(Ip), timestamp(), Agent).

runomc(Args, Fun) ->
	{value, Dn} = get_value(dn, Args),
	{value, Ip} = get_value(extra, Args),
	{value, Community} = get_value(snmpCommunity, Args, <<"public">>),
	Agent= [{community, to_list(Community)}],
    Fun(Dn, to_list(Ip), timestamp(), Agent).
   
