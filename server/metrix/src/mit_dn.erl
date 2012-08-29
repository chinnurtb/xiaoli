%%%----------------------------------------------------------------------
%%% File    : mit_dn.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit dn utilities
%%% Created : 18 Arg 2011
%%% Updated : 09 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_dn).

-export([bdn/1,
		 rdn/1,
		 ifdn/2,
		 radioDn/2,
		 ssidDn/2,
		 reverse/1]).

%base dn
bdn(Dn) when is_binary(Dn) ->
    list_to_binary(bdn(binary_to_list(Dn)));

bdn(Dn) when is_list(Dn) ->
	[_|T] = string:tokens(Dn, ","),
	string:join(T, ",").

%relative dn
rdn(Dn) when is_binary(Dn) ->
    list_to_binary(rdn(binary_to_list(Dn)));

rdn(Dn) when is_list(Dn) ->
    [Rdn|_] = string:tokens(Dn, ","),
    Rdn.

ifdn(IfIndex, Dn) ->
    list_to_binary(["ifindex=", integer_to_list(IfIndex), ",", Dn]).

radioDn(Index, Dn) ->
    list_to_binary(["radioIndex=", integer_to_list(Index), ",", Dn]).

ssidDn(Index, Dn) ->
    list_to_binary(["ssidIndex=", integer_to_list(Index), ",", Dn]).

reverse(Dn) when is_binary(Dn) ->
    list_to_binary(reverse(binary_to_list(Dn)));
	
reverse(Dn) when is_list(Dn) ->
	string:join(lists:reverse(string:tokens(Dn, ",")), ",").

