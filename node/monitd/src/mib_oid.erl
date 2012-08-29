%%%----------------------------------------------------------------------
%%% File    : mib_oid.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mib oid
%%% Created : 27 Dec 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mib_oid).

-export([new/1,
        to_str/1,
        add_idx/2,
        map/1,
        is_scalar/1]).

new(Oid) when is_binary(Oid) ->
    new(binary_to_list(Oid));

new(Oid) when is_list(Oid) ->
	[list_to_integer(S) || S <- string:tokens(Oid, ".")].

is_scalar(Oid) ->
    lists:last(Oid) == 0.

to_str(Oid) ->
	string:join([integer_to_list(I) || I <- Oid], ".").
    
add_idx(Oids, Idx) ->
    add_idx(Oids, Idx, []).

add_idx([], _Idx, Acc) ->
    lists:reverse(Acc);
add_idx([{Name, Oid}|T], Idx, Acc) ->
    add_idx(T, Idx, [{Name, Oid ++ Idx}|Acc]).

map(Items) ->
    [{N, new(S)} || {N, S} <- Items].

