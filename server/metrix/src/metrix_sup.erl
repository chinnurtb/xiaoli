%%%----------------------------------------------------------------------
%%% File    : metrix_sup.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : status and metrix process
%%% Created : 16 May. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(metrix_sup).

-author("ery.lee@gmail.com").

-behavior(supervisor).

-export([start_link/1, init/1]).

start_link(PoolSize) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [PoolSize]).

init([PoolSize]) ->
    Workers = [worker(Id) || Id <- lists:seq(1, PoolSize)],
    {ok, {{one_for_one, 10, 100}, Workers}}.

worker(Id) ->
	{metrix:name(Id), {metrix, start_link, [Id]},
	   permanent, 5000, worker, [metrix]}.
