%%%----------------------------------------------------------------------
%%% File    : mit_sup.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit_supervisor
%%% Created : 21 Feb 2008
%%% Updated : 13 Sep 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_sup).

-author("ery.lee@gmail.com").

-define(CHILD(I), {I, {I, start_link, []},
		permanent, 5000, worker, [I]}).

-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Workers =
    case mnesia:system_info(extra_db_nodes) of
    [] -> %master node
        [?CHILD(mit_event)|workers()];
    _ -> %slave node
        workers()
    end,
	Workers1 =
	case application:get_env(mode) of
	{ok, master} ->
		Workers ++ [?CHILD(mit_server), ?CHILD(mit_journal)];
	_ ->
		Workers
	end,
    {ok, {{one_for_one, 10, 100}, Workers1}}.

workers() ->
    [?CHILD(mit_dict), ?CHILD(mit)].


