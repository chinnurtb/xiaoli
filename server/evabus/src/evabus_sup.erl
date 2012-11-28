%%---------------------------------------------------------------------- 
%%% File    : evabus_sup.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : evabus supervisor
%%% Created : 29 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(evabus_sup).

-author('ery.lee@gmail.com').

-behavior(supervisor).

-export([start_link/0, init/1]).

-define(CHILD(M), 
		{M, {M, start_link, []},
			permanent, 5000, worker, [M]}). 

-define(CHILD2(M, O), 
		{M, {M, start_link, [O]},
			permanent, 5000, worker, [M]}). 

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, CronConf} = application:get_env(cron),
    {ok, FilterDir} = application:get_env(filter_dir),
	{ok, {{one_for_one, 10, 3600}, [
		?CHILD(evabus_setting),
		?CHILD(evabus_class),
		?CHILD(evabus_store),
		?CHILD(evabus_correlator),
		?CHILD2(evabus_filter, FilterDir),
		?CHILD(evabus_logger),
		?CHILD(evabus_mapper),
		?CHILD(evabus),
		?CHILD2(evabus_cron, CronConf)
	]}}.

