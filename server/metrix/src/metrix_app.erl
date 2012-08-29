%%%----------------------------------------------------------------------
%%% File    : metrix_app.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : metrix app startup
%%% Created : 14 Mar. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(metrix_app).

-include_lib("elog/include/elog.hrl").

-export([start/0, stop/0]).

-behaviour(application).

-export([start/2, stop/1]).

start() ->
    [start_app(App) || App <- [sasl, crypto, mnesia, elog, evmon, amqp_client, metrix]].

start_app(mnesia) ->
    case mnesia:system_info(extra_db_nodes) of
    [] ->
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]);
    _ ->
		ok
    end,
	mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity);

start_app(App) ->
    application:start(App).

stop() ->
    [stop_app(App) || App <- [metrix, amqp_client, elog, mnesia, crypto, sasl]].

stop_app(mnesia) ->
    mnesia:stop();

stop_app(App) ->
    application:stop(App).
    
start(normal, _Args) ->
	PoolSize = 
	case application:get_env(pool_size) of
	{ok, schedulers} -> erlang:system_info(schedulers);
	{ok, Val} -> Val;
	undefined -> erlang:system_info(schedulers)
	end,
	metrix_sup:start_link(PoolSize).

stop(_) ->
	ok. 
