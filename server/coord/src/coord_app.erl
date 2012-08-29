%%%----------------------------------------------------------------------
%%% File    : coord_app.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : coord application callback
%%% Created : 19 Feb 2008
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(coord_app).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-behavior(application).

-export([start/0, stop/0]).

%application callback
-export([start/2, stop/1]).

start() ->
	%start libs
    [start_app(App) || App <- [sasl, crypto, extlib, elog, mnesia, worker_pool]],
    [start_app(App) || App <- [amqp_client, emysql, mit, coord]],
    log_version(),
	wait_for_tables(),
    ?INFO_MSG("startup finished.").

start_app(mnesia) ->
    case mnesia:system_info(extra_db_nodes) of
    [] ->
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]);
    _ ->
        ok
    end,
    mnesia:start();

start_app(App) ->
    application:start(App).

log_version() ->
    UpdatedAt = {datetime, {date(), time()}},
    Apps = application:which_applications(),
    case lists:keysearch(coord, 1, Apps) of
	{value, {_, _, Ver}} ->
		emysql:update(versions, [{version, Ver}, {updated_at, UpdatedAt}], 
			{'or', {subsystem, "coord"}, {subsystem, "node"}});
	false ->
		?CRITICAL_MSG("coord is not started!")
	end.

wait_for_tables() ->
	?INFO("waiting for tables: ~p", [mnesia:system_info(local_tables)]),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
	?INFO_MSG("done.").

%%%----------------------------------------------------------------------
%%% stop application.
%%%----------------------------------------------------------------------
stop() ->
    [stop_app(App) || App <- [coord, mit, emysql, 
		amqp_client, elog, mnesia, crypto, sasl]].

stop_app(mnesia) ->
    mnesia:stop();

stop_app(App) ->
    application:stop(App).

%%%----------------------------------------------------------------------
%%% application callback.
%%%----------------------------------------------------------------------
start(normal, _Args) ->
    coord_sup:start_link().

stop(_) ->
	ok.

