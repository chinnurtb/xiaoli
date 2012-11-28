%%%----------------------------------------------------------------------
%%% File    : evabus_app.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : evabus application callback
%%% Created : 13 Aug 2008
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(evabus_app).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-export([start/0, stop/0]).

-behavior(application).

-export([start/2, stop/1]).

-define(APPS, [sasl,
			   crypto,
			   mnesia,
			   extlib,
			   elog,
			   epgsql,
			   amqp_client,
			   mit,
			   evabus]).

start() ->
    [start_app(App) || App <- ?APPS],
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

start_app(epgsql) ->
    epgsql_app:start();

start_app(App) ->
    application:start(App).

wait_for_tables() ->
	?INFO("waiting for tables: ~p", [mnesia:system_info(local_tables)]),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
	?INFO_MSG("done.").

stop() ->
    [stop_app(App) || App <- ?APPS].

stop_app(mnesia) ->
    mnesia:stop();

stop_app(App) ->
    application:stop(App).

start(normal, _Args) ->
	evabus_sup:start_link().

stop(_) ->
	ok.

