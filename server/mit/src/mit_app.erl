%%%----------------------------------------------------------------------
%%% File    : mit_app.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit application
%%% Created : 22 Oct 2008
%%% Updated : 23 Oct 2009
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_app).

-export([start/0]).

-behavior(application).

-export([start/2, stop/1]).

-define(APPS, [sasl,
               crypto, 
               mnesia,
               extlib,
               elog,
               evmon,
               epgsql,
               epgqueue,
               amqp_client,
               worker_pool,
               mit]).

start() ->
    [start(App) || App <- ?APPS].

start(mnesia) ->
    case mit:mode() of
    master ->
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]);
    slave ->
        ok
    end,
    mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity);

start(epgsql) ->
    epgsql_app:start();

start(App) ->
	application:start(App).

%=========================
%% application callback
%=========================
start(normal, _Args) ->
    mit_sup:start_link(mit:mode()).

stop(_) ->
    ok.

%====================
%% internal functions
%====================

