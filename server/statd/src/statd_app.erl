-module(statd_app).

-include_lib("elog/include/elog.hrl").

-export([start/0, stop/0]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APPS, [sasl,
			   crypto,
			   elog,
			   evmon,
			   emysql,
			   worker_pool,
			   amqp_client,
			   statd]).

start() ->
	[start_app(App) || App <- ?APPS].

start_app(App) ->
	application:start(App).

stop() ->
	[stop_app(App) || App <- lists:reverse(?APPS)].

stop_app(App) ->
	application:stop(App).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    statd_sup:start_link().

stop(_State) ->
    ok.

