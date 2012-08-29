-module(mit_app).

-author('ery.lee@gmail.com').

-export([start/0]).

-behavior(application).

-export([start/2, stop/1]).

start() ->
    [start_app(App) || App <- [sasl, crypto, mnesia, extlib, elog, evmon,
								amqp_client, emysql, worker_pool, mit]].

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

start(normal, _Args) ->
    mit_sup:start_link().

stop(_) ->
    ok.
