%%%----------------------------------------------------------------------
%%% File    : monitd_app.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : monitd application
%%% Created : 13 Jan 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2010, www.opengoss.com
%%%----------------------------------------------------------------------
-module(monitd_app).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-export([start/0, worker_start/0, stop/0]).

-behavior(application).

-export([start/2, stop/1]).

%node start 
start() ->
	start_libs(),
	case init:get_argument(workers) of
	{ok, [[S]]} -> start_workers(list_to_integer(S));
	_ -> start_workers(erlang:system_info(schedulers))
	end,
	[start_app(App) || App <- [amqp_client, sesnmp, agent, monitd]].

%worker start 
worker_start() ->
	start_libs(),
	[start_app(App) || App <- [amqp_client, sesnmp, monitd]].

start_libs() ->
	[start_app(App) || App <- [sasl, crypto, mnesia,
		rabbitmq_common, extlib, elog, evmon]].

start_workers(N) ->
	?INFO("start ~p workers", [N]),
	{ok, Host} = inet:gethostname(),
	?INFO("Host: ~p", [Host]),
	{ok, [[Home]]} = init:get_argument(node_home),
	?INFO("Home: ~p", [Home]),
	{ok, [[Cookie]]} = init:get_argument(setcookie),
	?INFO("Cookie: ~p", [Cookie]),
	lists:foreach(fun(I) -> 
		Name = worker_name(I),
		SaslLog = sasl_log(Home, I),
		LagerCfg = lager_log(Home, I),
		CrashLog = crash_log(Home, I),
		MnesiaDir = Home ++ "/var/mnesia/"++ Name,
		Args = ["-pa", Home ++ "/ebin",
				"-setcookie", Cookie,
				"-config", Home ++ "/etc/worker.config",
				"-args_file", Home ++ "/etc/worker.args",
				"-mnesia dir \\\"" ++ MnesiaDir ++ "\\\"",
				"-lager crash_log " ++ "\\\"" ++ CrashLog ++ "\\\"",
				"-lager handlers " ++ LagerCfg,
				"-boot start_sasl",
				"-sasl sasl_error_logger", "\\{file,\\\"" ++ SaslLog ++ "\\\"\\} "],
		%?INFO("~p", [string:join(Args, " ")]),
		{ok, Node} = slave:start(list_to_atom(Host), 
			list_to_atom(Name), string:join(Args, " ")),
		?INFO("slave ~p is started.", [Node])
	end, lists:seq(1, N)).
	
worker_name(I) ->
	"worker" ++ integer_to_list(I).

sasl_log(Home, I) ->
	Home ++ "/log/worker" ++ integer_to_list(I) ++ "_sasl.log".

crash_log(Home, I) ->
	Home ++ "/log/worker" ++ integer_to_list(I) ++ "_crash.log".

lager_log(Home, I) ->
	%\\{lager_console_backend,info\\},
	%InfoLog = Home ++ "/log/worker" ++ integer_to_list(I) ++ "_info.log",
	ErrorLog = Home ++ "/log/worker" ++ integer_to_list(I) ++ "_error.log",
    %FIXME: for jilin only
	"\\[\\{lager_file_backend,\\[\\{\\\"" ++ ErrorLog ++ "\\\",error,20485760,\\\"$D0\\\",5\\}\\]\\}\\]".
	%"\\[\\{lager_file_backend,\\[\\{\\\"" ++ InfoLog ++ "\\\",info,20485760,\\\"$D0\\\",5\\},\\{\\\"" ++ ErrorLog ++ "\\\",error,20485760,\\\"$D0\\\",5\\}\\]\\}\\]".

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
    [stop_app(App) || App <- [monitd, sesnmp, amqp_client, mnesia, sasl]],
    ?PRINT("~nstopped.~n", []).

stop_app(mnesia) ->
    mnesia:stop();
    
stop_app(App) ->
    application:stop(App).

start(normal, _Args) ->
	case init:get_argument(worker) of
	error -> %node
		monitd_sup:start_link(node);
	{ok, _} ->
		monitd_sup:start_link(worker)
	end.

stop(_) ->
	ok.
	
