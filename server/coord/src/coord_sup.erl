-module(coord_sup).

-author("ery.lee@gmail.com").

-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Db = {coord_db, {coord_db, start_link, []},
	            permanent, 5000, worker, [coord_db]},
    Env = application:get_all_env(),
	Srv= {coord, {coord, start_link, [Env]},
	            permanent, 5000, worker, [coord]},
	{ok, {{one_for_one, 10, 1000}, [Db, Srv]}}. 


