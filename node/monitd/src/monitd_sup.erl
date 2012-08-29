-module(monitd_sup).

-author("ery.lee@gmail.com").

-behavior(supervisor).

-export([start_link/1, init/1]).

-define(CHILD(M), 
		{M, {M, start_link, []},
			permanent, 5000, worker, [M]}). 

-define(CHILD2(M, O), 
		{M, {M, start_link, [O]},
			permanent, 5000, worker, [M]}). 

start_link(Role) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Role]).

%woker with coord, without monitd
init([worker]) ->
    {ok, JournalOpts} = application:get_env(journal),
    {ok, CmdPool} = application:get_env(oscmd_pool),
	{ok, {{one_for_one, 10, 100}, [
			?CHILD(mib_registry),
			?CHILD2(monitd_oscmd, CmdPool),
			?CHILD(monitd_sched),
			?CHILD(disco_server),
			?CHILD2(monitd_journal, JournalOpts),
			?CHILD(monitd_hub),
			?CHILD(monitd_coord)
		]}
	};

%node with monitd, without coord
init([_]) ->
    {ok, JournalOpts} = application:get_env(journal),
    {ok, CmdPool} = application:get_env(oscmd_pool),
	{ok, {{one_for_one, 10, 100}, [
		?CHILD(mib_registry),
		?CHILD2(monitd_oscmd, CmdPool),
		?CHILD2(monitd_journal, JournalOpts),
		?CHILD(disco_server),
		?CHILD(monitd_hub),
		?CHILD2(monitd, application:get_all_env())]}}.

