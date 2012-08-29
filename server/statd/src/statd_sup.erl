%%%----------------------------------------------------------------------
%%% File    : statd_sup.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : statd supervisor
%%% Created : 12 May. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(statd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 10, 3600}, [?CHILD(statd_hub), ?CHILD(statd)]}}.

