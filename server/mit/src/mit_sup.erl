%%%----------------------------------------------------------------------
%%% File    : mit_sup.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit supervisor
%%% Created : 21 Feb 2008
%%% Updated : 13 Sep 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_sup).

-author("ery.lee@gmail.com").

-define(CHILD(I), {I, {I, start_link, []},
                    permanent, 5000, worker, [I]}).

-behavior(supervisor).

-export([start_link/1, init/1]).

start_link(Mode) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Mode]).

init([master]) ->
    {ok, {{one_for_one, 10, 100}, [
        ?CHILD(mit_meta), 
        ?CHILD(mit),
        ?CHILD(mit_event),
        ?CHILD(mit_server)]}};

init([slave]) ->
    {ok, {{one_for_one, 10, 100}, [
        ?CHILD(mit_meta),
        ?CHILD(mit)]}}.

