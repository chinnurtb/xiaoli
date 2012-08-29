%%%----------------------------------------------------------------------
%%% File    : evabus_cron.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : evabus cron
%%% Created : 29 May. 2012
%%% License : http://opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(evabus_cron).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-import(erlang, [send_after/3]).

-behavior(gen_server).

-export([start_link/1]).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(task, {name, period, mfa}).

start_link(Conf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Conf], []).

init([Conf]) ->
	ets:new(evabus_task, [named_table, {keypos, 2}]),
	{ok, Terms} = file:consult(Conf),
	lists:foreach(fun(I) -> 
		{Name, Period, M, F, A} = lists:nth(I, Terms),
		Task = #task{name = Name, period = Period, mfa = {M, F, A}},
		ets:insert(evabus_task, Task),
		Delay = 300+I*30,
		?INFO("schedule task ~s after ~p seconds", [Name, Delay]),
		send_after(Delay*1000, self(), {schedule, Task})
	end, lists:seq(1, length(Terms))),
	?INFO("~p is started.", [?MODULE]),
    {ok, state}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({schedule, #task{name = Name, period = Period, mfa = {M,F,A}} = Task}, State) ->
	try 
        erlang:apply(M, F, A) 
    catch _:Err -> 
        ?ERROR("~p: ~p", [Name, Err]),
        ?ERROR("~p", [erlang:get_stacktrace()])
    end,
	erlang:send_after(Period*1000, self(), {schedule, Task}),
	{noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

