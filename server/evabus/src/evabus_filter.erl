%%%----------------------------------------------------------------------
%%% File    : evabus_filter.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Evabus filter
%%% Created : 26 Jan 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com
%%%----------------------------------------------------------------------
-module(evabus_filter).

-author('ery.lee@gmail.com').

-include("event.hrl").

-include("alarm.hrl").

-include_lib("elog/include/elog.hrl").

-include_lib("mit/include/mit.hrl").

-export([start_link/1, 
        filter/1]).

-record(state, {dir}).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2,
        code_change/3]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Dir], []).

filter(#event{sender = Sender, source = undefined} = Event) ->
    case mit:lookup(Sender) of
    {false, _} -> 
        ?INFO("sender ~p not exist", [Sender]), true;
    _ ->
        rule_filter(ets:first(event_filter), Event)
    end;

filter(#event{sender = Sender, source = Source} = Event) ->
    case {mit:lookup(Sender), mit:lookup(Source)} of
    {{false, _}, _} -> 
        ?INFO("sender ~p not exist", [Sender]), true;
    {_, {false, _}} ->
        ?INFO("source ~p not exist", [Source]), true;
    _ ->
        rule_filter(ets:first(event_filter), Event)
    end.

rule_filter('$end_of_table', _) ->
    false;

rule_filter(Exp, Event) ->
    case prefix_exp:eval(Exp, evabus_event:record(Event)) of
    false -> 
        rule_filter(ets:next(event_filter, Exp), Event);
    true -> 
        true
    end.

init([Dir]) ->
    ets:new(event_filter, [set, named_table]),
    State = #state{dir = Dir},
    handle_info(load_filters, State),
	?INFO_MSG("evabus_filter is started."),
    {ok, State}.

handle_call(Req, _From, State) ->
    ?ERROR("unexpected request: ~p", [Req]),
    {reply, {error, unexpected_req}, State}.

handle_cast(Msg, State) ->
    ?ERROR("unexpected message: ~p", [Msg]),
    {noreply, State}.

handle_info(load_filters, #state{dir = Dir} = State) ->
    {ok, FilterFiles} = file:list_dir(Dir),
    lists:foreach(fun(File) -> 
        case lists:suffix(".filter", File) of
        true -> 
            %?INFO("load filter: ~p", [File]),
            case file:consult(filename:join(Dir, File)) of 
            {ok, Filters} ->
                [ets:insert(event_filter, {Filter}) || Filter <- Filters];
            {error, Reason} ->
                ?ERROR("Can't load filter ~p : ~p", [File, Reason])
            end;
        false ->
            ignore
        end
    end, FilterFiles),
    erlang:send_after(300000, self(), load_filters),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

