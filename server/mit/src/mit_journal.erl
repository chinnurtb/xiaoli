%%%----------------------------------------------------------------------
%%% File    : mit_journal.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : 
%%% Created : 12 Apr. 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_journal).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-import(extbif, [to_list/1]).

-export([start_link/0,
        log/4]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).

-record(state, {}).


%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log(MoType, MoName, Action, Details) ->
    gen_server:cast(?MODULE, {log, MoType, MoName, Action, Details}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	?INFO_MSG("mit_journal is started."),
    {ok, #state{}}.
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Req, _From, State) ->
    ?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({log, MoType, MoName, Action, Details}, State) ->
    CreatedAt = {datetime, {date(), time()}},
    case emysql:insert(mit_journals, [{mo_type, to_list(MoType)}, {mo_name, MoName}, 
        {action, to_list(Action)}, {created_at, CreatedAt}]) of
    {updated, {_, Id}} ->
        lists:foreach(fun({Attr, OldVal, Val}) -> 
            Res = emysql:insert(mit_journal_details, [{journal_id, Id}, {attr, to_list(Attr)},
                {old_value, to_list(OldVal)}, {value, to_list(Val)}]),
            ?INFO("~p", [Res])
        end, Details);
    {error, Reason} ->
        ?ERROR("~p", [Reason])
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


