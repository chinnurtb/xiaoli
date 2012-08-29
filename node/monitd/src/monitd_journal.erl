%%%----------------------------------------------------------------------
%%% File    : monitd_journal.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : raw metrics journal.
%%% Created : 13 Jun. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(monitd_journal).

-author('ery.lee@gmail.com').

-import(extbif, [timestamp/0, zeropad/1]).
-import(monitd_hub, [unique_rdn/1]).

-include("metric.hrl").

-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/1, 
        info/0,
        write/1]).

-export([init/1, 
        handle_call/3, 
        priorities_call/3,
        handle_cast/2,
        handle_info/2,
        priorities_info/2,
        terminate/2,
        code_change/3]).

-record(state, {logdir, logfile, thishour, buffer_size = 100, queue = []}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Opts) ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [Opts], [{spawn_opt, [{min_heap_size, 20480}]}]).

info() ->
    gen_server2:call(?MODULE, info).

write(Metric) when is_record(Metric, metric) ->
    gen_server2:cast(?MODULE, {write, Metric}).

init([Opts]) ->
	random:seed(now()),
    Dir = proplists:get_value(dir, Opts),
    BufferSize = proplists:get_value(buffer, Opts, 100),
	BufferSize1 = BufferSize + random:uniform(BufferSize),
    State = #state{logdir = Dir, buffer_size = BufferSize1},
    {noreply, NewState} = handle_info(journal_rotation, State),
    erlang:send_after(2000, self(), flush_queue),
    ?INFO("~p is started.", [?MODULE]),
    {ok, NewState}.

handle_call(info, _From, #state{thishour = H} = State) ->
    Info = [{hour, H} | get()],
    {reply, {ok, Info}, State};
    
handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

priorities_call(info, _From, _State) ->
    3.

handle_cast({write, Metric}, #state{logfile = LogFile, buffer_size = MaxSize, queue = Q} = State) ->
    case length(Q) >= MaxSize of
    true ->
        incr(commit),
        flush_to_disk(LogFile, [Metric|Q]),
        {noreply, State#state{queue = []}};
    false ->
        NewQ = [Metric | Q],
        {noreply, State#state{queue = NewQ}}
    end;
    
handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(journal_rotation, #state{logdir = Dir, logfile = File, queue = Q} = State) ->
    flush_queue(File, Q),
    close_file(File),
    Now = timestamp(),
    {Hour,_,_} = time(),
	[Node|_] = string:tokens(atom_to_list(node()), "@"),
    FilePath = lists:concat([Dir, "/", zeropad(Hour), "/", Node, ".journal"]),
    filelib:ensure_dir(FilePath),
    {ok, NewFile} = file:open(FilePath, [write]),
    NextHour = ((Now div 3600) + 1) * 3600,
    erlang:send_after((NextHour + 60 - Now) * 1000, self(), journal_rotation),
    {noreply, State#state{logfile = NewFile, thishour = Hour, queue = []}};

handle_info(flush_queue, #state{logfile = File, queue = Q} = State) ->
    flush_queue(File, Q),
    erlang:send_after(2000, self(), flush_queue),
    {noreply, State#state{queue = []}};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

priorities_info(journal_rotation, _State) ->
    10;
priorities_info(flush_queue, _State) ->
    5.

terminate(_Reason, #state{logfile = LogFile, queue = Q}) ->
    flush_queue(LogFile, Q),
    close_file(LogFile),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

incr(Key) ->
    case get(Key) of
    undefined -> put(Key, 1);
    V -> put(Key, V+1)
    end.

close_file(undefined) ->
    ok;

close_file(File) ->
    file:close(File).

flush_queue(undefined, _Q) ->
    ok;
flush_queue(_File, Q) when length(Q) == 0 ->
    ok;
flush_queue(File, Q) ->
    flush_to_disk(File, Q).

flush_to_disk(LogFile, Q) ->
    Lines = [line(Metric) || Metric <- lists:reverse(Q)],
    file:write(LogFile, list_to_binary(Lines)).

line(#metric{name=Name, from=From, dn=Dn, timestamp=Ts, data=Data}) ->
	io_lib:format("~s:~s:~p@~s: ~s~n", [unique_rdn(Dn), Name, Ts, From, encode(Data)]).

encode(Data) ->
	Tokens = [lists:concat([atom_to_list(N), "=", strnum(V)]) || {N, V} <- Data],
	string:join(Tokens, ",").

strnum(V) when is_integer(V) ->
    integer_to_list(V);
strnum(V) when is_float(V) ->
    [S] = io_lib:format("~.6f", [V]), S;
strnum(V) when is_list(V) ->
	V;
strnum(V) when is_atom(V) ->
	atom_to_list(V);	
strnum(V) when is_binary(V) ->
	binary_to_list(V).

