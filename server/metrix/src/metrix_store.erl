%%%----------------------------------------------------------------------
%%% File    : metrix_store.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : store metrixs to errdb
%%% Created : 24 May 2010
%%% Updated : 08 Sep 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(metrix_store).

-author('ery.lee@gmail.com').

-include("metric.hrl").

-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/1,
        insert/2]).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).

-record(state, {errdb}).

start_link(Id) ->
    gen_server2:start_link({local, name(Id)}, ?MODULE, [Id], []).

name(Id) ->
	name(?MODULE, Id).

name(Mod, Id) ->
	list_to_atom(atom_to_list(Mod) ++ "_" ++ integer_to_list(Id)).

%Metric: {Type, Dn, Timestamp, Datalog}
insert(Pid, Metric) ->
	gen_server2:cast(Pid, {insert, Metric}).

init([Id]) ->
	put(inserted, 0),
    {ok, Opts} = application:get_env(errdb),
	Name = name(errdb_client, Id),
	{ok, Errdb} = errdb_client:start_link(Name, Opts),
	?INFO("~p is started.", [name(Id)]),
    {ok, #state{errdb=Errdb}}.

handle_call(Req, _From, State) ->
    ?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({insert, #metric{name=Type, dn=Dn, data=[]}}, State) ->
	?WARNING("empty metrix: ~p:~p", [Dn, Type]),
	{noreply, State};

handle_cast({insert, #metric{name=Type, dn=Dn, timestamp=Ts, data=Data}}, 
	#state{errdb = Errdb} = State) ->
	Inserted = get(inserted),
	put(inserted, Inserted+1),
	%TODO: FIX LATER
	%metrix_meas:check({Type, Dn, Ts, Metrics}),
	%metrixs obj and grp
	Grp = lists:last(string:tokens(atom_to_list(Type), ".")),
    Key = list_to_binary([unique_rdn(Dn), ":", Grp]),
    errdb_client:insert(Errdb, Key, Ts, Data),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%c
unique_rdn(<<"c=cn">> = Dn) ->
	mit_dn:rdn(Dn);	
%ap, ac, sw
unique_rdn(<<"cn=", _/binary>> = Dn) ->
	mit_dn:rdn(Dn);	
%host
unique_rdn(<<"host=", _/binary>> = Dn) ->
	Dn;

unique_rdn(Dn) ->
	[H1, H2|_] = binary:split(Dn, [<<",">>], [global]),
	<<H1/binary, ",", H2/binary>>.
