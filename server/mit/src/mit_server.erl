%%%----------------------------------------------------------------------
%%% File    : mit_server.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit server to handle entry
%%% Created : 05 July 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_server).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([start_link/0, emit/1]).

-behavior(gen_server).

%%callback
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {channel}).

%Rdn or Dn? whats' the fuck?


start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

emit(Event) ->
	gen_server2:cast(?MODULE, {emit, Event}).

init([]) ->
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    ?INFO_MSG("mit_server is started...[ok]"),
    {ok, #state{channel = Channel}}.

open(C) ->
	{ok, Channel} = amqp:open_channel(C),
    {ok, Queue} = amqp:queue(Channel, atom_to_list(node())),
    amqp:topic(Channel, "mit.server"),
    amqp:bind(Channel, "mit.server", Queue, "#"),
	amqp:consume(Channel, Queue),
	Channel.

handle_call(Req, _From, State) ->
    {stop, {badreq, Req}, State}.

handle_cast({emit, Event}, #state{channel = Channel} = State) ->
	amqp:publish(Channel, <<"eva.event">>, term_to_binary(Event), "event"),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

handle_info({deliver, _RoutingKey, _, Payload}, State) ->
    Fun = fun() ->
        try store(binary_to_term(Payload))
        catch
        _:Err -> 
            ?ERROR("~p", [Err]),
            ?ERROR("~p", [erlang:get_stacktrace()])
        end 
    end,
    worker_pool:submit(Fun),
    {noreply, State};

handle_info({amqp, disconnected}, State) ->
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn}, State) ->
	{noreply, State#state{channel = open(Conn)}};

handle_info(Info, State) ->
    {stop, {badinfo, Info}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%olt, router, switch
store({node, Cat, Dn, Attrs}) ->
    Mod = mit_meta:module(Cat),
    Mod:update(Dn, Attrs);

store({nodes, Cat, Dn, List}) ->
    Mod = mit_meta:module(Cat),
    Mod:add({nodes, Dn, List});

%boards
store({boards, Cat, Dn, Boards}) ->
    Mod = mit_meta:module(Cat),
    Mod:add({boards, Dn, Boards});

%ports
store({ports, Cat, Dn, Ports}) ->
    Mod = mit_meta:module(Cat),
    Mod:add({ports, Dn, Ports});

store(BadTerm) ->
    ?ERROR("badterm: ~n~p", [BadTerm]).


