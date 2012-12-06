%%%----------------------------------------------------------------------
%%% File    : mit_area.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Mit area
%%% Created : 03 Dec. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_area).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-include_lib("epgqueue/include/pgqueue.hrl").

-import(proplists, [get_value/2]).

-behavior(gen_server).

-export([start_link/0,
         lookup/1]).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-define(AREA_SQL,
    "select t.id, t.dn, t.name, t.alias, t.area_type as type, "
    "t2.dn as parent "
    "from areas t "
    "left join areas t2 on t.parent_id = t2.id").

-record(state, {}).

lookup(Dn) when is_binary(Dn) ->
    case mnesia:dirty_read(area, iolist_to_binary(Dn)) of
    [Area] -> {ok, Area};
    [] -> {false, Dn}
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case mit:mode() of
    master ->
        %clear mit change queue
        epgqueue:clear('mit.area'),
        mnesia:create_table(area, [
            {ram_copies, [node()]}, 
            {index, [id, parent]}, 
            {attributes, record_info(fields, area)}]),
        {ok, Areas} = epgsql:squery(main, ?AREA_SQL++";"),
        ?INFO("load ~p areas.", [length(Areas)]),
        [mnesia:dirty_write(record(A)) || A <- Areas],
        epgqueue:subscribe('mit.area', self());
    slave ->
        mnesia:add_table_copy(area, node(), ram_copies)
    end,
    ?INFO_MSG("mit_area is started...[ok]"),
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
    {stop, {badreq, Req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(#pgqevent{name = <<"area.inserted">>, data=Dn}, State) ->
    ?INFO("area.inserted: ~s", [Dn]),
    case load(Dn) of
    {ok, Area} -> 
        mnesia:dirty_write(Area);
    {error, Err} ->
        ?ERROR("~p", [Err])
    end,
    {noreply, State};

handle_info(#pgqevent{name = <<"area.updated">>, data=Dn}, State) ->
    ?INFO("area.inserted: ~s", [Dn]),
    case load(Dn) of
    {ok, Area} -> 
        mnesia:dirty_write(Area);
    {error, Err} ->
        ?ERROR("~p", [Err])
    end,
    {noreply, State};

handle_info(#pgqevent{name = <<"area.deleted">>, data=Dn}, State) ->
    ?INFO("area.deleted: ~s", [Dn]),
    mnesia:dirty_delete(Dn),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {badinfo, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
load(Dn) -> 
    Where = iolist_to_binary([" where dn='", Dn, "';"]),
    case epgsql:squery(main, iolist_to_binary([?AREA_SQL, Where])) of
    {ok, [Area]} -> {ok, record(Area)};
    {error, Error} -> {error, Error}
    end.

record(A) ->
    #area{dn = get_value(dn, A),
        id = get_value(id, A),
        cityid = get_value(cityid, A),
        parent = get_value(parent, A),
        name = get_value(name, A),
        alias = get_value(alias, A),
        type = get_value(type, A)}.

