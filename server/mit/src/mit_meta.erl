%%%----------------------------------------------------------------------
%%% File    : mit_meta.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit metadata 
%%% Created : 18 Arg 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_meta).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([start_link/0,
         lookup/1]).

-behavior(gen_server).

%%callback
-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3 ]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

lookup(Key) when is_tuple(Key) ->
    [Meta] = mnesia:dirty_read(mit_meta, Key),
    Meta.

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case mit:mode() of
    master -> %master node
        mnesia:create_table(meta, [{ram_copies, [node()]}, 
            {attributes, record_info(fields, meta)}]),
        handle_info(reload, state);
    slave -> %slave node
        ok
    end,
    mnesia:add_table_copy(meta, node(), ram_copies),
    ?INFO_MSG("mit_meta is started."),
    {ok, state}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(reload, State) ->
    {ok, Categories} = epgsql:select(main, categories, 
        [id, name, alias], {'and', {obj, "node"}, {is_valid, 1}}), 
    {ok, Vendors} = epgsql:select(main, vendors,
        [id, name, alias], {is_valid, 1}),
    {ok, Models} = epgsql:select(main, models,
        [id, name, alias], {is_valid, 1}),
    MetaList = [meta(category, Cat) || Cat <- Categories] ++
               [meta(vendor, Vendor) || Vendor <- Vendors] ++
               [meta(model, Model) || Model <- Models],
	[mnesia:dirty_write(Meta) || Meta <- MetaList],
    erlang:send_after(600*1000, self(), reload),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

meta(Type, Record) ->
    Id = get_value(id, Record),
    Name = get_value(name, Record),
    Alias = get_value(alias, Record),
    #meta{key={Type, Id}, name = Name, alias = Alias}.


