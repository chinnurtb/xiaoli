%%%----------------------------------------------------------------------
%%% File    : mit_dict.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit dictionary for vendor and type
%%% Created : 18 Arg 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_dict).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1]).

-import(proplists, [get_value/2]).

-export([start_link/0, 
        lookup/1,
        transform/2,
        stop/0]).

-behavior(gen_server).

%%callback
-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3 ]).

-record(dict_code, {name, id}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

transform(Class, Attrs) ->
    transform(Class, Attrs, []).

transform(_Class, [], Acc) ->
    Acc;

transform(Class, [{vendor, Vendor}|Attrs], Acc) ->
    transform(Class, Attrs, [{device_manu, lookup(Vendor)}|Acc]);

transform(Class, [{type, Type}|Attrs], Acc) ->
    transform(Class, Attrs, [type_attr(Class, lookup(Type))|Acc]);

transform(Class, [Attr|Attrs], Acc) ->
    transform(Class, Attrs, [Attr|Acc]).

type_attr(ap, Id) ->
	{ap_type, Id};

type_attr(ac, Id) ->
    {ac_type, Id};

type_attr(sw, Id) ->
    {sw_type, Id};

type_attr(omc, Id) ->
	{omc_type, Id}.

lookup(CodeName) ->
    case mnesia:dirty_read(dict_code, to_binary(CodeName)) of
    [#dict_code{id = CodeId}] ->
		CodeId;
    [] -> 
		%create one??
		?ERROR("Cannot find dict_code for: ~s", [CodeName]),
		0
    end.

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case mnesia:system_info(extra_db_nodes) of
    [] -> %master node
        mnesia:create_table(dict_code, [{ram_copies, [node()]}, 
            {attributes, record_info(fields, dict_code)}]),
        handle_info(reload, state);
    _ -> %slave node
        ok
    end,
    mnesia:add_table_copy(dict_code, node(), ram_copies),
    ?INFO_MSG("mit_dict is started."),
    {ok, state}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(reload, State) ->
    {ok, Records} = emysql:select(dic_codes),
	[mnesia:dirty_write(#dict_code{
		name = get_value(code_name, Record),
		id = get_value(id, Record)}) || Record <- Records],
    erlang:send_after(300*1000, self(), reload),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

