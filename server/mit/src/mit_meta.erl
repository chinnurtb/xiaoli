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
         category/1,
         vendor/1,
         model/1,
         module/1]).

-export([store/1]).

-behavior(gen_server).

%%callback
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

category(Id) when is_integer(Id) -> 
    lookup({category, Id});
category(Name) when is_atom(Name) ->
    lookup({category, Name}).

vendor(Id) when is_integer(Id) ->
    lookup({vendor, Id});
vendor(Name) when is_atom(Name) ->
    lookup({vendor, Name}).

model(Id) when is_integer(Id) ->
    lookup({model, Id});
model(Name) when is_atom(Name) ->
    lookup({model, Name}).

module(Name) when is_atom(Name) ->
    lookup({module, Name}).

lookup(Key) ->
    case mnesia:dirty_read(meta, Key) of
    [Meta] -> Meta#meta.val;
    [] -> undefined
    end.

%ONLY Model?
store(Model) ->
    gen_server:call(?MODULE, {store, Model}).

init([]) ->
    case mit:mode() of
    master -> %master node
        mnesia:create_table(meta, [{ram_copies, [node()]}, 
            {attributes, record_info(fields, meta)}]),
        {ok, Modules} = application:get_env(modules),
        [mnesia:dirty_write(#meta{key={module, Cat}, val=Mod})
            || {Cat, Mod} <- Modules],
        handle_info(reload, state);
    slave -> %slave node
        ok
    end,
    mnesia:add_table_copy(meta, node(), ram_copies),
    ?INFO_MSG("mit_meta is started...[ok]"),
    {ok, state}.

handle_call({store, Model}, _From, State) ->
    Reply =
    case model(b2a(get_value(name, Model))) of
    ModId when is_integer(ModId) -> 
        ModId;
    undefined ->
        Id = next_modid(),
        Record = [{id, Id}, {is_valid, 1} | Model],
        case epgsql:insert(main, models, Record) of
        {error, _} -> undefined;
        _ -> cache(model, Record), Id
        end
    end,
    {reply, Reply, State};

handle_call(Req, _From, State) ->
    {stop, {badreq, Req}, State}.

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

handle_info(reload, State) ->
    [cache(category, Cat) || Cat <- load(category)],
    [cache(vendor, Vendor) || Vendor <- load(vendor)],
    [cache(model, Model) || Model <- load(model)],
    erlang:send_after(600*1000, self(), reload),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {badinfo, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

load(category) ->
    {ok, Categories} = epgsql:select(main, categories, [id, name],
        {'and', {obj, "node"}, {is_valid, 1}}), 
    Categories;

load(vendor) ->
    {ok, Vendors} = epgsql:select(main, vendors, [id, name], {is_valid, 1}),
    Vendors;

load(model) ->
    {ok, Models} = epgsql:select(main, models, [id, name], {is_valid, 1}),
    Models.

cache(Type, Record) ->
    Id = get_value(id, Record),
    Name = b2a(get_value(name, Record)),
    mnesia:dirty_write(#meta{key={Type, Id}, val=Name}),
    mnesia:dirty_write(#meta{key={Type, Name}, val=Id}).

next_modid() ->
    {ok,[Next]} = epgsql:squery(main, "select nextval('models_id_seq');"),
    get_value(nextval, Next).


b2a(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B)).

