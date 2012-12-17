-module(mit_onu).

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-import(proplists, [get_value/2]).

-export([load/0, load/1, update/2, add/1]).

-define(TAB, node_onus).

-define(PORT_TAB, port_onus).

load() ->
    mit_node:load(onu).

load(Dn) ->
    mit_node:load(Dn).

add({_, _Dn, []}) ->
    ignore;

add({nodes, Dn, Onus}) ->
    {ok, Node} = mit:lookup(Dn),
    insert(Node, Onus);

add({ports, Dn, [Port|_] = Ports}) ->
    ?INFO("~p", [Port]),
    {ok, Node} = mit:lookup(Dn),
    mit_port:insert(?PORT_TAB, Node, Ports).
    
update(Dn, Attrs) ->
    mit_node:update(?TAB, Dn, Attrs).

insert(#mit_node{id=OltId}=Node, Onus) ->
    {ok, Records} = epgsql:select(main, node_onus, {oltid, OltId}),
    OldOnus = [{get_value(onuidx, Record), Record} || Record <- Records],

    NewFun = fun(_Idx, Onu) ->
        enrich(modelid, 
            enrich(Node, 
                enrich(time, Onu)))
    end,

    mit_db:merge(node_onus, Onus, OldOnus, NewFun, false).


enrich(#mit_node{dn=OltDn, id=OltId, areaid=AreaId, vendorid=VdId}=Node, Onu) 
    when is_record(Node, mit_node) -> 
    VendorId = 
    case get_value(vendor, Onu) of
    undefined -> 
        VdId;
    Vendor ->
        mit_meta:vendor(b2a(Vendor))
    end,
    [{dn, onudn(OltDn, Onu)},
    {oltid, OltId},
    {controller_id, OltId},
    {category_id, mit_meta:category(onu)},
    {vendor_id, VendorId},
    {area_id, AreaId} 
    | lists:keydelete(vendor, 1, Onu)];

enrich(modelid, Onu) ->
    Model = get_value(model, Onu),
    ModelId = modelid(Model, Onu),
    [{model_id, ModelId} | lists:keydelete(model, 1, Onu)];

enrich(time, Onu) ->
    [{created_at, {datetime, {date(), time()}}}|Onu].

modelid(undefined, Onu) ->
    Onu;
modelid(Model, Onu) ->
    case mit_meta:model(b2a(Model)) of
    undefined ->
        mit_meta:store([{category_id, get_value(categor_id, Onu)},
                        {vendor_id, get_value(vendor_id, Onu)},
                        {name, Model}, {alias, Model}]);
    Id when is_integer(Id) -> 
        Id
    end.

b2a(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B)).

onudn(OltDn, Onu) ->
    iolist_to_binary([OltDn, ",onu=", 
        integer_to_list(get_value(onuidx, Onu))]).

