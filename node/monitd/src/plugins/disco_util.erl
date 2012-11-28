-module(disco_util).

-include_lib("elog/include/elog.hrl").

-export([lookup/5,transform_port/1,format/1,split/1,to_string/1,lists_delete_null/1,is_digit/1]).

-import(extbif, [to_binary/1, to_list/1]).


lookup(Vendor,Kind,Device_type,Status_type,Status_o) ->
	case ets:lookup(device_status_type,{to_binary(Vendor),to_binary(Kind),to_binary(Device_type),to_binary(Status_type),to_binary(Status_o)}) of
		[] ->
			?WARNING("unknow device_kind_devicetype_statustype_o: ~p,~p,~p,~p,~p", [Vendor,Kind,Device_type,Status_type,Status_o]),
			0;
		[{_,Status}] -> Status
	end.
format(Data) ->
	lists:foldl(fun({Name, Value}, Acc) ->
           case Acc of
         [] ->
                  lists:concat([Name, "=", to_list(Value)]);
              _ ->
                  lists:concat([Name, "=", to_list(Value), ",", Acc])
                  end
      end, [], Data).

to_string(T)  ->
    lists:flatten(io_lib:format("~p", [T])).

split(Args) ->
    lists:foldl(fun(Attr, Data) ->
        case string:tokens(Attr, "=") of
            [Name, Value|_] -> [{list_to_atom(Name), Value}|Data];
            _ -> Data
        end
    end, [], string:tokens(Args, ",")).

lists_delete_null(List)->
    [ X || X <- List, X=/=[] andalso X=/=[{en,false},{endesc,false}]].

is_digit(V) ->
    if V == "--" ->  0;
       true -> V
    end.


transform_port(Attrs) ->
	transform_port(Attrs, []).
transform_port([{ifIndex, Idx}|T], Acc) ->
    transform_port(T, [{port_index, Idx}|Acc]);
transform_port([{ifDescr, Descr}|T], Acc) ->
    transform_port(T, [{port_desc, to_binary(Descr)}|Acc]);
transform_port([{ifType, Type}|T], Acc) ->
    transform_port(T, [{port_type, Type}|Acc]);
transform_port([{ifAdminStatus, Status}|T], Acc) ->
    transform_port(T, [{admin_status, Status}|Acc]);
transform_port([{ifOperStatus, Status}|T], Acc) ->
    transform_port(T, [{oper_status, Status}|Acc]);
transform_port([{admin_status, Status}|T], Acc) ->
    transform_port(T, [{admin_status, Status}|Acc]);
transform_port([{oper_status, Status}|T], Acc) ->
    transform_port(T, [{oper_status, Status}|Acc]);
transform_port([{portType, Type}|T], Acc) ->
    transform_port(T, [{port_category, Type}|Acc]);
transform_port([{slot_no, SlotNo}|T], Acc) ->
    transform_port(T, [{slot_no, SlotNo}|Acc]);
transform_port([{port_no, PortNo}|T], Acc) ->
    transform_port(T, [{port_no, PortNo}|Acc]);
transform_port([{upassuredbw, Upassuredbw}|T], Acc) ->
    transform_port(T, [{upassuredbw, Upassuredbw}|Acc]);
transform_port([{upmaximumbw, Upmaximumbw}|T], Acc) ->
    transform_port(T, [{upmaximumbw, Upmaximumbw}|Acc]);
transform_port([{downmaximumbw, Downmaximumbw}|T], Acc) ->
    transform_port(T, [{downmaximumbw, Downmaximumbw}|Acc]);

transform_port([{temperature, Data}|T], Acc) ->
    transform_port(T, [{temperature, Data}|Acc]);
transform_port([{voltage, Data}|T], Acc) ->
    transform_port(T, [{voltage, Data}|Acc]);
transform_port([{e_current, Data}|T], Acc) ->
    transform_port(T, [{e_current, Data}|Acc]);
transform_port([{led_power, Data}|T], Acc) ->
    transform_port(T, [{led_power, Data}|Acc]);
transform_port([{received_power, Data}|T], Acc) ->
    transform_port(T, [{received_power, Data}|Acc]);
transform_port([{telno, Data}|T], Acc) ->
    transform_port(T, [{telno, Data}|Acc]);
transform_port([_|T], Acc) ->
    transform_port(T, Acc);
transform_port([], Acc) ->
    Acc.
