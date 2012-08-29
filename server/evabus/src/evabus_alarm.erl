%%---------------------------------------------------------------------- 
%%% File    : evabus_alarm.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : evabus alarm
%%% Created : 29 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(evabus_alarm).

-include("alarm.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([test/0]).

-export([from/1, record/1, remove/2, replace/2]).

remove(Attrs, Record) ->
	lists:foldl(fun(Attr, Acc) -> 
		lists:keydelete(Attr, 1, Acc)
	end, Record, Attrs).

replace({Attr, _Val} = Tup, Record) ->
	lists:keyreplace(Attr, 1, Record, Tup).

from(Record) ->
	#alarm{
		alarm_key = get_value(alarm_key, Record),
		alarm_name = atom(get_value(alarm_name, Record)),
		alarm_alias = get_value(alarm_alias, Record),
		alarm_type = get_value(alarm_type, Record),
		alarm_state = get_value(alarm_state, Record),
		perceived_severity = get_value(perceived_severity, Record),
		sequence_no = get_value(sequence_no, Record),
		summary = get_value(summary, Record),
		alarm_sender = get_value(alarm_sender, Record),
		sender_ip = get_value(sender_ip, Record),
		sender_alias = get_value(sender_alias, Record),
		sender_class = get_value(sender_class, Record),
		alarm_source = get_value(alarm_source, Record),
		source_alias = get_value(source_alias, Record),
		source_class = get_value(source_class, Record),
		alarm_priority = get_value(alarm_priority, Record, 2),
		occur_count = get_value(occur_count, Record, 1),
		probable_cause = get_value(probable_cause, Record),
		specific_problem = get_value(specific_problem, Record),
		clear_user = get_value(clear_user, Record),
		clear_time = get_value(clear_time, Record),
		clear_type = get_value(clear_type, Record),
		raised_time = get_value(raised_time, Record),
		first_occurrence = get_value(first_occurrence, Record),
		last_occurrence = get_value(last_occurrence, Record),
		order_state = get_value(order_state, Record),
		standard_id = get_value(standard_id, Record)
	}.

record(Alarm) when is_record(Alarm, alarm) ->
	Fields = record_info(fields, alarm),
	Record = 
	lists:map(fun(I) -> 
		{lists:nth(I, Fields), element(I+1, Alarm)}
	end, lists:seq(1, length(Fields))),
	lists:filter(fun field_filter/1, Record).

field_filter({_, undefined}) -> false;
field_filter({vars, _}) -> false;
field_filter({timestamp, _}) -> false;
field_filter({_, _}) -> true.
	
test() ->
	Alarm = #alarm{alarm_key = "alarm_key",
				alarm_name =  alarm_name,
				alarm_alias = "test_alarm",
				alarm_type = "qos",
				alarm_state = 2},
	Record = record(Alarm),
	io:format("~p", [Record]).

atom(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B));

atom(L) when is_list(L) ->
    list_to_atom(L).

