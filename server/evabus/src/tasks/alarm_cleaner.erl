-module(alarm_cleaner).

-include_lib("elog/include/elog.hrl").

-export([clean/1]).

clean(Aging) ->
	AgedTime = extbif:datetime(extbif:timestamp() - Aging*60),
	Where = {'and', {perceived_severity, 0}, {'<', clear_time, {datetime, AgedTime}}},
	{ok, Records} = emysql:select(fault_events, Where),
	[emysql:insert(fault_histories, evabus_alarm:remove([id], Record)) || Record <- Records],
	Ids = [proplists:get_value(id, Record) || Record <- Records],
	emysql:delete(fault_events, {'in', id, Ids}).

