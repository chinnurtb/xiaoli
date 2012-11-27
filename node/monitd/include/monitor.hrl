
-record(timeperiod, {
    id,
    name,
    minute, %-- not used
    hour, % '*' -- cron hour
    dayofmonth, % '*' or 1,2,3...31
    month, % '*' or 1,2,3,...12
    dayofweek, % '*' or 0,1,2...6
    start_at, % timestamp
    end_at % timestamp
}).

-record(monitor_task, 
	{id,
	 period,
	 mod,
	 dn,
     node,
	 args,
	 tref=undefined,
	 duration,
	 latency,
	 handler,
	 created_at,
	 last_sched_at,
	 next_sched_at}). 

