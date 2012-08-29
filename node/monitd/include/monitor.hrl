-record(monitor_task, 
	{id,
	 period,
	 mod,
	 dn,
	 args,
	 tref=undefined,
	 duration,
	 latency,
	 handler, %function
	 created_at,
	 last_sched_at,
	 next_sched_at}). 

