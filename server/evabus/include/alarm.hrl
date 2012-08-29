
-record(alarm, {alarm_key,
			    alarm_name,
			    alarm_alias,
			    alarm_type,
			    alarm_state,
			    perceived_severity,
				sequence_no,
			    summary,
			    alarm_sender,
			    sender_ip,
			    sender_alias,
			    sender_class,
			    alarm_source,
			    source_alias,
			    source_class,
			    alarm_priority = 2,
			    occur_count = 1,
			    probable_cause,
			    specific_problem,
			    additional_information,
			    clear_user,
			    clear_time,
			    clear_type,
			    raised_time,
			    first_occurrence,
			    last_occurrence,
				order_state,
			    standard_id,
				timestamp, %timestamp used to compare
				manager, %from event manager
				vars}). %vars from event


