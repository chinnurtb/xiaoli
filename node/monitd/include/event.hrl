
-record(event, {name, %event class name 
				sender, %ipaddr of device
				source, %event source
				evtkey, %deduplicate key
				severity, %severity
				summary, %event summary
				timestamp, %event timestamp
				manager, %manager that generate this event
				from, %syslog, trap or monitor
				trapoid, %only for trap
				vars=[]}).

