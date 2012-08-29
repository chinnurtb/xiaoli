%tag: avail | fitap | fitaps | iface
%from: trap | poll
%
-record(status, {type, dn, from, data}).

{status, {fitap, online}, dn, from, data}
%from: trap | poll

status, {fitap, online}, dn, from, attrs
status, {fitap, offline}, dn, from, attrs
status, fitap, maybe_offline, dn, from, attrs

