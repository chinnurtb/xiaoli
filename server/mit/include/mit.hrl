%%%----------------------------------------------------------------------
%%% File    : mit.hrl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : MIT Header
%%% Created : 31 Mar 2010
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2010, www.opengoss.com 
%%%----------------------------------------------------------------------

%device types
-define(MIT_AP, 1).

-define(MIT_SW, 2).

-define(MIT_AC, 3).

-define(MIT_OMC, 4).

%mit changes
-define(MIT_ADDED, 1).

-define(MIT_DELETED, 2).

-define(MIT_UPDATED, 3).

-define(MIT_MOVED, 4).

%mit discovered
-define(UNDISCOVERED, 0).

-define(DISCOVERED, 1).

-define(REDISCOVER, 2).

%index: cn, parent
%uid: {type, id}
%cn: {type, cn}
-record(entry, {dn,  %entry dn
	cn, %common name of entry 
	ip, %ip address of the entry
	uid, %database id of entry
	text, %entry label
	class, %object class like '/top/ossIpDevice/ossIpSwitch"
	site, %the site that ac, sw belongs to
	parent, %example, fitap's parent is an ac
	oper_state, %oper state 
	statuses, %statues of entry
    manager, %manager of this entry
	attrs %attrs in db, should add cluster attr
}).

%used for ip lookup
-record(ip2dn, {ip, dn}).

