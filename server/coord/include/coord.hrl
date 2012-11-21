%%%----------------------------------------------------------------------
%%% File    : coord.hrl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : coord header file.
%%% Created : 29 Arg 2008
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-define(VERSION, "2.0").

-record(shard, {node, queue, count}).

-record(dispatch, {dn, shard, tref}).

-record(presence, {node, class, status, vsn, tref, summary, metrics}).

