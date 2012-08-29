%%---------------------------------------------------------------------- 
%%% File    : evabus_event.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : evabus event
%%% Created : 29 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(evabus_event).

-include("event.hrl").

-export([record/1]).

record(#event{name = Name,
			  sender = Sender,
			  source = Source,
			  evtkey = EvtKey,
			  vars = Vars}) ->
	[{name, Name}, 
	{sender, Sender}, 
	{source, Source}, 
	{evtkey, EvtKey} | Vars].
	
