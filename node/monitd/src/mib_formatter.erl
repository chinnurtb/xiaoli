%%%----------------------------------------------------------------------
%%% File    : mib_formatter.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mib formatter
%%% Created : 27 Jul 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mib_formatter).

-include_lib("elog/include/elog.hrl").

-export([format/2,
        bps/1,
		lenstr/1,
        s/1, i/1,
        ip/1,
		mac/1,
        stroid/1,
        fitype/1,
		user_num/1,
		asctime/1
        ]).


-define(WKDAY, ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]).

-define(MONTH, ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
				"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]).

format(bps, V) ->
    bps(V);

format(stroid, V) ->
    stroid(V);

format(i, V) ->
    i(V);

format(lenstr, V) ->
	lenstr(V);

format(s, V) ->
    s(V);

format(fitype, V) ->
    fitype(V);

format(ip, V) ->
	ip(V);

format(asctime, V) ->
	asctime(V);

format(mac, V) ->
    mac(V);

format({map, List}, V) ->
    case dataset:get_value(V, List) of
    {value, V1} -> 
        V1;
    {false, _} -> 
        {value, V2} = dataset:get_value('_', List),   
        V2
    end.


bps(V) ->
    V * 8.

stroid(L) when is_list(L) ->
    list_to_binary(string:join([integer_to_list(I) || I <- L], "."));

stroid(V) ->
    throw({error_oid, V}).

ip(V) when is_list(V) and (length(V) == 4)->
   list_to_binary(string:join(io_lib:format("~.10B~.10B~.10B~.10B", V), "."));
ip(V) when is_list(V) ->
   list_to_binary(V);
ip(_) ->
   <<"">>.

i(V) when is_integer(V) ->
   V;
i(V) when is_list(V) ->
   extbif:to_integer(V);
i(V) when is_float(V) ->
   V;
i(V) ->
	?WARNING("badint: ~p", [V]),
	0.
s(noSuchInstance) ->
  <<"">>;
s(noSuchObject)->
  <<"">>;
s(V) when is_atom(V) ->
  list_to_binary(atom_to_list(V));
s(V) ->
  extbif:to_binary(V).

lenstr([_Len|S]) ->
	s(S).

fitype(Type) ->
    list_to_binary(["FIT-", string:strip(Type)]).

mac(L) when is_list(L) and (length(L) == 6) ->
	L1 = io_lib:format("~.16B~.16B~.16B~.16B~.16B~.16B", L),
    L2 = lists:map(fun(C) -> 
        case length(C) of
        2 -> C;
        1 -> "0" ++ C
        end
    end, L1),
    list_to_binary(string:join(L2, ":"));

mac(L) when is_list(L) ->
    list_to_binary(string:to_upper(L));

mac(B) when is_binary(B) ->
    B;

mac(V) ->
	?WARNING("errmac: ~p", [V]),
	<<"">>.

user_num(S) ->
    Tokens = string:tokens(S, "@."),
    if
    Tokens == [] -> S;
    true -> hd(Tokens)
    end.

%asctime-date = wkday SP date3 SP time SP 4DIGIT
%date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
%		  ; month day (e.g., Jun  2)
%time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
%		  ; 00:00:00 - 23:59:59
%wkday        = "Mon" | "Tue" | "Wed"
%		| "Thu" | "Fri" | "Sat" | "Sun"
%weekday      = "Monday" | "Tuesday" | "Wednesday"
%		| "Thursday" | "Friday" | "Saturday" | "Sunday"
%month        = "Jan" | "Feb" | "Mar" | "Apr"
%			| "May" | "Jun" | "Jul" | "Aug"
%			| "Sep" | "Oct" | "Nov" | "Dec"
asctime(V) when is_list(V) ->
	try
		V1 = string:strip(V, both, $\n),
		{_Wkday, Month, Day, Time, Year} = list_to_tuple(string:tokens(V1, " ")),
		D = list_to_integer(Day),
		M = index_of(Month, ?MONTH),
		Y = list_to_integer(Year),
		{H, Mm, S} = list_to_tuple([list_to_integer(S) || S <- string:tokens(Time, ":")]),
		extbif:timestamp({{Y, M, D}, {H, Mm, S}})
	catch
	_:Err ->
		?WARNING("asctime: ~p, err: ~p", [V, Err]),
		extbif:timestamp()
	end;
		
asctime(V) ->
	?WARNING("err_asctime: ~p", [V]),
	extbif:timestamp().

index_of(E, L) ->
	index_of(E, L, 0).
index_of(_E, [], I) ->
	I;
index_of(E, [E|_L], I) ->
	I;
index_of(E, [_|L], I) ->
	index_of(E, L, I+1).

