-module(evabus_nmssvr).

-export([start/0]).

start() -> 
	{ok, Listen} = gen_tcp:listen(2345, [binary,
        {packet, line}, 
		{reuseaddr, true}, 
		{active, true}]), 
	{ok, Socket} = gen_tcp:accept(Listen), 
	gen_tcp:close(Listen), 
	loop(Socket). 

loop(Socket) -> 
	receive 
	{tcp, Socket, <<"HELLO\r\n">>} -> 
		io:format("Hello got!", []), 
		gen_tcp:send(Socket, integer_to_list(extbif:timestamp())),
		loop(Socket); 
	{tcp, Socket, Bin} -> 
		io:format("Server recved: ~p~n", [Bin]), 
		loop(Socket); 
	{tcp_closed, Socket} -> 
		io:format("Server socket closed~n") 
end.
