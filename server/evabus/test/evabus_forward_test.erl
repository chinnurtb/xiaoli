-module(evabus_forward_test).

-define(TCP_OPTIONS, [binary, 
    {packet, 0}, 
    {active, true}, 
    {reuseaddr, true}, 
    {send_timeout, 3000}]).

-export([start/2]).

start(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, ?TCP_OPTIONS, 3000),
    gen_tcp:send(Socket, "HELLO\r\n"),
    loop(Socket).


% Echo back whatever data we receive on Socket.
loop(Socket) ->
    io:format("send heartbeat to: ~p~n", [Socket]),
    gen_tcp:send(Socket, "heartbeat\r\n"),
    timer:sleep(5000),
    loop(Socket).
    
    
