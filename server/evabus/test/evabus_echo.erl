-module(evabus_echo).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-export([start/1]).

start(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

% Wait for incoming connections and spawn the echo loop when we get one.
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> loop(Socket, 0) end),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Socket, I) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            if 
            I == 0 ->
                gen_tcp:send(Socket, <<"0\r\n">>);
            true ->
                io:format("~p ~n", [Data])
            end,
            loop(Socket, I+1);
        {error, closed} ->
            ok
    end.
    
