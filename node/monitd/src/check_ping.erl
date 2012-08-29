-module(check_ping).

-import(extbif, [to_list/1]).

-export([cmd/1, run/1]).

run(Host) ->
    retry(fun do_run/1, Host, 2).

retry(Fun, Host, Times) when Times =< 0 ->
    Fun(Host);

retry(Fun, Host, Times) when Times > 0 ->
    %io:format("retry ~p~n", [Times]),
    case Fun(Host) of
    {"PING CRITICAL", _} -> retry(Fun, Host, Times-1);
    Result -> Result
    end.

do_run(Host0) ->
    Host = to_list(Host0),
    Cmd = 
    case os:type() of
    {unix, linux} ->
        "ping -c 4 -w 8 " ++ Host;
    {unix, sunos} ->
        "ping -s -n " ++ Host ++ " 56 4"; 
    {unix, darwin} ->
        "ping -c 4 -t 8 " ++ Host;
    {unix, 'hp-ux'} ->
        "ping " ++ Host ++ " -n 4 -m 8";
    {unix, aix} ->
        "ping -c 4 -w 8 " ++ Host
    end,
    Output = monitd_oscmd:run(Cmd),
    PingLines=lists:reverse(string:tokens(Output,"\n")),
    case re:run(Output, "\\s(\\d+)% packet loss", [{capture, [1], list}]) of
    {match, [S]} ->
        [Summary0,Summary1|_]=PingLines,
        Pl = list_to_integer(S),
        if
        Pl == 100 ->
            {"PING CRITICAL", Summary0};
        Pl > 0 ->
            {"PING WARNING", lists:concat([Summary1,",",Summary0])};
        true -> % Pl 
            {"PING OK", lists:concat([Summary1,",",Summary0])}
        end;
    nomatch ->
        {"UNKNOWN", "ping error: " ++ Output}
    end.

cmd(Host0) ->
    Host = to_list(Host0),
    Cmd = 
    case os:type() of
    {unix, linux} ->
        "ping -c 4 -w 8 " ++ Host;
    {unix, sunos} ->
        "ping -s " ++ Host ++ " 56 4"; 
    {unix, darwin} ->
        "ping -c 4 -t 8 " ++ Host;
    {unix, 'hp-ux'} ->
        "ping " ++ Host ++ " -n 4 -m 8";
    {unix, aix} ->
        "ping -c 4 -w 8 " ++ Host
    end,
    monitd_oscmd:run(Cmd).
