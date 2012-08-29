-module(mit_ctl).

-include("mit.hrl").

should_dispath(#entry{class = <<"/top/ossSite">>}) ->
    false;
should_dispath(#entry{oper_state = 2, class = <<"/top/ossIpDevice/ossWirelessAccessPoint">>}) ->
    false;
should_dispath(_Entry) ->
    true.
