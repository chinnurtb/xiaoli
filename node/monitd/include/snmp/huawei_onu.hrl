-define(BoardType,[
    {"H821ASMB", 'pstnPort'},
    {"H821ASNB", 'pstnPort'},
    {"H821EPFA", 'feElcPort'},
    {"H821EPFB", 'feElcPort'},
    {"H821EPUA", 'main'},
    {"H821EPUB", 'feElcPort'},
    {"H821EPFB", 'feElcPort'},
    {"H822ASMB", 'pstnPort'},
    {"H822ASNB", 'pstnPort'},
    {"H831CCUB", 'main'},
    {"H831PAIA", 'power'},
    {"H835ADLE", 'adslPort'},
    {"H838ASRB", 'pstnPort'},
    {"H838ASPB", 'pstnPort'},
    {"H801ASRB", 'pstnPort'},
    {"H808ADLE", 'adslPort'},
    {"H801MCUA", 'main'}
]).

-define(hwPstnPTPSrvState,{hwPstnPTPSrvState, "1.3.6.1.4.1.2011.6.113.1.2.1.2"}).
-define(hwPstnCTPSrvState,{hwPstnCTPSrvState, "1.3.6.1.4.1.2011.6.112.1.2.1.2"}).

-define(hwPstnCTPTelNo,{hwPstnCTPTelNo, "1.3.6.1.4.1.2011.6.112.1.1.1.4"}).

-define(hwH248MedGwyOperStatus,{hwH248MedGwyOperStatus, "1.3.6.1.4.1.2011.6.36.1.1.1.14"}).


-define(hwNarrowIp,{narrowIp, "1.3.6.1.4.1.2011.6.36.1.1.1.4"}).

% pon info
-define(OnuTxPower, {txPower, "1.3.6.1.4.1.2011.6.119.1.1.1.1.14"}).
-define(OnuRxPower, {rxPower, "1.3.6.1.4.1.2011.6.119.1.1.1.1.15"}).
-define(OnuTemperature, {temperature, "1.3.6.1.4.1.2011.6.119.1.1.1.1.16"}).
-define(OnuCurrent, {current, "1.3.6.1.4.1.2011.6.119.1.1.1.1.17"}).
-define(OnuVoltage, {voltage, "1.3.6.1.4.1.2011.6.119.1.1.1.1.18"}).
