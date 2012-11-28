-define(zxDslBoardType,{boardtype, "1.3.6.1.4.1.3902.1004.3.1.1.6.1.4"}).
-define(zxDslBoardAdminStatus,{adminstatus, "1.3.6.1.4.1.3902.1004.3.1.1.6.1.8"}).
-define(zxDslBoardOperStatus,{operstatus, "1.3.6.1.4.1.3902.1004.3.1.1.6.1.9"}).
-define(zxDslBoardHardVersion,{hardversion, "1.3.6.1.4.1.3902.1004.3.1.1.6.1.10"}).
-define(zxDslBoardSoftVersion,{masterversiontag, "1.3.6.1.4.1.3902.1004.3.1.1.6.1.11"}).
-define(zxDslBoardCpuLoad,{cpuload, "1.3.6.1.4.1.3902.1004.3.1.1.6.1.14"}).
-define(zxDslBoardMemUsage,{memusage, "1.3.6.1.4.1.3902.1004.3.1.1.6.1.16"}).

-define(PortStatus, {portStatus, "1.3.6.1.4.1.3902.701.1.1.10.1.3.1.3"}).

-define(BoardType,[
    {307, "ASTEB", 'adslPort'},
    {2867, "ASTEC", 'adslPort'},
    {547, "M24E", 'feElcPort'},
    {803, "MSEBA", 'feElcPort'},
    {1059, "MSEBB", 'feElcPort'},
    {2067, "SCCBk",'geElcPort'},
    {2323, "GNI", 'geElcPort'},
    {451, "ATLC", 'pstnPort'},
    {1219, "ATLA", 'pstnPort'},
    {1475, "V24B", 'pstnPort'},
    {2499, "ATLCI", 'pstnPort'},
    {2755, "ATLCZ", 'pstnPort'},
    {3011, "V24C", 'pstnPort'}
]).


%================== F820 ========================
-define(F820BoardType, [
    {"MS8E", 'feElcPort'},
    {"EPUA", 'feElcPort'},
    {"V16B", 'pstnPort'},
    {"V08B", 'pstnPort'}
 ]).

 -define(F820PortStatus, {portStatus, "1.3.6.1.4.1.3902.1015.1820.1.3.7.1.4"}).

 -define(F820NarrowIp,  {narrowIp, "1.3.6.1.4.1.3902.1015.1820.1.1.2.1.1"}).
 -define(ZteNarrowIp,  {narrowIp, "1.3.6.1.4.1.3902.1004.3.1.8.5.1.5"}).


-define(BoardType2,{boardtype, "1.3.6.1.4.1.3902.1015.2.1.1.3.1.4"}).
-define(BoardAdminStatus,{adminstatus, "1.3.6.1.4.1.3902.1015.2.1.1.3.1.6"}).
-define(BoardOperStatus,{operstatus, "1.3.6.1.4.1.3902.1015.2.1.1.3.1.5"}).
-define(BoardHardVersion,{hardversion, "1.3.6.1.4.1.3902.1015.2.1.2.2.1.1"}).
-define(BoardSoftVersion,{masterversiontag, "1.3.6.1.4.1.3902.1015.2.1.2.2.1.4"}).
-define(BoardCpuLoad,{cpuload, "1.3.6.1.4.1.3902.1015.2.1.1.3.1.9"}).
-define(BoardMemUsage,{memusage, "1.3.6.1.4.1.3902.1015.2.1.1.3.1.11"}).

% pon info
-define(TxPower, {txPower, "1.3.6.1.4.1.3902.1015.1010.1.1.1.29.1.4"}).
-define(RxPower, {rxPower, "1.3.6.1.4.1.3902.1015.1010.1.1.1.29.1.5"}).
-define(Temperature, {temperature, "1.3.6.1.4.1.3902.1015.1010.1.1.1.29.1.1"}).
-define(Current, {current, "1.3.6.1.4.1.3902.1015.1010.1.1.1.29.1.3"}).
-define(Voltage, {voltage, "1.3.6.1.4.1.3902.1015.1010.1.1.1.29.1.2"}).
