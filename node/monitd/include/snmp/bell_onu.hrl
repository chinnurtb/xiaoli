-define(BoardClass, [
    {"MANC-A", 'eponPort'},
    {"MANT-A", 'main'},
    {"MPOT-A", 'pstnPort'},
    {"MALT-A", 'adslPort'},
    {"MELT-B", 'feElcPort'},
    {"NALT-B", 'adslPort'},
    {"NPOT-A", 'feElcPort'}
 ]).


-define(BoardType,{boardtype, "1.3.6.1.4.1.637.61.1.23.3.1.2"}).
-define(BoardOperStatus,{operstatus, "1.3.6.1.4.1.637.61.1.23.3.1.8"}).

-define(BoardHardVersion,{hardversion, "1.3.6.1.4.1.3902.1015.2.1.2.2.1.1"}).
-define(BoardSoftVersion,{masterversiontag, "1.3.6.1.4.1.3902.1015.2.1.2.2.1.4"}).
-define(BoardCpuLoad,{cpuload, "1.3.6.1.4.1.3902.1015.2.1.1.3.1.9"}).
-define(BoardMemUsage,{memusage, "1.3.6.1.4.1.3902.1015.2.1.1.3.1.11"}).


-define(AdslProfile,{adslProfile, "1.3.6.1.4.1.637.61.1.39.3.7.1.1"}).

-define(MaxDownRate,{maxDownRate, "1.3.6.1.4.1.637.61.1.39.3.3.2.1.8"}).
-define(MaxUpRate,{maxUpRate, "1.3.6.1.4.1.637.61.1.39.3.3.2.1.9"}).
