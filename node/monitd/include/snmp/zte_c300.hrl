%board
-define(CfgMainType,    {cfgmaintype,       "1.3.6.1.4.1.3902.1015.2.1.1.3.1.2"}).
-define(ActMainType,    {actmaintype,       "1.3.6.1.4.1.3902.1015.2.1.1.3.1.3"}).
-define(ActType,        {cardacttype,       "1.3.6.1.4.1.3902.1015.2.1.1.3.1.4"}).
-define(OperStatus,     {operstatus,        "1.3.6.1.4.1.3902.1015.2.1.1.3.1.5"}).
-define(AdminStatus,    {adminstatus,       "1.3.6.1.4.1.3902.1015.2.1.1.3.1.6"}).
-define(CpuLoad,        {cpuload,           "1.3.6.1.4.1.3902.1015.2.1.1.3.1.9"}).
-define(MemUsage,       {memusage,          "1.3.6.1.4.1.3902.1015.2.1.1.3.1.11"}).
-define(StandbyStatus,  {standbystatus,     "1.3.6.1.4.1.3902.1015.2.1.1.3.1.13"}).
-define(Lockstatus,     {lockstatus,        "1.3.6.1.4.1.3902.1015.2.1.1.3.1.18"}).

%CardVersion
-define(HardVersion,        {hardversion,       "1.3.6.1.4.1.3902.1015.2.1.2.2.1.1"}).
-define(VersionFileType,    {masterversiontype, "1.3.6.1.4.1.3902.1015.2.1.2.2.1.3"}).
-define(VersionTag,         {masterversiontag,  "1.3.6.1.4.1.3902.1015.2.1.2.2.1.4"}).
-define(BootromFileType,    {bootromfiletype,   "1.3.6.1.4.1.3902.1015.2.1.2.2.1.8"}).
-define(BootromTag,         {bootromtag,        "1.3.6.1.4.1.3902.1015.2.1.2.2.1.9"}).
-define(BootromBuildTime,   {bootrombuildtime,  "1.3.6.1.4.1.3902.1015.2.1.2.2.1.11"}).


-define(zxAnEponIfOltHCInOctets,        {ifInOctets,            "1.3.6.1.4.1.3902.1015.1010.5.4.1.2"}).
-define(zxAnEponIfOltHCInUcastPkts,     {ifInUcastPkts,         "1.3.6.1.4.1.3902.1015.1010.5.4.1.8"}).
-define(zxAnEponIfOltHCInMulticastPkts, {ifInMulticastPkts,     "1.3.6.1.4.1.3902.1015.1010.5.4.1.10"}).
-define(zxAnEponIfOltHCInBroadcastPkts, {ifInBroadcastPkts,     "1.3.6.1.4.1.3902.1015.1010.5.4.1.11"}).

-define(zxAnEponIfOltHCOutOctets,       {ifOutOctets,           "1.3.6.1.4.1.3902.1015.1010.5.4.1.17"}).
-define(zxAnEponIfOltHCOutUcastPkts,    {ifOutUcastPkts,        "1.3.6.1.4.1.3902.1015.1010.5.4.1.23"}).
-define(zxAnEponIfOltHCOutMulticastPkts,{ifOutMulticastPkts,    "1.3.6.1.4.1.3902.1015.1010.5.4.1.25"}).
%-define(zxAnEponIfOltHCOutBroadcastPkts,{ifOutBroadcastPkts,    "1.3.6.1.4.1.3902.1015.1010.5.4.1.26"}).

-define(zxAnEponIfXOltTable, [
        ?zxAnEponIfOltHCInOctets,
        ?zxAnEponIfOltHCInUcastPkts,
        ?zxAnEponIfOltHCInMulticastPkts,
        ?zxAnEponIfOltHCInBroadcastPkts,
        ?zxAnEponIfOltHCOutOctets,
        ?zxAnEponIfOltHCOutUcastPkts,
        ?zxAnEponIfOltHCOutMulticastPkts
%        ?zxAnEponIfOltHCOutBroadcastPkts
        ]).



