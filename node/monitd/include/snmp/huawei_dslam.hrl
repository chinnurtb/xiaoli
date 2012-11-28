%dslam huawei

-define(AdslLineConfProfileName,      {adslLineConfProfileName,       "1.3.6.1.2.1.10.94.1.1.1.1.4"}).
-define(AdslLineConfType,             {adslLineConfType,       "1.3.6.1.2.1.10.94.1.1.1.1.2"}).


-define(HwMusaBoardSlotDesc,            {boardtype,              "1.3.6.1.4.1.2011.2.6.7.1.1.2.1.7"}).
-define(HwMusaBoardActivedPortNum,      {pnum,                   "1.3.6.1.4.1.2011.2.6.7.1.1.2.1.12"}).
-define(HwMusaBoardOnlineState,         {operstatus,             "1.3.6.1.4.1.2011.6.3.3.2.1.8"}).
-define(HwMusaBoardCpuRate,             {cpuload,                "1.3.6.1.4.1.2011.2.6.7.1.1.2.1.5"}).
-define(HwMusaBoardRamUseRate,          {memusage,               "1.3.6.1.4.1.2011.2.6.7.1.1.2.1.6"}).
-define(HwMusaBoardTemperature,         {temperature,            "1.3.6.1.4.1.2011.2.6.7.1.1.2.1.10"}).
-define(hwSlotWorkMode,                 {workMode,               "1.3.6.1.4.1.2011.6.3.3.2.1.6"}).

-define(BoardEntry, [
        ?HwMusaBoardSlotDesc,
        ?HwMusaBoardOnlineState
        ]).



