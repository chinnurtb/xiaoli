
%======================================================================
% OLT版卡参数
%======================================================================
%业务板
-define(BoardType2,          {type,          "1.3.6.1.4.1.5875.800.3.9.2.1.1.2"}).
-define(BoardHardVersion2,   {hardversion,   "1.3.6.1.4.1.5875.800.3.9.2.1.1.3"}).
-define(BoardOperStatus2,    {operstatus,    "1.3.6.1.4.1.5875.800.3.9.2.1.1.5"}).
-define(BoardCpuLoad2,       {cpuload,       "1.3.6.1.4.1.5875.800.3.9.2.1.1.8"}).
-define(BoardMemUsage2,      {memusage,      "1.3.6.1.4.1.5875.800.3.9.2.1.1.9"}).
%主控板
-define(BoardType1,          {type,          "1.3.6.1.4.1.5875.800.3.9.8.1.1.1"}).
-define(BoardHardVersion1,   {hardversion,   "1.3.6.1.4.1.5875.800.3.9.2.1.1.3"}).
-define(BoardOperStatus1,    {operstatus,    "1.3.6.1.4.1.5875.800.3.9.8.1.1.4"}).
-define(BoardCpuLoad1,       {cpuload,       "1.3.6.1.4.1.5875.800.3.9.8.1.1.5"}).
-define(BoardMemUsage1,      {memusage,      "1.3.6.1.4.1.5875.800.3.9.8.1.1.6"}).
%======================================================================
% PON
%======================================================================
-define(PonIndex,           {index,         "1.3.6.1.4.1.5875.800.3.101.6.1.1"}).
-define(PonAdminStatus,     {adminstate,    "1.3.6.1.4.1.5875.800.3.9.3.4.1.4"}).
-define(PonOnlineStatus,    {onlinestate,   "1.3.6.1.4.1.5875.800.3.9.3.4.1.5"}).

-define(GeiIndex,           {index,         "1.3.6.1.4.1.5875.800.3.101.3.1.1"}).


%======================================================================
% ONU
%======================================================================
-define(OnuType,            {type,          "1.3.6.1.4.1.5875.800.3.10.1.1.5"}).
-define(OnuMac,             {mac,           "1.3.6.1.4.1.5875.800.3.10.1.1.10"}).
-define(OnuOnlineStatus,    {onlinestate,   "1.3.6.1.4.1.5875.800.3.10.1.1.11"}).
-define(OnuHardVersion,     {hardversion,   "1.3.6.1.4.1.5875.800.3.10.1.1.13"}).
-define(OnuSoftVersion,     {softversion,   "1.3.6.1.4.1.5875.800.3.10.1.1.12"}).
-define(OnudownAssuredBw,   {downassuredbw, "1.3.6.1.4.1.5875.800.3.3.1.1.5"}).
-define(OnudownMaximumBw,   {downmaximumbw, "1.3.6.1.4.1.5875.800.3.3.1.1.6"}).
-define(OnuupAssuredBw,     {upassuredbw,   "1.3.6.1.4.1.5875.800.3.3.1.1.3"}).
-define(OnuupMaximumBw,     {upmaximumbw,   "1.3.6.1.4.1.5875.800.3.3.1.1.4"}).
-define(OnuIp,              {ip,            "1.3.6.1.4.1.5875.800.3.17.1.1.11"}).


-define(GOnuPWD, {authpassword, "1.3.6.1.4.1.5875.800.3.10.1.1.8"}).

%ftth lan
-define(LanAdminstate,  {adminstate, "1.3.6.1.4.1.5875.800.3.9.3.1.1.3"}).
-define(LanOperstate,   {operstate, "1.3.6.1.4.1.5875.800.3.9.3.1.1.4"}).
%ftth pstn
-define(PstnAdminstate, {adminstate, "1.3.6.1.4.1.5875.800.3.9.3.2.1.4"}).

