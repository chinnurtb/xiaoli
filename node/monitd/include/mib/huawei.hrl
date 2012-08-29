%%ac:WS6603 ap:WA631
%%----------------------------------------------------------------------
%% ac discover -- ac配置(发现)
%%----------------------------------------------------------------------
-define(Ac,[
     {softVersion,     "1.3.6.1.4.1.2011.6.3.1.3.0"}
     %{acName,          ""}
     %{portalServerURL, ""}
]).

-define(AcRadio,[
     %{radiusAuthServerIPAdd, ""},
     %{radiusAuthServerPort,  ""}
]).

-define(AcVlan,[
     {ipPoolName,      "1.3.6.1.4.1.2011.6.8.1.1.1.2"},
     {ipPoolStartAddr, "1.3.6.1.4.1.2011.6.8.1.3.1.3"},
     {ipPoolStopAddr,  "1.3.6.1.4.1.2011.6.8.1.3.1.4"}
]).

-define(Ap,[
     {apName,       "1.3.6.1.4.1.2011.6.139.2.6.1.1.7" },
     {apSerialNo,   "1.3.6.1.4.1.2011.6.139.2.6.1.1.6" },
     {apType,       "1.3.6.1.4.1.2011.6.139.2.6.1.1.2" },
     {apIp,         "1.3.6.1.4.1.2011.6.139.2.6.1.1.15"},
     {apMask,       "1.3.6.1.4.1.2011.6.139.2.6.1.1.16"},
     {apMac,        "1.3.6.1.4.1.2011.6.139.2.6.1.1.5" },
     {apSoftVersion,"1.3.6.1.4.1.2011.6.139.2.6.1.1.9" }
]).

%----------------------------------------------------------------------
%% ac monet --ac 性能采集 
%%----------------------------------------------------------------------
-define(Acinfo, [
     %{cpuRTUsage,               ""},
     %{memRTUsage,               ""},
     %{dHCPReqTimes,             ""},
     %{dHCPReqSucTimes,          ""},
     %{onlineNum,                ""},
     %{authNum,                  ""},
     %{maxNum,                   ""},
     %{normalNum,                ""}, 
     %{deauthNum,                ""}, 
     %{authReqNum,               ""},
     %{authSucNum,               ""},
     %{dHCPIpPoolUsage,          ""},
     %{portalChallengeReqCount,  ""},
     %{portalChallengeRespCount, ""},
     %{portalAuthReqCount,       ""},
     %{portalAuthRespCount,      ""},
     %{bandWidth,                ""},
     %{accReqNum,                ""},
     %{accSucNum,                ""},
     %{radiusAvgDelay,           ""},
     %{leaveReqPkts,             ""},
     %{leaveRepPkts,             ""},
     %{leaveReqCount,            ""},
     %{leaveRepCount,            ""},
     %{radiusReqPkts,            ""},
     %{radiusRepPkts,            ""},
]).
%%----------------------------------------------------------------------
%% ap discover -- ap配置(发现)
%%----------------------------------------------------------------------
-define(ApRadio,[
     {radioPeriod,  "1.3.6.1.4.1.2011.6.139.3.1.1.16"},
     {radioDtim,    "1.3.6.1.4.1.2011.6.139.3.1.1.15"},
     {radioRts,     "1.3.6.1.4.1.2011.6.139.3.1.1.10"},
     {radioSlice,   "1.3.6.1.4.1.2011.6.139.3.1.1.11"},
     {radioModel,   "1.3.6.1.4.1.2011.6.139.3.1.1.4" }
     %{radioChannel, "1.3.6.1.4.1.2011.6.139.3.2.1.5"},
     %{radioPower,   "1.3.6.1.4.1.2011.6.139.3.2.1.7"}
  ]).

-define(ApWirelessIf,[
     %{ifDescr,       ""},
     %{ifType,        ""},
     %{ifMtu,         ""},
     %{ifSpeed,       ""},
     %{ifPhysAddress, ""},
     %{ifAdminStatus, ""},
     %{ifOperStatus,  ""}
 ]).

-define(ApState, [
    {apState, "1.3.6.1.4.1.2011.6.139.2.6.1.1.8"}
]).

%%----------------------------------------------------------------------
%% ap monet --ap 性能采集 
%%----------------------------------------------------------------------
-define(MonWireless, [
     {ifInOctets,    "1.3.6.1.4.1.2011.6.139.3.8.1.2"},
     {ifOutOctets,   "1.3.6.1.4.1.2011.6.139.3.8.1.4"},
     {ifInPkts,      "1.3.6.1.4.1.2011.6.139.3.8.1.1" },
     {ifOutPkts,     "1.3.6.1.4.1.2011.6.139.3.8.1.3" }
]).

-define(MonWire, [
        {ifInOctets,     "1.3.6.1.4.1.2011.6.139.2.6.6.1.11"},
        {ifInErrors,     "1.3.6.1.4.1.2011.6.139.2.6.6.1.9" },
        {ifOutOctets,    "1.3.6.1.4.1.2011.6.139.2.6.6.1.12"}
        %{ifInUcastPkts,  ""},
        %{ifInNUcastPkts, ""},
        %{ifInDiscards,   ""},
        %{ifOutUcastPkts, ""},
        %{ifOutNUcastPkts,""},
        %{ifOutDiscards,  ""},
        %{ifOutErrors,    ""}  
  ]).

%%ap连接信息统计
-define(MonAssoc, [
    {assocNum,           "1.3.6.1.4.1.2011.6.139.4.9.1.3" },
    {assocFailNum,       "1.3.6.1.4.1.2011.6.139.4.9.1.7" },
    {reAssocNum,         "1.3.6.1.4.1.2011.6.139.4.9.1.8" },
    {reAssocFailNum,     "1.3.6.1.4.1.2011.6.139.4.9.1.10"},
    {assocRefusedNum,    "1.3.6.1.4.1.2011.6.139.4.9.1.6" },
    {apStationOnlineSum, "1.3.6.1.4.1.2011.6.139.2.6.6.1.5"},
    {cpuRTUsage,         "1.3.6.1.4.1.2011.6.139.2.6.6.1.2"},
    {memRTUsage,         "1.3.6.1.4.1.2011.6.139.2.6.6.1.1"}

    %{deauthNum,          ""},
    %{apStationAssocSum,  "" },

  ]).

-define(StaMacList, {staMaclist, "1.3.6.1.4.1.2011.6.139.4.6.1.5"}).
%-define(StaIP, {staIp, ""}).
%-define(StaRssi, {staRssi, ""}).
%-define(StaNoiseRate, {staNoiseRate, ""}).
%-define(StaChannel, {staChannel, ""}).
%-define(StaVlan, {staVlan, ""}).
%-define(StaSsid, {staSsid, ""}).

-define(FitapMac,   {apMac,      "1.3.6.1.4.1.2011.6.139.2.6.1.1.5" }).     
-define(StaRxframe, {staRxframe, "1.3.6.1.4.1.2011.6.139.4.5.1.10"}).
-define(StaTxframe, {staTxframe, "1.3.6.1.4.1.2011.6.139.4.5.1.9"}).
-define(StaRxbytes, {staRxbytes, "1.3.6.1.4.1.2011.6.139.4.5.1.12"}).
-define(StaTxbytes, {staTxbytes, "1.3.6.1.4.1.2011.6.139.4.5.1.11"}).

-define(ApSta,[
    %{apId,         "" },
    %{staMac,       "" },      
    %{staIp,        "" },       
    %{staRssi,      "" },    
    %{staNoiseRate, "" },
    %{staChannel,   "" },  
    %{staVlan,      "" },     
    %{staSsid,      "" },
    %{staTxPkts,    "" },   
    %{staTxbytes,   "" },  
    %{staRxPkts,    "" },   
    %{staRxbytes,   "" } 
  ]).
%%----------------------------------------------------------------------
%% ApSsid
%%----------------------------------------------------------------------
-define(SsidAssoc,{ssidAssoc,"1.3.6.1.4.1.2011.6.139.4.1.1.2"}).
-define(DiscoApSsid,[
    {ssidName,             "1.3.6.1.4.1.2011.6.139.4.1.1.2"},
    %{ssidEnabled,          ""},
    {ssidHidden,           "1.3.6.1.4.1.2011.6.139.4.1.1.3"},
    {staIsolate,           "1.3.6.1.4.1.2011.6.139.4.1.1.4"},
    {dot11Auth,            "1.3.6.1.4.1.2011.6.139.6.4.1.7"},
    %{security,             ""},
    %{authenMode,           ""},
    %{securityCiphers,      ""},
    {vlanId,               "1.3.6.1.4.1.2011.6.139.4.1.1.11"},
    {maxSimultUsers,       "1.3.6.1.4.1.2011.6.139.4.1.1.8"}
]).


