%   普天 (auteX5612) 
%   虹信 (FH-AC2400) 
%   大唐 (MX-400II) 
%   京信 (AC2400-F1024) 
%   联信永益 (LXP-X7605) 
%   明华澳汉 (WL-AC1) 
%   傲天动联 (auteX7605)
%%----------------------------------------------------------------------
%% ac discover -- ac配置(发现)
%%----------------------------------------------------------------------
-define(Ac,[
     {acName,          "6.1.2.1.2.1.0"},
     {softVersion,     "6.1.2.1.1.2.0"},
     {maxApLimit,      "6.1.2.3.2.1.0"},
     {portalServerURL, "6.1.2.2.6.0"  }
]).

%-define(AcIf,[
%     {ifDescr,       "6.1.2.4.2.1.2"},
%     {ifType,        "6.1.2.4.2.1.3"},
%     {ifMtu,         "6.1.2.4.2.1.4"},
%     {ifSpeed,       "6.1.2.4.2.1.5"},
%     {ifPhysAddress, "6.1.2.4.2.1.6"},
%     {ifAdminStatus, "6.1.2.4.2.1.7"},
%     {ifOperStatus,  "6.1.2.4.2.1.8"}
%]).

-define(AcRadio,[
     {radiusAuthServerIPAdd, "6.1.2.14.6.1.2"},
     {radiusAuthServerPort,  "6.1.2.14.6.1.3"}
]).

-define(AcVlan,[
     {ipPoolName,      "6.1.2.6.5.1.10"},
     {ipPoolStartAddr, "6.1.2.6.5.1.2" },
     {ipPoolStopAddr,  "6.1.2.6.5.1.3" }
]).

-define(PoolOids,[
    {ipPoolName, "6.1.2.6.6.1.2"},
    {ipPoolUsage,"6.1.2.6.6.1.3"}
]).

-define(Ap,[
     {apName,       "6.1.1.1.1.1.2" },
     {apSerialNo,   "6.1.1.1.1.1.11"},
     {apType,       "6.1.1.1.1.1.5" },
     {apIp,         "6.1.1.1.5.1.1" },
     {apMask,       "6.1.1.1.5.1.2" },
     {apMac,        "6.1.1.1.1.1.1" },
     {apSoftVersion,"6.1.1.1.1.1.6" }
]).

%%----------------------------------------------------------------------
%% ac monet --ac 性能采集 
%%----------------------------------------------------------------------
-define(Acinfo, [
     {cpuRTUsage,               "6.1.2.1.2.13.0"},
     {memRTUsage,               "6.1.2.1.2.8.0" },
     {flashMemTotal,            "6.1.2.1.2.18.0"},
     {flashMemFree,             "6.1.2.1.2.19.0"},
     {dHCPIpPoolUsage,          "6.1.2.6.3.1.0" },
     {dHCPReqTimes,             "6.1.2.6.3.2.0" },
     {dHCPReqSucTimes,          "6.1.2.6.3.3.0" },
     {onlineNum,                "6.1.2.3.1.3.0" },
     {authNum,                  "6.1.2.3.1.13.0"},
     {normalNum,                "6.1.2.13.4.1.6"},% ssid多值     
     {deauthNum,                "6.1.2.3.1.23.0"}, 
     {authReqNum,               "6.1.2.3.1.9.0" },
     {authSucNum,               "6.1.2.3.1.10.0"},
     {leaveReqCount,            "6.1.2.3.1.31.0"},
     {leaveRepCount,            "6.1.2.3.1.32.0"},
     {accReqNum,                "6.1.2.3.1.33.0"},
     {accSucNum,                "6.1.2.3.1.34.0"},
     {portalChallengeReqCount,  "6.1.2.3.1.24.0"},
     {portalChallengeRespCount, "6.1.2.3.1.25.0"},
     {portalAuthReqCount,       "6.1.2.3.1.26.0"},
     {portalAuthRespCount,      "6.1.2.3.1.30.0"},
     {radiusAvgDelay,           "6.1.2.14.2.1.14.0"}
  ]).

-define(Acintf, [
     {ifInUcastPkts,  "6.1.2.4.3.1.1" },
     {ifInNUcastPkts, "6.1.2.4.3.1.2" },
     {ifInOctets,     "6.1.2.4.3.1.3" },   
     {ifInDiscards,   "6.1.2.4.3.1.4" }, 
     {ifInErrors,     "6.1.2.4.3.1.5" },   
     {ifOutUcastPkts, "6.1.2.4.3.1.6" },
     {ifOutNUcastPkts,"6.1.2.4.3.1.7" },
     {ifOutOctets,    "6.1.2.4.3.1.8" },  
     {ifOutDiscards,  "6.1.2.4.3.1.9" },
     {ifOutErrors,    "6.1.2.4.3.1.10"}, 
     {ifUpDwnTimes,   "6.1.2.4.3.1.11"}
  ]).
%%----------------------------------------------------------------------
%% ap discover -- ap配置(发现)
%%----------------------------------------------------------------------
-define(ApRadio,[
     {radioPeriod,  "6.1.1.4.3.1.5" },
     {radioDtim,    "6.1.1.4.3.1.7" },
     {radioRts,     "6.1.1.4.3.1.8" },
     {radioSlice,   "6.1.1.4.3.1.6" },
     {radioModel,   "6.1.1.4.1.1.7" },
     {radioChannel, "6.1.1.4.1.1.6" },
     {radioPower,   "6.1.1.4.1.1.2" }
  ]).

-define(ApWirelessIf,[
     {ifDescr,       "6.1.1.3.3.1.2"},
     {ifType,        "6.1.1.3.3.1.3"},
     {ifMtu,         "6.1.1.3.3.1.4"},
     {ifSpeed,       "6.1.1.3.3.1.5"},
     {ifPhysAddress, "6.1.1.3.3.1.6"},
     {ifAdminStatus, "6.1.1.3.3.1.7"},
     {ifOperStatus,  "6.1.1.3.3.1.8"}
 ]).

-define(ApState, [
    {apState, "6.1.1.2.5.1.10"}
]).

%%----------------------------------------------------------------------
%% ap monet --ap 性能采集 
%%----------------------------------------------------------------------
-define(MonWireless, [
     {ifInOctets,    "6.1.1.3.5.1.10"},
     {ifOutPkts,     "6.1.1.3.5.1.8" },
     {ifInPkts,      "6.1.1.3.5.1.9" },
     {ifOutOctets,   "6.1.1.3.5.1.11"},
     {ifInErrors,    "6.1.1.3.5.1.21"},
     {ifInAvgSignal, "6.1.1.3.5.1.1" },
     {ifInHighSignal,"6.1.1.3.5.1.2" },
     {ifInLowSignal, "6.1.1.3.5.1.3" }
     %{ifFrameRetryRate,""}
]).

-define(MonWire, [
    {ifInUcastPkts,  "6.1.1.3.2.1.1" },
    {ifInNUcastPkts, "6.1.1.3.2.1.2" },
    {ifInOctets,     "6.1.1.3.2.1.12"},
    {ifInDiscards,   "6.1.1.3.2.1.4" },
    {ifInErrors,     "6.1.1.3.2.1.5" },
    {ifOutUcastPkts, "6.1.1.3.2.1.6" },
    {ifOutNUcastPkts,"6.1.1.3.2.1.7" },
    {ifOutOctets,    "6.1.1.3.2.1.13"},
    {ifOutDiscards,  "6.1.1.3.2.1.9" },
    {ifOutErrors,    "6.1.1.3.2.1.10"}  
  ]).

%%ap连接信息统计
-define(MonAssoc, [
    {assocNum,           "6.1.1.2.4.1.2" },
    {assocFailNum,       "6.1.1.2.4.1.3" },
    {reAssocNum,         "6.1.1.2.4.1.4" },
    {deauthNum,          "6.1.1.2.4.1.11"},
    {assocRefusedNum,    "6.1.1.2.4.1.10"},
    {apStationAssocSum,  "6.1.1.2.4.1.6" },
    {apStationOnlineSum, "6.1.1.17.1.1.1"},
    {cpuRTUsage,         "6.1.1.1.2.1.3" },
    {memRTUsage,         "6.1.1.1.2.1.9" }
%   {reAssocFailNum,     ""},
  ]).

-define(SurekamAssoc, [
    {assocNum,           "6.1.1.2.4.1.2" },
    {assocFailNum,       "6.1.1.2.4.1.3" },
    {reAssocNum,         "6.1.1.2.4.1.4" },
    {deauthNum,          "6.1.1.2.4.1.11"},
    {assocRefusedNum,    "6.1.1.2.4.1.10"},
    {apStationAssocSum,  "6.1.1.2.4.1.9" },
    {apStationOnlineSum, "6.1.1.2.4.1.6" },
    {cpuRTUsage,         "6.1.1.1.2.1.3" },
    {memRTUsage,         "6.1.1.1.2.1.9" }
%   {reAssocFailNum,     ""},
  ]).

-define(StaLogin,[
    {loginNum,     "6.1.1.17.2.1.2"}
  ]).

-define(ApSta,[
    {staMac,       "6.1.1.8.1.1.1" },      
    {staIp,        "6.1.1.8.2.1.2" }, 
    {staRssi,      "6.1.1.8.1.1.15"},    
    {staNoiseRate, "6.1.1.8.1.1.3" },
    {staChannel,   "6.1.1.8.2.1.4" },  
    {staVlan,      "6.1.1.8.2.1.7" },     
    {staSsid,      "6.1.1.8.2.1.8" },
    {staTxframe,   "6.1.1.8.1.1.4" },   
    {staTxbytes,   "6.1.1.8.1.1.5" },  
    {staRxframe,   "6.1.1.8.1.1.6" },   
    {staRxbytes,   "6.1.1.8.1.1.7" } 
  ]).
%%----------------------------------------------------------------------
%% ApSsid
%%----------------------------------------------------------------------
-define(SsidAssoc,{ssidAssoc,"6.1.2.12.4.1.1"}).
-define(DiscoApSsid,[
    {ssidName,       "6.1.2.13.6.1.1"},
    {ssidEnabled,    "6.1.2.13.6.1.2"},
    {ssidHidden,     "6.1.2.13.6.1.3"},
    {staIsolate,     "6.1.2.13.6.1.4"},
    {dot11Auth,      "6.1.2.13.6.1.5"},
    {security,       "6.1.2.13.6.1.6"},
    {authenMode,     "6.1.2.13.6.1.7"},
    {securityCiphers,"6.1.2.13.6.1.8"},
    {vlanId,         "6.1.2.13.6.1.18"},
    {maxSimultUsers, "6.1.2.13.6.1.19"}
]).
