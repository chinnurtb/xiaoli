%include:
%中太     (AW7000) 
%国人     (SGR-WM2004-A-10000-6144) 
%杰赛     (JSAC-AT7605) 
%三元达   (BNM-1024) 
%弘浩明传 (HM-ANI3000) 
%中兴大AC (ZXV10 W908) 
%东方信联 (TS-AC-10-S-500) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ac discover -- ac配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(Ac,[
     {acName,      "3.1.1.1.1.1.0" },
     {maxApLimit,  "3.1.1.1.1.6.0" },
     {sysModel,    "3.1.1.1.1.18.0"},
     {softVersion, "3.1.1.1.1.19.0"}
]).

-define(EthEntry, [
     {ifIndex,       "3.1.1.4.2.1.2"},
     {ifDescr,       "3.1.1.4.2.1.3"},
     {ifType,        "3.1.1.4.2.1.4"},
     {ifMtu,         "3.1.1.4.2.1.5"},
     {ifSpeed,       "3.1.1.4.2.1.6"},
     {ifPhysAddress, "3.1.1.4.2.1.7"},
     {ifAdminStatus, "3.1.1.4.2.1.8"},
     {ifOperStatus,  "3.1.1.4.2.1.9"}
]).

-define(AcRadio,[
     {radiusAuthServerIPAdd, "3.3.5.1.2"},
     {radiusAuthServerPort,  "3.3.5.1.3"}
]).

-define(AcVlan,[
     {ipPoolName,      "3.1.1.1.10.1.11"},
     {ipPoolStartAddr, "3.1.1.1.10.1.3" },
     {ipPoolStopAddr,  "3.1.1.1.10.1.4" }
]).

-define(PoolOids,[
    {ipPoolName, "3.1.1.1.10.1.11"},
    {ipPoolUsage,"3.1.1.1.10.1.18"}
]).


-define(Ap,[
     {apMac,         "3.1.1.3.1.1.8" }, 
     {apState,       "3.1.1.3.1.1.9" },
     {apName,        "3.1.1.3.1.1.11"},
     {apType,        "3.1.1.3.1.1.14"},
     {apSerialNo,    "3.1.1.3.1.1.12"},
     {apSoftVersion, "3.1.1.3.1.1.16"},
     {apIp,          "3.1.1.3.1.1.3" },
     {apMask,        "3.1.1.3.1.1.4" }
]).

%%----------------------------------------------------------------------
%% ac monet --ac 性能采集
%%----------------------------------------------------------------------
-define(Acinfo, [
     {cpuRTUsage,               "3.1.1.1.1.87.0" },
     {memRTUsage,               "3.1.1.1.1.89.0" },
     {flashMemRTUsage,          "3.1.1.1.1.117.0"},
     {dHCPIpPoolUsage,          "3.1.1.1.1.75.0" },
     {dHCPReqTimes,             "3.1.1.1.1.76.0" },
     {dHCPReqSucTimes,          "3.1.1.1.1.77.0" },
     {onlineNum,                "3.1.1.1.1.79.0" },
     {authNum,                  "3.1.1.1.1.74.0" },
     {normalNum,                "3.1.1.1.1.99.0" },
     {deauthNum,                "3.1.1.1.1.100.0"},
     {authReqNum,               "3.1.1.1.31.1.0" },
     {authSucNum,               "3.1.1.1.31.2.0" },
     {accReqNum,                "3.1.1.1.1.111.0"},
     {accSucNum,                "3.1.1.1.1.112.0"},
     {portalChallengeReqCount,  "3.1.1.1.30.2.0" },
     {portalChallengeRespCount, "3.1.1.1.30.4.0" },
     {portalAuthReqCount,       "3.1.1.1.30.1.0" },
     {portalAuthRespCount,      "3.1.1.1.30.3.0" },
     {leaveReqCount,            "3.1.1.1.31.4.0" },
     {leaveRepCount,            "3.1.1.1.31.5.0" },
     {radiusAvgDelay,           "3.1.1.1.1.103.0"}
    ]).
%Counter32
-define(EthTraffic, [
     {ifDescr,        "3.1.1.4.2.1.3" },
     {ifInUcastPkts,  "3.1.1.4.2.1.12"},
     {ifInNUcastPkts, "3.1.1.4.2.1.13"},
     {ifInOctets,     "3.1.1.4.2.1.11"},
     {ifInDiscards,   "3.1.1.4.2.1.14"},
     {ifInErrors,     "3.1.1.4.2.1.15"},
     {ifOutUcastPkts, "3.1.1.4.2.1.18"},
     {ifOutNUcastPkts,"3.1.1.4.2.1.19"},
     {ifOutOctets,    "3.1.1.4.2.1.17"}, 
     {ifOutDiscards,  "3.1.1.4.2.1.20"},
     {ifOutErrors,    "3.1.1.4.2.1.21"},
     {ifUpDwnTimes,   "3.1.1.4.2.1.22"}  
]).
%Part Counter64
-define(ZteW908EthTraffic, [
     {ifDescr,        "3.1.1.4.2.1.3"    },
     {ifInUcastPkts,  "3.1.1.1.24.2.1.9" },
     {ifInNUcastPkts, "3.1.1.1.24.2.1.10"},
     {ifInOctets,     "3.1.1.1.24.2.1.8" },
     {ifInDiscards,   "3.1.1.4.2.1.14"   },
     {ifInErrors,     "3.1.1.4.2.1.15"   },
     {ifOutUcastPkts, "3.1.1.1.24.2.1.13"},
     {ifOutNUcastPkts,"3.1.1.1.24.2.1.14"},
     {ifOutOctets,    "3.1.1.1.24.2.1.12"}, 
     {ifOutDiscards,  "3.1.1.4.2.1.20"   },
     {ifOutErrors,    "3.1.1.4.2.1.21"   },
     {ifUpDwnTimes,   "3.1.1.4.2.1.22"   }  
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap monet --ap 性能采集% 
%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MonWireless, [
     {ifInAvgSignal,  "3.1.1.3.7.1.72"},
     {ifInHighSignal, "3.1.1.3.7.1.73"},
     {ifInLowSignal,  "3.1.1.3.7.1.74"},
     {ifInPkts,       "3.1.1.3.7.1.10"},
     {ifOutPkts,      "3.1.1.3.7.1.11"},
     {ifInOctets,     "3.1.1.3.7.1.12"},
     {ifOutOctets,    "3.1.1.3.7.1.13"},
     {ifInErrors,     "3.1.1.3.7.1.71"},
     {fitapWirelessFrameRetryRate, "3.1.1.3.7.1.55"}
]).

-define(MonWire, [
     {ifInUcastPkts,   "3.1.1.3.6.1.9" },
     {ifInNUcastPkts,  "3.1.1.3.6.1.16"},
     {ifInOctets,      "3.1.1.3.6.1.17"},
     {ifInDiscards,    "3.1.1.3.6.1.12"},
     {ifInErrors,      "3.1.1.3.6.1.18"},
     {ifOutUcastPkts,  "3.1.1.3.6.1.13"},  
     {ifOutNUcastPkts, "3.1.1.3.6.1.19"},
     {ifOutOctets,     "3.1.1.3.6.1.20"},
     {ifOutDiscards,   "3.1.1.3.6.1.21"},
     {ifOutErrors,     "3.1.1.3.6.1.22"}
]).

-define(MonAssoc,[
     {assocNum,           "3.1.1.3.1.1.51" },
     {assocFailNum,       "3.1.1.3.23.1.30"},
     {reAssocNum,         "3.1.1.3.23.1.11"},
     {reAssocSuccNum,     "3.1.1.3.23.1.12"},
     {assocRefusedNum,    "3.1.1.3.23.1.36" },
     {deauthNum,          "3.1.1.3.23.1.27"},
     {apStationAssocSum,  "3.1.1.3.1.1.28" },
     {apStationOnlineSum, "3.1.1.3.1.1.123"},
     {cpuRTUsage,         "3.1.1.3.1.1.31"},
     {memRTUsage,         "3.1.1.3.1.1.87"}
]).

-define(StaLogin,[
    {loginNum,     "3.1.1.1.23.2.1.9"}
  ]).


-define(ApSta,[
     {apId,          "3.1.1.3.3.1.35"},
     {staMac,        "3.1.1.3.3.1.6" },
     {staIp,         "3.1.1.3.3.1.18"},
     {staRssi,       "3.1.1.3.3.1.16"},
     {staNoiseRate,  "3.1.1.3.3.1.17"},
     {staChannel,    "3.1.1.3.3.1.27"},
     {staVlan,       "3.1.1.3.3.1.24"},
     {staSsid,       "3.1.1.3.3.1.19"},
     {staTxframe,    "3.1.1.3.3.1.4" },
     {staTxbytes,    "3.1.1.3.3.1.28"},
     {staRxframe,    "3.1.1.3.3.1.3" },
     {staRxbytes,    "3.1.1.3.3.1.29"}
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap discover -- ap配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(ApRadio,[
     {radioPeriod,  "3.1.1.3.2.1.30"},
     {radioDtim,    "3.1.1.3.2.1.31"},
     {radioRts,     "3.1.1.3.2.1.7" },
     {radioSlice,   "3.1.1.3.2.1.6" },
     {radioModel,   "3.1.1.3.2.1.23"},
     {radioChannel, "3.1.1.3.2.1.27"},
     {radioPower,   "3.1.1.3.2.1.24"}
]).

-define(ApWirelessIf,[
     {ifDescr,       "3.1.1.3.6.1.2" },
     {ifType,        "3.1.1.3.6.1.3" },
     {ifMtu,         "3.1.1.3.6.1.4" },
     {ifSpeed,       "3.1.1.3.6.1.5" },
     {ifPhysAddress, "3.1.1.3.6.1.6" },
     {ifAdminStatus, "3.1.1.3.6.1.23"},
     {ifOperStatus,  "3.1.1.3.6.1.8" }
 ]).


-define(ApState, [
     {apState, "3.1.1.3.1.1.9"}
]).
%%----------------------------------------------------------------------
%% ApSsid
%%----------------------------------------------------------------------
-define(SsidAssoc,{ssidAssoc,"3.2.1.2.1.2"}).
-define(DiscoApSsid,[
    {ssidName,             "3.2.1.3.1.6" },
    {ssidEnabled,          "3.2.1.3.1.17"},
    {ssidHidden,           "3.2.1.3.1.7" },
    {staIsolate,           "3.2.1.3.1.18"},
    {dot11Auth,            "3.2.1.3.1.4" },
    {security,             "3.2.1.3.1.5" },
    {authenMode,           "3.2.1.3.1.19"},
    {securityCiphers,      "3.2.1.3.1.20"},
    {vlanId,               "3.2.1.3.1.8" },
    {maxSimultUsers,       "3.2.1.3.1.11"}
]).



