%系列三：
%京信 (AC2400) 
%大唐 (MX-400) 
%虹信2(FHAP2400) 
%中兴 (ZXV10_W901) 
%%----------------------------------------------------------------------
%% ac discover -- ac配置(发现)
%%----------------------------------------------------------------------
-define(Ac,[
     {acName,          "1.1.0"  },
     {maxApLimit,      "1.8.0"  },
     {softVersion,     "1.21.0" },
     {portalServerURL, "4.1.2.0"}
]).

-define(Ap,[
     {apMac,        "6.2.1.2"  }, 
     {apName,       "6.2.1.4"  },
     {apType,       "6.2.1.274"},
     {apSoftVersion,"6.2.1.54" },
     {apIp,         "6.2.1.3"  },
     {apMask,       "6.2.1.52" }
]).

-define(EthEntry,[
     {ifDescr,       "6.1.15.1.2"},
     {ifType,        "6.1.15.1.5"},
     {ifMtu,         "6.1.15.1.6"},
     {ifSpeed,       "6.1.15.1.7"},
     {ifPhysAddress, "6.1.15.1.3"},
     {ifAdminStatus, "6.1.15.1.9"},
     {ifOperStatus,  "6.1.15.1.4"}
]).

-define(AcRadio,[
     {radiusAuthServerIPAdd, "4.4.6.1.2"},
     {radiusAuthServerPort,  "4.4.6.1.3"}
]).

-define(AcVlan,[
     {ipPoolName,      "2.4.5.1.14"},
     {ipPoolStartAddr, "2.4.5.1.3" },
     {ipPoolStopAddr,  "2.4.5.1.4" }
]).

-define(PoolOids,[
    {ipPoolName, "2.4.5.1.14"},
    {ipPoolUsage,"2.4.5.1.15"}
]).


%%----------------------------------------------------------------------
%% ap discover -- ap配置(发现)
%%----------------------------------------------------------------------
-define(ApRadio,[
     {radioPeriod,   "3.9.1.3" },
     {radioDtim,     "3.9.1.4" },
     {radioRts,      "3.9.1.5" },
     {radioSlice,    "3.9.1.6" },
     {radioModel,    "3.8.1.16"},
     {radioChannel,  "3.8.1.13"},
     {radioPower,    "3.8.1.18"}
]).

-define(ApWirelessIf,[
     {ifDescr,       "3.8.1.3" },
     {ifType,        "3.8.1.4" },
     {ifMtu,         "3.8.1.5" },
     {ifSpeed,       "3.8.1.6" },
     {ifPhysAddress, "3.8.1.7" },
     {ifAdminStatus, "3.8.1.8" },
     {ifOperStatus,  "3.8.1.9" }
]).
-define(ApState,[{apState,"6.2.1.5"}]).


%%----------------------------------------------------------------------
%% ac monet --ac 性能采集
%%----------------------------------------------------------------------
-define(Acinfo, [
     {cpuRTUsage,               "1.24.0"  },
     {memRTUsage,               "1.25.0"  },
     {dHCPIpPoolUsage,          "6.1.8.0" },
     {dHCPReqTimes,             "6.1.16.0"},
     {dHCPReqSucTimes,          "6.1.17.0"},
     {onlineNum,                "6.1.6.0" },
     {authNum,                  "6.1.27.0"},
     {normalNum,                "6.1.43.0"},
     {deauthNum,                "6.1.44.0"},
     {authReqNum,               "6.1.23.0"},
     {authSucNum,               "6.1.24.0"},
     {accReqNum,                "6.1.81.0"},
     {accSucNum,                "6.1.82.0"},
     {portalChallengeReqCount,  "6.1.90.0"},
     {portalChallengeRespCount, "6.1.91.0"},
     {portalAuthReqCount,       "6.1.88.0"},
     {portalAuthRespCount,      "6.1.89.0"},
     {leaveReqCount,            "6.1.70.0"},
     {leaveRepCount,            "6.1.69.0"},
     {flashMemTotal,            "1.68.0"  },
     {flashMemFree,             "1.67.0"  }
     %{radiusAvgDelay,           ""}),
     %{addressCount,             ""},
]).

-define(EthTraffic, [
     {ifDescr,        "6.1.15.1.2"},
     {ifInNUcastPkts, "6.1.15.1.20"},
     {ifInUcastPkts,  "6.1.15.1.19"},
     {ifInOctets,     "6.1.15.1.14"},
     {ifInDiscards,   "6.1.15.1.26"},
     {ifInErrors,     "6.1.15.1.23"},
     {ifOutNUcastPkts,"6.1.15.1.22"},
     {ifOutUcastPkts, "6.1.15.1.21"},
     {ifOutOctets,    "6.1.15.1.15"},
     {ifOutDiscards,  "6.1.15.1.27"},
     {ifOutErrors,    "6.1.15.1.24"},
     {ifUpDwnTimes,   "6.1.15.1.25"}
]).

% 山西反馈 ac粒度,模拟成ifIndex=0,然后作为上联
-define(CombaEthTraffic, [
     {ifInNUcastPkts, "6.1.39.9.0" },
     {ifInUcastPkts,  "6.1.39.8.0" },
     {ifInOctets,     "6.1.39.3.0" },
     {ifInDiscards,   "6.1.39.15.0"},
     {ifInErrors,     "6.1.39.12.0"},
     {ifOutNUcastPkts,"6.1.39.11.0"},
     {ifOutUcastPkts, "6.1.39.10.0"},
     {ifOutOctets,    "6.1.39.4.0" },
     {ifOutDiscards,  "6.1.39.16.0"},
     {ifOutErrors,    "6.1.39.13.0"}
]).

%%----------------------------------------------------------------------
%% ap monet --ap 性能采集 
%%----------------------------------------------------------------------
-define(ChongqinMonWireless, [
     {ifOutPkts,      "6.2.1.17" },
     {ifInPkts,       "6.2.1.16" },
     {ifInOctets,     "6.2.1.18" },
     {ifOutOctets,    "6.2.1.19" },
     {ifInHighSignal, "6.2.1.380"},
     {ifInLowSignal,  "6.2.1.381"},
     {ifInAvgSignal,  "6.2.1.6"  },
     {ifInErrors,     "6.2.1.29" }
     %{fitapWirelessFrameRetryRate, ""}
]).

-define(ChongqinMonWire, [
     {ifInUcastPkts,  "6.2.1.313"},
     {ifInNUcastPkts, "6.2.1.316"},
     {ifInOctets,     "6.2.1.291"},
     {ifInDiscards,   "6.2.1.299"},
     {ifInErrors,     "6.2.1.294"},
     {ifOutUcastPkts, "6.2.1.314"},  
     {ifOutNUcastPkts,"6.2.1.315"},
     {ifOutOctets,    "6.2.1.292"},
     {ifOutDiscards,  "6.2.1.298"},
     {ifOutErrors,    "6.2.1.297"}
]).
-define(ChongqinMonAssoc,[
     {assocNum,           "6.2.1.41" },
     {assocFailNum,       "6.2.1.42" },
     {apStationOnlineSum, "6.22.1.2" },
     {cpuRTUsage,         "6.2.1.375"},
     {memRTUsage,         "6.2.1.377"}
]).


-define(MonWireless, [
     {ifInAvgSignal,    "6.15.3.1.4" },
     {ifInHighSignal,   "6.15.3.1.5" },
     {ifInLowSignal,    "6.15.3.1.6" },
     {ifOutOctets,      "6.15.3.1.13"},
     {ifOutPkts,        "6.15.3.1.11"},
     {ifInPkts,         "6.15.3.1.12"},
     {ifInOctets,       "6.15.3.1.14"},
     {ifInErrors,       "6.15.3.1.25"},
     {ifFrameRetryRate, "6.2.1.32"   }
]).

-define(MonWire, [
     {ifInUcastPkts,   "6.15.2.1.3" },
     {ifInNUcastPkts,  "6.15.2.1.4" },
     {ifInOctets,      "6.15.2.1.5" },
     {ifInDiscards,    "6.15.2.1.6" },
     {ifInErrors,      "6.15.2.1.7" },
     {ifOutUcastPkts,  "6.15.2.1.8" },
     {ifOutNUcastPkts, "6.15.2.1.9" },
     {ifOutOctets,     "6.15.2.1.10"},
     {ifInDiscards,    "6.15.2.1.11"},
     {ifOutErrors,     "6.15.2.1.12"}
]).

-define(MonAssoc,[
     {assocNum,           "6.17.1.4" },
     {assocFailNum,       "6.17.1.5" },
     {reAssocNum,         "6.17.1.6" },
     {reAssocFailNum,     "6.17.1.18"},
     {assocRefusedNum,    "6.17.1.8" },
     {deauthNum,          "6.17.1.7" },
     {apStationAssocSum,  "6.17.1.2" },
     {apStationOnlineSum, "6.22.1.2" },
     {cpuRTUsage,         "6.18.1.2" },
     {memRTUsage,         "6.18.1.4" }
]).

-define(StaLogin,[
    {loginNum,     "6.24.1.4"}
]).

-define(ApSta,[
     {staMac,        "6.3.1.2" },
     {apId,          "6.3.1.5" }, 
     {staIp,         "6.3.1.6" },
     {staRssi,       "6.3.1.8" },
     {staNoiseRate,  "6.3.1.31"},
     {staChannel,    "6.3.1.32"},
     {staVlan,       "6.3.1.33"},
     {staSsid,       "6.3.1.9" },
     {staRxframe,    "6.3.1.11"},
     {staTxframe,    "6.3.1.13"},
     {staRxbytes,    "6.3.1.12"},
     {staTxbytes,    "6.3.1.14"}
]).
%%----------------------------------------------------------------------
%% ApSsid
%%----------------------------------------------------------------------
-define(SsidAssoc,{ssidAssoc,"6.16.1.6"}).
-define(DiscoApSsid,[
     {ssidName,             "3.13.1.2" },
     {ssidEnabled,          "3.13.1.3" },
     {ssidHidden,           "3.13.1.4" },
     {staIsolate,           "3.13.1.5" },
     {dot11Auth,            "3.13.1.6" },
     {security,             "3.13.1.7" },
     {authenMode,           "3.13.1.8" },
     {securityCiphers,      "3.13.1.9" },
     {vlanId,               "3.13.1.10"},
     {maxSimultUsers,       "3.13.1.11"}
]).

-define(MonApSsid,[
     {ifOutPkts,   "6.16.1.20"},
     {ifInPkts,    "6.16.1.19"},
     {ifOutOctets, "6.16.1.18"},
     {ifInOctets,  "6.16.1.17"}
]).

