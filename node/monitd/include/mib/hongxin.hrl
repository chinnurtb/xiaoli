%%----------------------------------------------------------------------
%% ac discover -- ac配置(发现)
%%----------------------------------------------------------------------
-define(Ac,[
     {acName,          "1.3.6.1.4.1.33940.6.1.2.1.2.1.0"},
     {softVersion,     "1.3.6.1.4.1.33940.6.1.2.1.1.2.0"},
     {maxApLimit,       "1.3.6.1.4.1.33940.6.1.2.3.2.1.0"},
     {portalServerURL, "1.3.6.1.4.1.33940.6.1.2.2.6.0"  }
]).
-define(AsIPAddress, {asIPAddress, "1.3.6.1.4.1.33940.6.1.2.14.2.1.16.0"}).


% --- RadiusServerIndex 
-define(RadiusAuthServerIPAdd, {radiusAuthServerIPAdd, "1.3.6.1.4.1.33940.6.1.2.14.6.1.2"}).
-define(RadiusAuthServerPort, {radiusAuthServerPort, "1.3.6.1.4.1.33940.6.1.2.14.6.1.3"}).

% ----  vlaninterface
-define(IPPoolName, {ipPoolName, "1.3.6.1.4.1.33940.6.1.2.6.5.1.10"}).
-define(IPPoolStartAddr, {ipPoolStartAddr, "1.3.6.1.4.1.33940.6.1.2.6.5.1.2"}).
-define(IPPoolStopAddr, {ipPoolStopAddr, "1.3.6.1.4.1.33940.6.1.2.6.5.1.3"}).

-define(IPPoolName2, {ipPoolName, "1.3.6.1.4.1.33940.6.1.2.6.6.1.2"}).
-define(IPPoolUsage, {ipPoolUsage, "1.3.6.1.4.1.33940.6.1.2.6.6.1.3"}).

-define(PoolOids, [?IPPoolName2, ?IPPoolUsage]).

%%----------------------------------------------------------------------
%% ac monet --ac 性能采集
%%----------------------------------------------------------------------
-define(Acinfo, [
     {cpuRTUsage,               "1.3.6.1.4.1.33940.6.1.2.1.2.13.0"   },
     {memRTUsage,               "1.3.6.1.4.1.33940.6.1.2.1.2.8.0"    },
     {dHCPReqTimes,             "1.3.6.1.4.1.33940.6.1.2.6.3.2.0"    },
     {dHCPReqSucTimes,          "1.3.6.1.4.1.33940.6.1.2.6.3.3.0"    },
     {authNum,                  "1.3.6.1.4.1.33940.6.1.2.3.1.13.0"   },
     {onlineNum,                "1.3.6.1.4.1.33940.6.1.2.3.1.3.0"    },
     {maxNum,                   "1.3.6.1.4.1.33940.6.1.2.3.1.19.0"   },
     {normalNum,                "1.3.6.1.4.1.33940.6.1.2.13.4.1.6"   },
     {deauthNum,                "1.3.6.1.4.1.33940.6.1.2.3.1.23.0"   },
     {authReqNum,               "1.3.6.1.4.1.33940.6.1.2.3.1.9.0"    },
     {authSucNum,               "1.3.6.1.4.1.33940.6.1.2.3.1.10.0"   },
     {radiusAvgDelay,           "1.3.6.1.4.1.33940.6.1.2.14.2.1.14.0"},
     {dHCPIpPoolUsage,          "1.3.6.1.4.1.33940.6.1.2.6.3.1.0"    },
     {leaveReqCount,            "1.3.6.1.4.1.33940.6.1.2.3.1.31.0"   },
     {leaveRepCount,            "1.3.6.1.4.1.33940.6.1.2.3.1.32.0"   },
     {flashMemTotal,            "1.3.6.1.4.1.33940.6.1.2.1.2.18.0"   },
     {flashMemFree,             "1.3.6.1.4.1.33940.6.1.2.1.2.19.0"   }
    %{bandWidth,                "1.3.6.1.4.1.33940.6.1.2.3.1.5.0"    },
    %{portalChallengeReqCount,  "1.3.6.1.4.1.33940.6.1.2.3.1.24"},
    %{portalChallengeRespCount, "1.3.6.1.4.1.33940.6.1.2.3.1.24"},
    %{portalAuthReqCount,       "1.3.6.1.4.1.33940.6.1.2.3.1.26"},
    %{portalAuthRespCount,      "1.3.6.1.4.1.33940.6.1.2.3.1.30"},
    
    %{flashMemRTUsage,          ""}
 ]).
  
-define(Acintf, [

     {ifInUcastPkts,   "1.3.6.1.4.1.33940.6.1.2.4.3.1.1" },
     {ifInNUcastPkts,  "1.3.6.1.4.1.33940.6.1.2.4.3.1.2" },
     {ifInOctets,      "1.3.6.1.4.1.33940.6.1.2.4.3.1.3" },
     {ifInDiscards,    "1.3.6.1.4.1.33940.6.1.2.4.3.1.4" },
     {ifInErrors,      "1.3.6.1.4.1.33940.6.1.2.4.3.1.5" },
     {ifOutUcastPkts,  "1.3.6.1.4.1.33940.6.1.2.4.3.1.6" },
     {ifOutNUcastPkts, "1.3.6.1.4.1.33940.6.1.2.4.3.1.7" },
     {ifOutOctets,     "1.3.6.1.4.1.33940.6.1.2.4.3.1.8" },
     {ifOutDiscards,   "1.3.6.1.4.1.33940.6.1.2.4.3.1.9" },
     {ifOutErrors,     "1.3.6.1.4.1.33940.6.1.2.4.3.1.10"},
     {ifUpDwnTimes,    "1.3.6.1.4.1.33940.6.1.2.4.3.1.11"}
]).

%%----------------------------------------------------------------------
%% ap monet --ap 性能采集% 
%%----------------------------------------------------------------------
-define(MonWireless, [
     {ifOutPkts,       "1.3.6.1.4.1.33940.6.1.1.3.5.1.8" },
     {ifInPkts,        "1.3.6.1.4.1.33940.6.1.1.3.5.1.9" },
     {ifInOctets,      "1.3.6.1.4.1.33940.6.1.1.3.5.1.10"},
     {ifOutOctets,     "1.3.6.1.4.1.33940.6.1.1.3.5.1.11"},
     {ifInErrors,      "1.3.6.1.4.1.33940.6.1.1.3.5.1.21"},
     {ifInAvgSignal,   "1.3.6.1.4.1.33940.6.1.1.3.5.1.1" },
     {ifInHighSignal,  "1.3.6.1.4.1.33940.6.1.1.3.5.1.2" },
     {ifInLowSignal,   "1.3.6.1.4.1.33940.6.1.1.3.5.1.3" },
     {ifFrameRetryRate,"1.3.6.1.4.1.33940.6.1.1.3.5.1.22"}
]).

-define(MonWire, [
     {ifInUcastPkts,   "1.3.6.1.4.1.33940.6.1.1.3.2.1.1" },
     {ifInNUcastPkts,  "1.3.6.1.4.1.33940.6.1.1.3.2.1.2" },
     {ifInOctets,      "1.3.6.1.4.1.33940.6.1.1.3.2.1.12"},
     {ifInDiscards,    "1.3.6.1.4.1.33940.6.1.1.3.2.1.4" },
     {ifInErrors,      "1.3.6.1.4.1.33940.6.1.1.3.2.1.5" },
     {ifOutUcastPkts,  "1.3.6.1.4.1.33940.6.1.1.3.2.1.6" },  
     {ifOutnUcastPkts, "1.3.6.1.4.1.33940.6.1.1.3.2.1.7" },
     {ifOutOctets,     "1.3.6.1.4.1.33940.6.1.1.3.2.1.13"},
     {ifOutDiscards,   "1.3.6.1.4.1.33940.6.1.1.3.2.1.9" },
     {ifOutErrors,     "1.3.6.1.4.1.33940.6.1.1.3.2.1.10"}
]).

-define(MonAssoc,[
     {assocNum,           "1.3.6.1.4.1.33940.6.1.1.2.4.1.2" },
     {assocFailNum,       "1.3.6.1.4.1.33940.6.1.1.2.4.1.3" },
     {reAssocNum,         "1.3.6.1.4.1.33940.6.1.1.2.4.1.4" },
     {assocRefusedNum,    "1.3.6.1.4.1.33940.6.1.1.2.4.1.10"},
     {deauthNum,          "1.3.6.1.4.1.33940.6.1.1.2.4.1.11"},
     {apStationAssocSum,  "1.3.6.1.4.1.33940.6.1.1.2.4.1.6" },   
     {apStationOnlineSum, "1.3.6.1.4.1.33940.6.1.1.17.1.1.1"},
     {cpuRTUsage,         "1.3.6.1.4.1.33940.6.1.1.1.2.1.3" },
     {memRTUsage,         "1.3.6.1.4.1.33940.6.1.1.1.2.1.9" }
    %{reAssocFailNum,     "" },
]).

%%sta终端性能统计  --- StaIndex
-define(FitapStaMac, {staMac, "1.3.6.1.4.1.33940.6.1.1.8.1.1.1"}).
%-define(FitapMac, {apMac, ""}).
-define(FitapStaIP, {staIp, "1.3.6.1.4.1.33940.6.1.1.8.1.1.2"}).
-define(FitapStaRssi, {staRssi, "1.3.6.1.4.1.33940.6.1.1.8.1.1.15"}).
-define(FitapStaNoiseRate, {staNoiseRate, "1.3.6.1.4.1.33940.6.1.1.8.1.1.3"}).
-define(FitapStaChannel, {staChannel, "1.3.6.1.4.1.33940.6.1.1.8.2.1.4"}).
-define(FitapStaVlan, {staVlan, "1.3.6.1.4.1.33940.6.1.1.8.2.1.7"}).
-define(FitapStaSsid, {staSsid, "1.3.6.1.4.1.33940.6.1.1.8.2.1.8"}).
-define(FitapStaRxframe, {staRxframe, "1.3.6.1.4.1.33940.6.1.1.8.1.1.6"}).
-define(FitapStaTxframe, {staTxframe, "1.3.6.1.4.1.33940.6.1.1.8.1.1.4"}).
-define(FitapStaRxbytes, {staRxbytes, "1.3.6.1.4.1.33940.6.1.1.8.1.1.7"}).
-define(FitapStaTxbytes, {staTxbytes, "1.3.6.1.4.1.33940.6.1.1.8.1.1.5"}).

-define(ApState, {apState, "1.3.6.1.4.1.33940.6.1.1.2.5.1.10"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap discover -- ap配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%系统信息配置
-define(ApMacAddr,{apMac,"1.3.6.1.4.1.33940.6.1.1.1.1.1.1"}). %apMAC地址
-define(ApName,{apName,"1.3.6.1.4.1.33940.6.1.1.1.1.1.2"}).
-define(ApType,{apType,"1.3.6.1.4.1.33940.6.1.1.1.1.1.5"}).
-define(ApSerialNo,{apSerialNo,"1.3.6.1.4.1.33940.6.1.1.1.1.1.11"}).
-define(ApSoftVersion,{apSoftVersion,"1.3.6.1.4.1.33940.6.1.1.1.1.1.6"}).
-define(ApIp,{apIp,"1.3.6.1.4.1.33940.6.1.1.1.5.1.1"}).
-define(ApMask,{apMask,"1.3.6.1.4.1.33940.6.1.1.1.5.1.2"}).

%%radio   APIndex + WirelessIFIndex
-define(FitapRadioPeriod, {radioPeriod, "1.3.6.1.4.1.33940.6.1.1.4.3.1.5"}).
-define(FitapRadioDtim, {radioDtim, "1.3.6.1.4.1.33940.6.1.1.4.3.1.7"}).
-define(FitapRadioRts, {radioRts, "1.3.6.1.4.1.33940.6.1.1.4.3.1.8"}).
-define(FitapRadioSlice, {radioSlice, "1.3.6.1.4.1.33940.6.1.1.4.3.1.6"}).

-define(FitapRadioModel, {radioModel, "1.3.6.1.4.1.33940.6.1.1.4.1.1.7"}).
-define(FitapRadioChannel, {radioChannel, "1.3.6.1.4.1.33940.6.1.1.4.1.1.6"}).
-define(FitapRadioPower, {radioPower, "1.3.6.1.4.1.33940.6.1.1.3.3.1.20"}).

%%security -- ssidIndex
-define(WlanSecurity, {wlanSecurity, "1.3.6.1.4.1.33940.6.1.2.13.6.1.6"}).

%%interface --- APIndex + WirelessIFIndex
-define(WireIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.33940.6.1.1.3.1.1.9"}).


-define(WirelessIfDescr, {ifDescr, "1.3.6.1.4.1.33940.6.1.1.3.3.1.2"}).
-define(WirelessIfType, {ifType, "1.3.6.1.4.1.33940.6.1.1.3.3.1.3"}).
-define(WirelessIfMTU, {ifMtu, "1.3.6.1.4.1.33940.6.1.1.3.3.1.4"}).
-define(WirelessIfSpeed, {ifSpeed, "1.3.6.1.4.1.33940.6.1.1.3.3.1.5"}).
-define(WirelessIfMacAddress, {ifPhysAddress, "1.3.6.1.4.1.33940.6.1.1.3.3.1.6"}).
-define(WirelessIfAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.33940.6.1.1.3.3.1.7"}).
-define(WirelessIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.33940.6.1.1.3.3.1.8"}).
%%----------------------------------------------------------------------
%% ApSsid
%%----------------------------------------------------------------------
-define(SsidAssoc,{ssidAssoc,"1.3.6.1.4.1.33940.6.1.2.12.4.1.1"}).
-define(DiscoApSsid,[
     {ssidName,             "1.3.6.1.4.1.33940.6.1.2.13.6.1.1"},
     {ssidEnabled,          "1.3.6.1.4.1.33940.6.1.2.13.6.1.2"},
     {ssidHidden,           "1.3.6.1.4.1.33940.6.1.2.13.6.1.3"},
     {staIsolate,           "1.3.6.1.4.1.33940.6.1.2.13.6.1.4"},
     {dot11Auth,            "1.3.6.1.4.1.33940.6.1.2.13.6.1.5"},
     {security,             "1.3.6.1.4.1.33940.6.1.2.13.6.1.6"},
     {authenMode,           "1.3.6.1.4.1.33940.6.1.2.13.6.1.7"},
     {securityCiphers,      "1.3.6.1.4.1.33940.6.1.2.13.6.1.8"},
     {vlanId,               "1.3.6.1.4.1.33940.6.1.2.13.6.1.18"},
     {maxSimultUsers,       "1.3.6.1.4.1.33940.6.1.2.13.6.1.19"}
]).

-define(MonApSsid,[
     %{ssidName,      "1.3.6.1.4.1.33940.6.1.1.5.4.1.4"},
     {ifOutPkts,   "1.3.6.1.4.1.33940.6.1.1.5.6.1.3"},
     {ifInPkts,    "1.3.6.1.4.1.33940.6.1.1.5.6.1.4"},
     {ifOutOctets, "1.3.6.1.4.1.33940.6.1.1.5.6.1.6"},
     {ifInOctets,  "1.3.6.1.4.1.33940.6.1.1.5.6.1.5"}
]).



