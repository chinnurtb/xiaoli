%"Motorola, Inc.","RFS7000 Wireless Switch"
%WSAP-5100-050-WWR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ac discover -- ac配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(AcName, {acName, "1.3.6.1.2.1.1.5.0"}).
-define(SysModel, {sysModel, "1.3.6.1.4.1.388.14.4.1.17.1.0"}).
-define(SoftwareVersion, {softVersion, "1.3.6.1.4.1.388.14.4.1.17.3.0"}).

%-define(PortalServerURL, {portalServerURL, "1.3.6.1.4.1.388.14.3.2.1.19.2.1.8"}).
%-define(AsIPAddress, {asIPAddress, ""}).

%-define(AcIf,[
%     {ifDescr,       "1.3.6.1.2.1.2.2.1.2"},
%     {ifType,        "1.3.6.1.2.1.2.2.1.3"},
%     {ifMtu,         "1.3.6.1.2.1.2.2.1.4"},
%     {ifSpeed,       "1.3.6.1.2.1.2.2.1.5"},
%     {ifPhysAddress, "1.3.6.1.2.1.2.2.1.6"},
%     {ifAdminStatus, "1.3.6.1.2.1.2.2.1.7"},
%     {ifOperStatus,  "1.3.6.1.2.1.2.2.1.8"}
%]).
%
% --- RadiusServerIndex 
-define(RadiusAuthServerIPAdd, {radiusAuthServerIPAdd, "1.3.6.1.4.1.388.14.3.2.1.14.2.1.1.1"}).
-define(RadiusAuthServerPort, {radiusAuthServerPort, "1.3.6.1.4.1.388.14.3.2.1.14.2.1.1.2"}).

% ----  vlaninterface
-define(IPPoolName, {ipPoolName, "1.3.6.1.4.1.388.14.2.3.5.3.1.6"}).
-define(IPPoolStartAddr, {ipPoolStartAddr, "1.3.6.1.4.1.388.14.2.3.5.4.1.1"}).
-define(IPPoolStopAddr, {ipPoolStopAddr, "1.3.6.1.4.1.388.14.2.3.5.4.1.2"}).

%%----------------------------------------------------------------------
%% ac monet --ac 性能采集
%%----------------------------------------------------------------------
-define(AcInfo,[ 
     {cpuRTUsage,      "1.3.6.1.4.1.388.14.1.6.1.4.44.0"  },
     {memRTUsage,      "1.3.6.1.4.1.388.14.1.6.1.15.8.0"  },
     {dHCPReqTimes,    "1.3.6.1.4.1.388.14.2.3.5.1.9.0"   },
     {dHCPReqSucTimes, "1.3.6.1.4.1.388.14.2.3.5.1.10.0"  },
     {dHCPIpPoolUsage, "1.3.6.1.4.1.388.14.2.3.5.1.8.0"   }, 
     {authNum,         "1.3.6.1.4.1.388.14.3.3.1.6.7.14.0"},
     {maxNum,          "1.3.6.1.4.1.388.14.3.3.1.6.4.0"   },
     {onlineNum,       "1.3.6.1.4.1.388.14.3.3.1.6.18.1.0"},
     {deauthNum,       "1.3.6.1.4.1.388.14.3.3.1.6.15.3.0"},
     {authReqNum,      "1.3.6.1.4.1.388.14.3.3.1.6.7.1.0" },
     {authSucNum,      "1.3.6.1.4.1.388.14.3.3.1.6.7.15.0"},
     {portalChallengeReqCount, "1.3.6.1.4.1.388.14.3.3.1.6.20.0"},
     {portalChallengeRespCount,"1.3.6.1.4.1.388.14.3.3.1.6.22.0"},
     {portalAuthReqCount,      "1.3.6.1.4.1.388.14.3.3.1.6.19.0"},
     {portalAuthRespCount,     "1.3.6.1.4.1.388.14.3.3.1.6.21.0"}
]).

%% interface
-define(InUcastPkts,  {ifInUcastPkts, "1.3.6.1.2.1.2.2.1.11"}).
-define(InNUcastPkts, {ifInNUcastPkts,"1.3.6.1.2.1.2.2.1.12"}).
-define(InOctets,     {ifInOctets,    "1.3.6.1.2.1.2.2.1.10"}).
-define(InDiscards,   {ifInDiscards,  "1.3.6.1.2.1.2.2.1.13"}).
-define(InErrors,     {ifInErrors,    "1.3.6.1.2.1.2.2.1.14"}).

-define(OutUcastPkts, {ifOutUcastPkts, "1.3.6.1.2.1.2.2.1.17"}).
-define(OutNUcastPkts,{ifOutNUcastPkts,"1.3.6.1.2.1.2.2.1.18"}).
-define(OutOctets,    {ifOutOctets,    "1.3.6.1.2.1.2.2.1.16"}).
-define(OutDiscards,  {ifOutDiscards,  "1.3.6.1.2.1.2.2.1.19"}).  
-define(OutErrors,    {ifOutErrors,    "1.3.6.1.2.1.2.2.1.20"}).  
%-define(AcUpDwnTimes,{ifUpDwnTimes, ""}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap monet --ap 性能采集% 
%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(MonWireless, [
     {ifInAvgSignal,  "1.3.6.1.4.1.388.14.3.3.1.2.11.1.10"},
     {ifInHighSignal, "1.3.6.1.4.1.388.14.3.3.1.2.2.1.16" },
     {ifInLowSignal,  "1.3.6.1.4.1.388.14.3.3.1.2.2.1.17" },
     {ifOutPkts,      "1.3.6.1.4.1.388.14.3.3.1.2.2.1.20" },
     {ifInPkts,       "1.3.6.1.4.1.388.14.3.3.1.2.2.1.21" },
     {ifInOctets,     "1.3.6.1.4.1.388.14.3.3.1.2.2.1.22" },
     {ifOutOctets,    "1.3.6.1.4.1.388.14.3.3.1.2.2.1.18" }
]).
-define(MonWire, [
     {ifInUcastPkts,   "1.3.6.1.4.1.388.14.3.2.1.9.12.1.11"},
     {ifInNUcastPkts,  "1.3.6.1.4.1.388.14.3.2.1.9.12.1.12"},
     {ifInOctets,      "1.3.6.1.4.1.388.14.3.2.1.9.12.1.10"},
     {ifInDiscards,    "1.3.6.1.4.1.388.14.3.2.1.9.12.1.13"},
     {ifInErrors,      "1.3.6.1.4.1.388.14.3.2.1.9.12.1.14"},
     {ifOutUcastPkts,  "1.3.6.1.4.1.388.14.3.2.1.9.12.1.16"},  
     {ifOutNUcastPkts, "1.3.6.1.4.1.388.14.3.2.1.9.12.1.17"},
     {ifOutOctets,     "1.3.6.1.4.1.388.14.3.2.1.9.12.1.15"},
     {ifOutDiscards,   "1.3.6.1.4.1.388.14.3.2.1.9.12.1.18"},
     {ifOutErrors,     "1.3.6.1.4.1.388.14.3.2.1.9.12.1.19"}
]).
-define(MonAssoc,[
     {apStationAssocSum, "1.3.6.1.4.1.388.14.3.3.1.10.1.1.10"},
     {apStationOnlineSum,"1.3.6.1.4.1.388.14.3.3.1.9.3.1.4"  },
     {assocNum,          "1.3.6.1.4.1.388.14.3.3.1.10.1.1.1" },
     {assocFailNum,      "1.3.6.1.4.1.388.14.3.3.1.10.1.1.2" },
     {deauthNum,         "1.3.6.1.4.1.388.14.3.3.1.9.3.1.5"  },
     {reAssocNum,        "1.3.6.1.4.1.388.14.3.3.1.10.1.1.3" },
     {reAssocFailNum,    "1.3.6.1.4.1.388.14.3.3.1.10.1.1.12"},
     {assocRefusedNum,   "1.3.6.1.4.1.388.14.3.3.1.10.1.1.5" },
     {cpuRTUsage,        "1.3.6.1.4.1.388.14.3.2.1.9.14.1.14"},
     {memRTUsage,        "1.3.6.1.4.1.388.14.3.2.1.9.14.1.13"}
]).

%-define(FitapSsidName,{ssidName, ""}).
-define(FitapSsidInOctets, {ssidInOctets, "1.3.6.1.4.1.388.14.3.3.1.10.2.1.12"}).
-define(FitapSsidInPkts, {ssidInPkts, "1.3.6.1.4.1.388.14.3.3.1.10.2.1.8"}).
-define(FitapSsidOutOctets, {ssidOutOctets, "1.3.6.1.4.1.388.14.3.3.1.10.2.1.7"}).
-define(FitapSsidOutPkts, {ssidOutPkts, "1.3.6.1.4.1.388.14.3.3.1.10.2.1.2"}).

-define(ApSta,[
     {apId,         "1.3.6.1.4.1.388.14.3.2.1.12.3.1.39"},
     {staMac,       "1.3.6.1.4.1.388.14.3.2.1.12.3.1.1"},
     {staIp,        "1.3.6.1.4.1.388.14.3.2.1.12.3.1.6"},
     {staRssi,      "1.3.6.1.4.1.388.14.3.3.1.3.1.1.13"},
     {staNoiseRate, "1.3.6.1.4.1.388.14.3.3.1.3.1.1.22"},
     {staChannel,   "1.3.6.1.4.1.388.14.3.2.1.12.3.1.32"},
     {staVlan,      "1.3.6.1.4.1.388.14.3.2.1.12.3.1.4"},
     {staSsid,      "1.3.6.1.4.1.388.14.3.2.1.12.3.1.33"},
     {staRxframe,   "1.3.6.1.4.1.388.14.3.3.1.3.1.1.25"},
     {staTxframe,   "1.3.6.1.4.1.388.14.3.3.1.3.1.1.23"},
     {staRxbytes,   "1.3.6.1.4.1.388.14.3.3.1.3.1.1.26"},
     {staTxbytes,   "1.3.6.1.4.1.388.14.3.3.1.3.1.1.24"} 
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap discover -- ap配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%系统信息配置
%ApMacAddr ApName ApMask 与 ApType ApSerialNo ApSoftVersion不在同一表上
-define(ApMacAddr,{apMac,"1.3.6.1.4.1.388.14.3.2.1.9.2.1.2"}). %apMAC地址
%-define(ApName,{apName,"1.3.6.1.4.1.388.14.3.2.1.24.1.1.4"}).
-define(ApType,{apType,"1.3.6.1.4.1.388.14.3.2.1.9.2.1.3"}).
-define(ApSerialNo,{apSerialNo,"1.3.6.1.4.1.388.14.3.2.1.9.2.1.4"}).
%-define(ApSoftVersion,{apSoftVersion,"1.3.6.1.4.1.388.14.3.2.1.9.3.1.3"}).
-define(ApIp,{apIp,"1.3.6.1.4.1.388.14.3.2.1.9.2.1.12"}).
%-define(ApMask,{apMask,"1.3.6.1.4.1.388.14.3.2.1.24.1.1.2"}).

%%radio   APIndex + WirelessIFIndex
-define(FitapRadioPeriod, {radioPeriod, "1.3.6.1.4.1.388.14.3.2.1.11.5.1.10"}).
-define(FitapRadioDtim, {radioDtim, "1.3.6.1.4.1.388.14.3.2.1.11.5.1.11"}).
-define(FitapRadioRts, {radioRts, "1.3.6.1.4.1.388.14.3.2.1.11.5.1.9"}).
-define(FitapRadioSlice, {radioSlice, "1.3.6.1.4.1.388.14.3.2.1.11.5.1.36"}).

-define(FitapRadioModel, {radioModel, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.15"}).
-define(FitapRadioChannel, {radioChannel, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.13"}).
-define(FitapRadioPower, {radioPower, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.20"}).

%%interface --- APIndex + WirelessIFIndex
%-define(WireIfOperStatus, {ifOperStatus, ""}).

-define(WirelessIfDescr, {ifDescr, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.1"}).
-define(WirelessIfType, {ifType, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.2"}).
-define(WirelessIfMTU, {ifMtu, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.10"}).
-define(WirelessIfSpeed, {ifSpeed, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.12"}).
-define(WirelessIfMacAddress, {ifPhysAddress, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.11"}).
-define(WirelessIfAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.6"}).
-define(WirelessIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.388.14.3.2.1.32.5.1.1.3"}).

-define(ApState, {apState, "1.3.6.1.4.1.388.14.3.2.1.24.1.1.10"}).

