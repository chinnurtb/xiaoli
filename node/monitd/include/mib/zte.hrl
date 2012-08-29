% zte "ZXV10_W901"
% W815
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ac discover -- ac配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(AcName, {acName, "1.3.6.1.4.1.3902.1.1.1.0"}).
-define(SysModel, {sysModel, "1.3.6.1.4.1.3902.1.1.44.0"}).
-define(SoftwareVersion, {softVersion, "1.3.6.1.4.1.3902.1.1.21.0"}).
-define(PortalServerURL, {portalServerURL, "1.3.6.1.4.1.3902.1.4.1.2.0"}).
-define(AsIPAddress, {asIPAddress, "1.3.6.1.4.1.3902.1.3.6.2.0"}).

%%% interface
%-define(IfDescr, {ifDescr, "1.3.6.1.4.1.3902.1.6.1.15.1.2"}).
%-define(IfType, {ifType, "1.3.6.1.4.1.3902.1.6.1.15.1.5"}).
%-define(IfMTU, {ifMtu, "1.3.6.1.4.1.3902.1.6.1.15.1.6"}).
%-define(IfSpeed, {ifSpeed, "1.3.6.1.4.1.3902.1.6.1.15.1.7"}).
%-define(IfMacAddress, {ifPhysAddress, "1.3.6.1.4.1.3902.1.6.1.15.1.3"}).
%-define(IfAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.3902.1.6.1.15.1.9"}).
%-define(IfOperStatus, {ifOperStatus, "1.3.6.1.4.1.3902.1.6.1.15.1.4"}).

% --- RadiusServerIndex 
-define(RadiusAuthServerIPAdd, {radiusAuthServerIPAdd, "1.3.6.1.4.1.3902.1.4.4.6.1.2"}).
-define(RadiusAuthServerPort, {radiusAuthServerPort, "1.3.6.1.4.1.3902.1.4.4.6.1.3"}).

% ----  vlaninterface
%-define(IPPoolName, {ipPoolName, "1.3.6.1.4.1.3902.1.2.4.5.1.14"}).
-define(IPPoolStartAddr, {ipPoolStartAddr, "1.3.6.1.4.1.3902.1.2.4.5.1.3"}).
-define(IPPoolStopAddr, {ipPoolStopAddr, "1.3.6.1.4.1.3902.1.2.4.5.1.4"}).

%%----------------------------------------------------------------------
%% ac monet --ac 性能采集
%%----------------------------------------------------------------------
-define(Acinfo, [
     {cpuRTUsage,               "1.3.6.1.4.1.3902.1.1.24.0"  },
     {memRTUsage,               "1.3.6.1.4.1.3902.1.1.25.0"  },
     {dHCPReqTimes,             "1.3.6.1.4.1.3902.1.6.1.16.0"},
     {dHCPReqSucTimes,          "1.3.6.1.4.1.3902.1.6.1.17.0"},
     {onlineNum,                "1.3.6.1.4.1.3902.1.6.1.6.0" },
     {authNum,                  "1.3.6.1.4.1.3902.1.6.1.27.0"},
     {authReqNum,               "1.3.6.1.4.1.3902.1.6.1.23.0"},
     {authSucNum,               "1.3.6.1.4.1.3902.1.6.1.24.0"},
     {dHCPIpPoolUsage,          "1.3.6.1.4.1.3902.1.6.1.18.0"}, 
    
    %{maxNum,                   "1.3.6.1.4.1.3902.1.6.1.32.0"},
    %{normalNum,                "1.3.6.1.4.1.3902.1.6.1.43.0"},
    {deauthNum,                "1.3.6.1.4.1.3902.1.6.1.44.0"}
    %{accReqNum,                "1.3.6.1.4.1.3902.1.6.1.81.0"},
    %{accSucNum,                "1.3.6.1.4.1.3902.1.6.1.82.0"},
    %{radiusReqPkts,            "1.3.6.1.4.1.3902.1.6.1.67.0"},
    %{radiusRepPkts,            "1.3.6.1.4.1.3902.1.6.1.68.0"},
    %{leaveReqPkts,             "1.3.6.1.4.1.3902.1.6.1.69.0"},
    %{leaveRepPkts,             "1.3.6.1.4.1.3902.1.6.1.70.0"}
    
    %{bandWidth,                ""},
    %{radiusAvgDelay,           ""},
    %{portalChallengeReqCount,  ""},
    %{portalChallengeRespCount, ""},
    %{portalAuthReqCount,       ""},
    %{portalAuthRespCount,      ""},
    %{leaveReqCount,            ""},
    %{leaveRepCount,            ""},
    %{addressCount,             ""},
]).


-define(Acintf, [
     {ifInUcastPkts, "1.3.6.1.4.1.3902.1.6.1.15.1.19"},
     {ifInNUcastPkts,"1.3.6.1.4.1.3902.1.6.1.15.1.20"},
     {ifInOctets,    "1.3.6.1.4.1.3902.1.6.1.15.1.14"},
     {ifInDiscards,  "1.3.6.1.4.1.3902.1.6.1.15.1.26"},
     {ifInErrors,    "1.3.6.1.4.1.3902.1.6.1.15.1.23"},
     {ifOutUcastPkts,"1.3.6.1.4.1.3902.1.6.1.15.1.21"},
     {ifOutNUcastPkts,"1.3.6.1.4.1.3902.1.6.1.15.1.22"},
     {ifOutOctets,   "1.3.6.1.4.1.3902.1.6.1.15.1.15"},
     {ifOutDiscards, "1.3.6.1.4.1.3902.1.6.1.15.1.27"},
     {ifOutErrors,   "1.3.6.1.4.1.3902.1.6.1.15.1.24"},
     {ifUpDwnTimes,  "1.3.6.1.4.1.3902.1.6.1.15.1.25"}
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap monet --ap 性能采集% 
%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(MonWireless, [
     {ifOutPkts,      "1.3.6.1.4.1.3902.1.6.2.1.17" },
     {ifInPkts,       "1.3.6.1.4.1.3902.1.6.2.1.16" },
     {ifInOctets,     "1.3.6.1.4.1.3902.1.6.2.1.18" },
     {ifOutOctets,    "1.3.6.1.4.1.3902.1.6.2.1.19" },
     {ifInHighSignal, "1.3.6.1.4.1.3902.1.6.2.1.380"},
     {ifInLowSignal,  "1.3.6.1.4.1.3902.1.6.2.1.381"},
     {ifInAvgSignal,  "1.3.6.1.4.1.3902.1.6.2.1.6"  },
     {ifInErrors,     "1.3.6.1.4.1.3902.1.6.2.1.29" }
     %{fitapWirelessFrameRetryRate, ""}
]).

-define(MonWire, [
     {ifInUcastPkts,  "1.3.6.1.4.1.3902.1.6.2.1.313"},
     {ifInNUcastPkts, "1.3.6.1.4.1.3902.1.6.2.1.316"},
     {ifInOctets,     "1.3.6.1.4.1.3902.1.6.2.1.291"},
     {ifInDiscards,   "1.3.6.1.4.1.3902.1.6.2.1.299"},
     {ifInErrors,     "1.3.6.1.4.1.3902.1.6.2.1.294"},
     {ifOutUcastPkts, "1.3.6.1.4.1.3902.1.6.2.1.314"},  
     {ifOutNUcastPkts,"1.3.6.1.4.1.3902.1.6.2.1.315"},
     {ifOutOctets,    "1.3.6.1.4.1.3902.1.6.2.1.292"},
     {ifOutDiscards,  "1.3.6.1.4.1.3902.1.6.2.1.298"},
     {ifOutErrors,    "1.3.6.1.4.1.3902.1.6.2.1.297"}
]).

%%ap连接信息统计
-define(MonAssoc,[
     {assocNum,           "1.3.6.1.4.1.3902.1.6.2.1.41" },
     {assocFailNum,       "1.3.6.1.4.1.3902.1.6.2.1.42" },
     {apStationOnlineSum, "1.3.6.1.4.1.3902.1.6.22.1.2" },
     {cpuRTUsage,         "1.3.6.1.4.1.3902.1.6.2.1.375"},
     {memRTUsage,         "1.3.6.1.4.1.3902.1.6.2.1.377"}

     %{apStationAssocSum, "1.3.6.1.4.1.3902.1.6.3.1.22"},
     %{reAssocNum,        ""},
     %{assocRefusedNum,   ""},
     %{deauthNum,         ""},
     %{reAssocFailNum,    ""},
]).
%%ssid性能统计 

%-define(FitapSsidEnbale,{ssidEnbale, "1.3.6.1.4.1.3902.1.3.13.1.3"}).
%-define(FitapSsidName,{ssidName, "1.3.6.1.4.1.3902.1.6.16.1.3"}).
%-define(FitapSsidInOctets, {ssidInOctets, "1.3.6.1.4.1.3902.1.6.16.1.17"}).
%-define(FitapSsidInPkts, {ssidInPkts, "1.3.6.1.4.1.3902.1.6.16.1.19"}).
%-define(FitapSsidOutOctets, {ssidOutOctets, "1.3.6.1.4.1.3902.1.6.16.1.18"}).
%-define(FitapSsidOutPkts, {ssidOutPkts, "1.3.6.1.4.1.3902.1.6.16.1.20"}).

%%sta终端性能统计  --- StaIndex
-define(FitapMac, {apId, "1.3.6.1.4.1.3902.1.6.3.1.5"}).
-define(FitapStaMac, {staMac, "1.3.6.1.4.1.3902.1.6.3.1.2"}).
-define(FitapStaIP, {staIp, "1.3.6.1.4.1.3902.1.6.3.1.6"}).
-define(FitapStaRssi, {staRssi, "1.3.6.1.4.1.3902.1.6.3.1.8"}).
-define(FitapStaNoiseRate, {staNoiseRate, "1.3.6.1.4.1.3902.1.6.3.1.31"}).
-define(FitapStaChannel, {staChannel, "1.3.6.1.4.1.3902.1.6.3.1.32"}).
-define(FitapStaVlan, {staVlan, "1.3.6.1.4.1.3902.1.6.3.1.33"}).
-define(FitapStaSsid, {staSsid, "1.3.6.1.4.1.3902.1.6.3.1.9"}).
-define(FitapStaRxframe, {staRxframe, "1.3.6.1.4.1.3902.1.6.3.1.11"}).
-define(FitapStaTxframe, {staTxframe, "1.3.6.1.4.1.3902.1.6.3.1.13"}).
-define(FitapStaRxbytes, {staRxbytes, "1.3.6.1.4.1.3902.1.6.3.1.12"}).
-define(FitapStaTxbytes, {staTxbytes, "1.3.6.1.4.1.3902.1.6.3.1.14"}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap discover -- ap配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%系统信息配置
-define(ApMacAddr,{apMac,"1.3.6.1.4.1.3902.1.6.2.1.2"}). %apMAC地址
-define(ApName,{apName,"1.3.6.1.4.1.3902.1.6.2.1.4"}).
-define(ApType,{apType,"1.3.6.1.4.1.3902.1.6.2.1.274"}).
-define(ApSerialNo,{apSerialNo,"1.3.6.1.4.1.3902.1.6.2.1.75"}).
-define(ApSoftVersion,{apSoftVersion,"1.3.6.1.4.1.3902.1.6.2.1.54"}).
-define(ApIp,{apIp,"1.3.6.1.4.1.3902.1.6.2.1.3"}).
-define(ApMask,{apMask,"1.3.6.1.4.1.3902.1.6.2.1.52"}).

%%radio   APIndex + WirelessIFIndex
%-define(FitapRadioPeriod, {radioPeriod, "1.3.6.1.4.1.3902.1.3.9.1.3"}).
%-define(FitapRadioDtim, {radioDtim, "1.3.6.1.4.1.3902.1.3.9.1.4"}).
%-define(FitapRadioRts, {radioRts, "1.3.6.1.4.1.3902.1.3.9.1.5"}).
%-define(FitapRadioSlice, {radioSlice, "1.3.6.1.4.1.3902.1.3.9.1.6"}).

%-define(FitapRadioModel, {radioModel, "1.3.6.1.4.1.3902.1.3.8.1.15"}).
%-define(FitapRadioChannel, {radioChannel, "1.3.6.1.4.1.3902.1.3.8.1.13"}).
%-define(FitapRadioPower, {radioPower, "1.3.6.1.4.1.3902.1.3.8.1.18"}).


%%interface --- APIndex + WirelessIFIndex
%-define(WireIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.3902.1.3.10.1.7"}).

-define(WirelessIfDescr, {ifDescr, "1.3.6.1.4.1.3902.1.6.15.2.1.1"}).
-define(WirelessIfType, {ifType, "1.3.6.1.4.1.3902.1.6.15.2.1.2"}).
-define(WirelessIfMTU, {ifMtu, "1.3.6.1.4.1.3902.1.6.15.2.1.3"}).
-define(WirelessIfSpeed, {ifSpeed, "1.3.6.1.4.1.3902.1.6.15.2.1.5"}).
-define(WirelessIfMacAddress, {ifPhysAddress, "1.3.6.1.4.1.3902.1.6.15.2.1.4"}).
-define(WirelessIfAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.3902.1.6.15.2.1.7"}).
%-define(WirelessIfOperStatus, {ifOperStatus, ""}).

-define(ApState, {apState, "1.3.6.1.4.1.3902.1.6.2.1.5"}).

