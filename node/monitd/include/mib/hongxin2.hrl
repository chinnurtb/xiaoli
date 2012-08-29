% hongxin "FHAP2400"
%FIT-ZAG5000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ac discover -- ac配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(AcName, {acName, "1.3.6.1.4.1.33940.80.1.1.0"}).
-define(SysModel, {sysModel, "1.3.6.1.4.1.33940.80.1.44.0"}).
-define(SoftwareVersion, {softVersion, "1.3.6.1.4.1.33940.80.1.21.0"}).

-define(PortalServerURL, {portalServerURL, "1.3.6.1.4.1.33940.80.4.1.2.0"}).
-define(AsIPAddress, {asIPAddress, "1.3.6.1.4.1.33940.80.3.6.2.0"}).

%% interface
%-define(IfDescr, {ifDescr, "1.3.6.1.4.1.33940.80.6.1.15.1.2"}).
%-define(IfType, {ifType, "1.3.6.1.4.1.33940.80.6.1.15.1.5"}).
%-define(IfMTU, {ifMtu, "1.3.6.1.4.1.33940.80.6.1.15.1.6"}).
%-define(IfSpeed, {ifSpeed, "1.3.6.1.4.1.33940.80.6.1.15.1.7"}).
%-define(IfMacAddress, {ifPhysAddress, "1.3.6.1.4.1.33940.80.6.1.15.1.3"}).
%-define(IfAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.33940.80.6.1.15.1.9"}).
%-define(IfOperStatus, {ifOperStatus, "1.3.6.1.4.1.33940.80.6.1.15.1.4"}).

% --- RadiusServerIndex 
-define(RadiusAuthServerIPAdd, {radiusAuthServerIPAdd, "1.3.6.1.4.1.33940.80.4.4.6.1.2"}).
-define(RadiusAuthServerPort, {radiusAuthServerPort, "1.3.6.1.4.1.33940.80.4.4.6.1.3"}).

% ----  vlaninterface
-define(IPPoolName, {ipPoolName, "1.3.6.1.4.1.33940.80.2.4.5.1.1"}).
-define(IPPoolStartAddr, {ipPoolStartAddr, "1.3.6.1.4.1.33940.80.2.4.5.1.3"}).
-define(IPPoolStopAddr, {ipPoolStopAddr, "1.3.6.1.4.1.33940.80.2.4.5.1.4"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ac monet --ac 性能采集% 
%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(Acinfo, [
           {cpuRTUsage,     "1.3.6.1.4.1.33940.80.1.24.0"  },
           {memRTUsage,     "1.3.6.1.4.1.33940.80.1.25.0"  },
           {dHCPReqTimes,   "1.3.6.1.4.1.33940.80.6.1.16.0"},
           {dHCPReqSucTimes,"1.3.6.1.4.1.33940.80.6.1.17.0"},
           {dHCPIpPoolUsage,"1.3.6.1.4.1.33940.80.1.69.0"  },
           {onlineNum,      "1.3.6.1.4.1.33940.80.6.1.6.0" },
           {authNum,        "1.3.6.1.4.1.33940.80.6.1.27.0"},
           {maxNum,         "1.3.6.1.4.1.33940.80.6.1.32.0"},
           {normalNum,      "1.3.6.1.4.1.33940.80.6.1.43.0"},
           {deauthNum,      "1.3.6.1.4.1.33940.80.6.1.44.0"},
           {authReqNum,     "1.3.6.1.4.1.33940.80.6.1.23.0"},
           {authSucNum,     "1.3.6.1.4.1.33940.80.6.1.24.0"},
           {accReqNum,      "1.3.6.1.4.1.33940.80.6.1.81.0"},
           {accSucNum,      "1.3.6.1.4.1.33940.80.6.1.82.0"},
           {radiusReqPkts,  "1.3.6.1.4.1.33940.80.6.1.67.0"},
           {radiusRepPkts,  "1.3.6.1.4.1.33940.80.6.1.68.0"},
           {portalChallengeReqCount, "1.3.6.1.4.1.33940.80.6.1.90.0"},
           {portalChallengeRespCount,"1.3.6.1.4.1.33940.80.6.1.91.0"},
           {portalAuthReqCount,      "1.3.6.1.4.1.33940.80.6.1.88.0"},
           {portalAuthRespCount,     "1.3.6.1.4.1.33940.80.6.1.89.0"},
           {leaveReqCount,          "1.3.6.1.4.1.33940.80.6.1.69.0"},
           {leaveRepCount,          "1.3.6.1.4.1.33940.80.6.1.70.0"}
           %{radiusAvgDelay,""}.
           %{bandWidth,     ""}.
]).

%% interface
%-define(AcInUcastPkts, {ifInUcastPkts, "1.3.6.1.4.1.33940.80.6.1.15.1.19"}).
%-define(AcInNUcastPkts,{ifInNUcastPkts, "1.3.6.1.4.1.33940.80.6.1.15.1.20"}).
%-define(AcInOctets, {ifInOctets, "1.3.6.1.4.1.33940.80.6.1.15.1.14"}).
%-define(AcInDiscards,{ifInDiscards, "1.3.6.1.4.1.33940.80.6.1.15.1.26"}).
%-define(AcInErrors,{ifInErrors, "1.3.6.1.4.1.33940.80.6.1.15.1.23"}).
%-define(AcOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.4.1.33940.80.6.1.15.1.21"}).
%-define(AcOutNUcastPkts,{ifOutNUcastPkts, "1.3.6.1.4.1.33940.80.6.1.15.1.22"}).
%-define(AcOutOctets, {ifOutOctets, "1.3.6.1.4.1.33940.80.6.1.15.1.15"}).
%-define(AcOutDiscards,{ifOutDiscards, "1.3.6.1.4.1.33940.80.6.1.15.1.27"}).
%-define(AcOutErrors,{ifOutErrors, "1.3.6.1.4.1.33940.80.6.1.15.1.24"}).
%-define(AcUpDwnTimes,{ifUpDwnTimes, "1.3.6.1.4.1.33940.80.6.1.15.1.25"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap monet --ap 性能采集% 
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%ap 无线接口性能  
-define(FitapWirelessInAvgSignal,{ifInAvgSignal, "1.3.6.1.4.1.33940.80.6.15.3.1.4"}).
-define(FitapWirelessInHighSignal,{ifInHighSignal, "1.3.6.1.4.1.33940.80.6.15.3.1.5"}).
-define(FitapWirelessInLowSignal,{ifInLowSignal, "1.3.6.1.4.1.33940.80.6.15.3.1.6"}).
-define(FitapWirelessFrameRetryRate,{fitapWirelessFrameRetryRate, "1.3.6.1.4.1.33940.80.6.2.1.32"}).

-define(FitapWirelessOutPkts,{ifOutPkts, "1.3.6.1.4.1.33940.80.6.15.3.1.11"}).
-define(FitapWirelessInPkts,{ifInPkts, "1.3.6.1.4.1.33940.80.6.15.3.1.12"}).
-define(FitapWirelessInOctets,{ifInOctets, "1.3.6.1.4.1.33940.80.6.15.3.1.14"}).
-define(FitapWirelessOutOctets,{ifOutOctets, "1.3.6.1.4.1.33940.80.6.15.3.1.13"}).
-define(FitapWirelessInErrors,{ifInErrors, "1.3.6.1.4.1.33940.80.6.15.3.1.25"}).



%%ap 有线接口性能  
-define(FitapWireInUcastPkts, {ifInUcastPkts, "1.3.6.1.4.1.33940.80.6.15.2.1.3"}).
-define(FitapWireInNUcastPkts,{ifInNUcastPkts, "1.3.6.1.4.1.33940.80.6.15.2.1.4"}).
-define(FitapWireInOctets,{ifInOctets, "1.3.6.1.4.1.33940.80.6.15.2.1.5"}).
-define(FitapWireInDiscards,{ifInDiscards, "1.3.6.1.4.1.33940.80.6.15.2.1.6"}).
-define(FitapWireInErrors,{ifInErrors, "1.3.6.1.4.1.33940.80.6.15.2.1.7"}).

-define(FitapWireOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.4.1.33940.80.6.15.2.1.8"}).  
-define(FitapWireOutNUcastPkts,{ifOutNUcastPkts, "1.3.6.1.4.1.33940.80.6.15.2.1.9"}).
-define(FitapWireOutOctets,{ifOutOctets, "1.3.6.1.4.1.33940.80.6.15.2.1.10"}).
-define(FitapWireOutDiscards,{ifOutDiscards, "1.3.6.1.4.1.33940.80.6.15.2.1.11"}).
-define(FitapWireOutErrors,{ifOutErrors, "1.3.6.1.4.1.33940.80.6.15.2.1.12"}).
  
%%ap连接信息统计
-define(MonAssoc,[
     {assocNum,           "1.3.6.1.4.1.33940.80.6.2.1.41"},
     {assocFailNum,       "1.3.6.1.4.1.33940.80.6.2.1.42"},
     {reAssocNum,         "1.3.6.1.4.1.33940.80.6.17.1.6"},
     {assocRefusedNum,    "1.3.6.1.4.1.33940.80.6.2.1.256"},
     %{deauthNum,          "1.3.6.1.4.1.33940.80.6.17.1.7"},
     {apStationAssocSum,  "1.3.6.1.4.1.33940.80.6.17.1.2"},
     {apStationOnlineSum, "1.3.6.1.4.1.33940.80.6.22.1.2"},
     {cpuRTUsage,         "1.3.6.1.4.1.33940.80.6.18.1.2"},
     {memRTUsage,         "1.3.6.1.4.1.33940.80.6.18.1.4"}

     %{reAssocFailNum,     ""},
]).


%%ssid性能统计 

%-define(FitapSsidEnbale,{ssidEnbale, "1.3.6.1.4.1.33940.80.3.13.1.3"}).
-define(FitapSsidName,{ssidName, "1.3.6.1.4.1.33940.80.6.16.1.3"}).
-define(FitapSsidInOctets, {ssidInOctets, "1.3.6.1.4.1.33940.80.6.16.1.17"}).
-define(FitapSsidInPkts, {ssidInPkts, "1.3.6.1.4.1.33940.80.6.16.1.19"}).
-define(FitapSsidOutOctets, {ssidOutOctets, "1.3.6.1.4.1.33940.80.6.16.1.18"}).
-define(FitapSsidOutPkts, {ssidOutPkts, "1.3.6.1.4.1.33940.80.6.16.1.20"}).

%%sta终端性能统计  --- StaIndex
-define(FitapMac, {apId, "1.3.6.1.4.1.33940.80.6.3.1.5"}).
-define(FitapStaMac, {staMac, "1.3.6.1.4.1.33940.80.6.3.1.2"}).
-define(FitapStaIP, {staIp, "1.3.6.1.4.1.33940.80.6.3.1.6"}).
-define(FitapStaRssi, {staRssi, "1.3.6.1.4.1.33940.80.6.3.1.8"}).
-define(FitapStaNoiseRate, {staNoiseRate, "1.3.6.1.4.1.33940.80.6.3.1.31"}).
-define(FitapStaChannel, {staChannel, "1.3.6.1.4.1.33940.80.6.3.1.32"}).
-define(FitapStaVlan, {staVlan, "1.3.6.1.4.1.33940.80.6.3.1.33"}).
-define(FitapStaSsid, {staSsid, "1.3.6.1.4.1.33940.80.6.3.1.9"}).
-define(FitapStaRxframe, {staRxframe, "1.3.6.1.4.1.33940.80.6.3.1.11"}).
-define(FitapStaTxframe, {staTxframe, "1.3.6.1.4.1.33940.80.6.3.1.13"}).
-define(FitapStaRxbytes, {staRxbytes, "1.3.6.1.4.1.33940.80.6.3.1.12"}).
-define(FitapStaTxbytes, {staTxbytes, "1.3.6.1.4.1.33940.80.6.3.1.14"}).

-define(StaColumns,[
    ?FitapStaMac,
    ?FitapMac,
    ?FitapStaIP,
    ?FitapStaRssi,
    ?FitapStaRxframe,
    ?FitapStaRxbytes,
    ?FitapStaTxframe,
    ?FitapStaTxbytes,
    ?FitapStaSsid,
    ?FitapStaNoiseRate,
    ?FitapStaChannel,
    ?FitapStaVlan
    ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap discover -- ap配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%系统信息配置
-define(ApMacAddr,{apMac,"1.3.6.1.4.1.33940.80.6.2.1.2"}). %apMAC地址
-define(ApName,{apName,"1.3.6.1.4.1.33940.80.6.2.1.4"}).
-define(ApType,{apType,"1.3.6.1.4.1.33940.80.6.2.1.274"}).
-define(ApSerialNo,{apSerialNo,"1.3.6.1.4.1.33940.80.6.18.1.11"}).
-define(ApSoftVersion,{apSoftVersion,"1.3.6.1.4.1.33940.80.6.2.1.54"}).
-define(ApIp,{apIp,"1.3.6.1.4.1.33940.80.6.2.1.3"}).
-define(ApMask,{apMask,"1.3.6.1.4.1.33940.80.6.2.1.52"}).

%%radio   APIndex + WirelessIFIndex
-define(FitapRadioPeriod, {radioPeriod, "1.3.6.1.4.1.33940.80.3.9.1.3"}).
-define(FitapRadioDtim, {radioDtim, "1.3.6.1.4.1.33940.80.3.9.1.4"}).
-define(FitapRadioRts, {radioRts, "1.3.6.1.4.1.33940.80.3.9.1.5"}).
-define(FitapRadioSlice, {radioSlice, "1.3.6.1.4.1.33940.80.3.9.1.6"}).

-define(FitapRadioModel, {radioModel, "1.3.6.1.4.1.33940.80.3.8.1.15"}).
-define(FitapRadioChannel, {radioChannel, "1.3.6.1.4.1.33940.80.3.8.1.13"}).
-define(FitapRadioPower, {radioPower, "1.3.6.1.4.1.33940.80.3.8.1.18"}).

%%security -- ssidIndex
-define(WlanSecurity, {wlanSecurity, "1.3.6.1.4.1.33940.80.3.13.1.7"}).

%%interface --- APIndex + WirelessIFIndex
-define(WireIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.33940.80.3.10.1.7"}).

-define(WirelessIfDescr, {ifDescr, "1.3.6.1.4.1.33940.80.3.8.1.3"}).
-define(WirelessIfType, {ifType, "1.3.6.1.4.1.33940.80.3.8.1.4"}).
-define(WirelessIfMTU, {ifMtu, "1.3.6.1.4.1.33940.80.3.8.1.5"}).
-define(WirelessIfSpeed, {ifSpeed, "1.3.6.1.4.1.33940.80.3.8.1.6"}).
-define(WirelessIfMacAddress, {ifPhysAddress, "1.3.6.1.4.1.33940.80.3.8.1.7"}).
-define(WirelessIfAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.33940.80.3.8.1.8"}).
-define(WirelessIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.33940.80.3.8.1.9"}).

-define(ApState, {apState, "1.3.6.1.4.1.33940.80.6.2.1.5"}).

%%----------------------------------------------------------------------
%% ApSsid
%%----------------------------------------------------------------------
-define(SsidAssoc,{ssidAssoc,"1.3.6.1.4.1.33940.80.6.16.1.6"}).
-define(DiscoApSsid,[
    {ssidName,             "1.3.6.1.4.1.33940.80.3.13.1.2" },
    {ssidEnabled,          "1.3.6.1.4.1.33940.80.3.13.1.3" },
    {ssidHidden,           "1.3.6.1.4.1.33940.80.3.13.1.4" },
    {staIsolate,           "1.3.6.1.4.1.33940.80.3.13.1.5" },
    {dot11Auth,            "1.3.6.1.4.1.33940.80.3.13.1.6" },
    {security,             "1.3.6.1.4.1.33940.80.3.13.1.7" },
    {authenMode,           "1.3.6.1.4.1.33940.80.3.13.1.8" },
    {securityCiphers,      "1.3.6.1.4.1.33940.80.3.13.1.9" },
    {vlanId,               "1.3.6.1.4.1.33940.80.3.13.1.10"},
    {maxSimultUsers,       "1.3.6.1.4.1.33940.80.3.13.1.11"}
]).



