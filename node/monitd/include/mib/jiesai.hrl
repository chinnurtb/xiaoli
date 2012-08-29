%jiesai JSAC-AT7605
%JS2400_InTeleAP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ac discover -- ac配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(AcName, {acName, "1.3.6.1.4.1.34230.3.1.1.1.1.1.0"}).
-define(SysModel, {sysModel, "1.3.6.1.4.1.34230.3.1.1.1.1.18.0"}).
-define(SoftwareVersion, {softVersion, "1.3.6.1.4.1.34230.3.1.1.1.1.19.0"}).

%-define(PortalServerURL, {portalServerURL, "1.3.6.1.4.1.34230.3.1.1.1.11.1.2.0"}).
-define(AsIPAddress, {asIPAddress, "1.3.6.1.4.1.34230.3.1.1.1.1.17.0"}).

%%% interface 1: znAcBoardIndex 2: znAcIfIndex 
-define(EthIndex, {ifIndex, "1.3.6.1.4.1.34230.3.1.1.4.2.1.2"}).
-define(EthDescr, {ifDescr, "1.3.6.1.4.1.34230.3.1.1.4.2.1.3"}).
-define(EthType, {ifType, "1.3.6.1.4.1.34230.3.1.1.4.2.1.4"}).
-define(EthMTU, {ifMtu, "1.3.6.1.4.1.34230.3.1.1.4.2.1.5"}).
-define(EthSpeed, {ifSpeed, "1.3.6.1.4.1.34230.3.1.1.4.2.1.6"}).
-define(EthMacAddress, {ifPhysAddress, "1.3.6.1.4.1.34230.3.1.1.4.2.1.7"}).
-define(EthAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.34230.3.1.1.4.2.1.8"}).
-define(EthOperStatus, {ifOperStatus, "1.3.6.1.4.1.34230.3.1.1.4.2.1.9"}).

-define(EthEntry, [
    ?EthIndex,
    ?EthDescr,
    ?EthType,
    ?EthMTU,
    ?EthSpeed,
    ?EthMacAddress,
    ?EthAdminStatus,
    ?EthOperStatus
]).

% --- RadiusServerIndex 
%-define(RadiusAuthServerIPAdd, {radiusAuthServerIPAdd, "1.3.6.1.4.1.34230.3.3.5.1.2"}).
%-define(RadiusAuthServerPort, {radiusAuthServerPort, "1.3.6.1.4.1.34230.3.3.5.1.3"}).

% ----  vlaninterface
-define(IPPoolName, {ipPoolName, "1.3.6.1.4.1.34230.3.1.1.1.10.1.11"}).
-define(IPPoolStartAddr, {ipPoolStartAddr, "1.3.6.1.4.1.34230.3.1.1.1.10.1.3"}).
-define(IPPoolStopAddr, {ipPoolStopAddr, "1.3.6.1.4.1.34230.3.1.1.1.10.1.4"}).

%%----------------------------------------------------------------------
%% ac monet --ac 性能采集
%%----------------------------------------------------------------------

-define(CpuRTUsage, {cpuRTUsage, "1.3.6.1.4.1.34230.3.1.1.1.1.87.0"}).
-define(MemRTUsage, {memRTUsage, "1.3.6.1.4.1.34230.3.1.1.1.1.89.0"}).
%现取上行带宽
-define(BandWidth, {bandWidth, "1.3.6.1.4.1.34230.3.1.1.1.1.47.0"}).
-define(DHCPReqTimes, {dHCPReqTimes, "1.3.6.1.4.1.34230.3.1.1.1.1.76.0"}).
-define(DHCPReqSucTimes,{dHCPReqSucTimes, "1.3.6.1.4.1.34230.3.1.1.1.1.77.0"}).
-define(DHCPIpPoolUsage,{dHCPIpPoolUsage, "1.3.6.1.4.1.34230.3.1.1.1.1.75.0"}).

-define(OnlineNum, {onlineNum, "1.3.6.1.4.1.34230.3.1.1.1.1.7.0"}).
-define(AuthNum, {authNum, "1.3.6.1.4.1.34230.3.1.1.1.1.74.0"}).
%-define(MaxNum, {maxNum, ""}).
-define(NormalNum,   {normalNum, "1.3.6.1.4.1.34230.3.1.1.1.1.99.0" }).
-define(AcDeauthNum, {deauthNum, "1.3.6.1.4.1.34230.3.1.1.1.1.100.0"}).
-define(AuthReqNum,  {authReqNum, "1.3.6.1.4.1.34230.3.1.1.1.31.1.0"}).%bug 642
-define(AuthSucNum,  {authSucNum, "1.3.6.1.4.1.34230.3.1.1.1.31.2.0"}).
%-define(AccReqNum, {accReqNum, "1.3.6.1.4.1.34230.3.1.1.1.1.111.0"}).
%-define(AccSucNum, {accSucNum, "1.3.6.1.4.1.34230.3.1.1.1.1.112.0"}).

%-define(RadiusReqPkts, {radiusReqPkts, ""}).
%-define(RadiusRepPkts, {radiusRepPkts, ""}).
%-define(LeaveReqPkts, {leaveReqPkts, ""}).
%-define(LeaveRepPkts, {leaveRepPkts, ""}).
-define(RadiusAvgDelay, {radiusAvgDelay, "1.3.6.1.4.1.34230.3.1.1.1.1.103.0"}).
%% interface
-define(AcInUcastPkts, {ifInUcastPkts, "1.3.6.1.4.1.34230.3.1.1.4.2.1.12"}).
-define(AcInNUcastPkts,{ifInNUcastPkts, "1.3.6.1.4.1.34230.3.1.1.4.2.1.13"}).
-define(AcInOctets, {ifInOctets, "1.3.6.1.4.1.34230.3.1.1.4.2.1.36"}).
-define(AcInDiscards,{ifInDiscards, "1.3.6.1.4.1.34230.3.1.1.4.2.1.14"}).
-define(AcInErrors,{ifInErrors, "1.3.6.1.4.1.34230.3.1.1.4.2.1.15"}).

-define(AcOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.4.1.34230.3.1.1.4.2.1.18"}).
-define(AcOutNUcastPkts,{ifOutNUcastPkts, "1.3.6.1.4.1.34230.3.1.1.4.2.1.19"}).
-define(AcOutOctets, {ifOutOctets, "1.3.6.1.4.1.34230.3.1.1.4.2.1.37"}).
-define(AcOutDiscards,{ifOutDiscards, "1.3.6.1.4.1.34230.3.1.1.4.2.1.20"}).
-define(AcOutErrors,{ifOutErrors, "1.3.6.1.4.1.34230.3.1.1.4.2.1.21"}).

-define(AcUpDwnTimes,{ifUpDwnTimes, "1.3.6.1.4.1.34230.3.1.1.4.2.1.22"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap monet --ap 性能采集% 
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%ap 无线接口性能  

-define(FitapWirelessInAvgSignal,{ifInAvgSignal, "1.3.6.1.4.1.34230.3.1.1.3.7.1.72"}).
-define(FitapWirelessInHighSignal,{ifInHighSignal, "1.3.6.1.4.1.34230.3.1.1.3.7.1.73"}).
-define(FitapWirelessInLowSignal,{ifInLowSignal, "1.3.6.1.4.1.34230.3.1.1.3.7.1.74"}).

-define(FitapWirelessOutPkts,{ifOutPkts, "1.3.6.1.4.1.34230.3.1.1.3.7.1.11"}).
-define(FitapWirelessInPkts,{ifInPkts, "1.3.6.1.4.1.34230.3.1.1.3.7.1.10"}).
-define(FitapWirelessInOctets,{ifInOctets, "1.3.6.1.4.1.34230.3.1.1.3.7.1.12"}).
-define(FitapWirelessOutOctets,{ifOutOctets, "1.3.6.1.4.1.34230.3.1.1.3.7.1.13"}).

-define(FitapWirelessInErrors,{ifInErrors, "1.3.6.1.4.1.34230.3.1.1.3.7.1.71"}).

%%ap 有线接口性能  
-define(FitapWireInUcastPkts, {ifInUcastPkts, "1.3.6.1.4.1.34230.3.1.1.3.6.1.9"}).
-define(FitapWireInNUcastPkts,{ifInNUcastPkts, "1.3.6.1.4.1.34230.3.1.1.3.6.1.16"}).
-define(FitapWireInOctets,{ifInOctets, "1.3.6.1.4.1.34230.3.1.1.3.6.1.17"}).
-define(FitapWireInDiscards,{ifInDiscards, "1.3.6.1.4.1.34230.3.1.1.3.6.1.12"}).
-define(FitapWireInErrors,{ifInErrors, "1.3.6.1.4.1.34230.3.1.1.3.6.1.18"}).
-define(FitapWireOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.4.1.34230.3.1.1.3.6.1.13"}).
-define(FitapWireOutNUcastPkts,{ifOutNUcastPkts, "1.3.6.1.4.1.34230.3.1.1.3.6.1.19"}).
-define(FitapWireOutOctets,{ifOutOctets, "1.3.6.1.4.1.34230.3.1.1.3.6.1.20"}). 
-define(FitapWireOutDiscards,{ifOutDiscards, "1.3.6.1.4.1.34230.3.1.1.3.6.1.21"}).
-define(FitapWireOutErrors,{ifOutErrors, "1.3.6.1.4.1.34230.3.1.1.3.6.1.22"}).  

%%ap连接信息统计

-define(MonAssoc, [
     {apStationAssocSum,  "1.3.6.1.4.1.34230.3.1.1.3.1.1.28" },
     {apStationOnlineSum, "1.3.6.1.4.1.34230.3.1.1.3.1.1.123"},
     {assocNum,           "1.3.6.1.4.1.34230.3.1.1.3.1.1.51" },
     {assocFailNum,       "1.3.6.1.4.1.34230.3.1.1.3.23.1.30"},
     {reAssocNum,         "1.3.6.1.4.1.34230.3.1.1.3.23.1.11"},
     {assocRefusedNum,    "1.3.6.1.4.1.34230.3.1.1.3.23.1.36"},
     {deauthNum,          "1.3.6.1.4.1.34230.3.1.1.3.23.1.27"}
]).
%%ssid性能统计 
%-define(FitapSsidName,{ssidName, "1.3.6.1.4.1.34230.3.2.1.3.1.6"}).
%-define(FitapSsidOutPkts, {ssidOutPkts, "1.3.6.1.4.1.34230.3.1.1.5.1.2"}).
%-define(FitapSsidInPkts, {ssidInPkts, "1.3.6.1.4.1.34230.3.1.1.5.1.3"}).
%-define(FitapSsidInOctets, {ssidInOctets, "1.3.6.1.4.1.34230.3.1.1.5.1.5"}).
%-define(FitapSsidOutOctets, {ssidOutOctets, "1.3.6.1.4.1.34230.3.1.1.5.1.4"}).

%%sta终端性能统计  --- StaIndex
-define(FitapMac, {apMac, "1.3.6.1.4.1.34230.3.1.1.3.1.1.8"}).

-define(FitapStaMac, {staMac, "1.3.6.1.4.1.34230.3.1.1.3.3.1.6"}).
-define(FitapStaIP, {staIp, "1.3.6.1.4.1.34230.3.1.1.3.3.1.18"}).
-define(FitapStaRssi, {staRssi, "1.3.6.1.4.1.34230.3.1.1.3.3.1.16"}).

-define(FitapStaNoiseRate, {staNoiseRate, "1.3.6.1.4.1.34230.3.1.1.3.3.1.17"}).
-define(FitapStaChannel, {staChannel, "1.3.6.1.4.1.34230.3.1.1.3.3.1.27"}).

-define(FitapStaVlan, {staVlan, "1.3.6.1.4.1.34230.3.1.1.3.3.1.24"}).
-define(FitapStaSsid, {staSsid, "1.3.6.1.4.1.34230.3.1.1.3.3.1.19"}).

-define(FitapStaTxframe, {staTxframe, "1.3.6.1.4.1.34230.3.1.1.3.3.1.28"}).
-define(FitapStaTxbytes, {staTxbytes, "1.3.6.1.4.1.34230.3.1.1.3.3.1.4"}).
-define(FitapStaRxframe, {staRxframe, "1.3.6.1.4.1.34230.3.1.1.3.3.1.29"}).
-define(FitapStaRxbytes, {staRxbytes, "1.3.6.1.4.1.34230.3.1.1.3.3.1.3"}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap discover -- ap配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%系统信息配置
-define(ApMacAddr,{apMac,"1.3.6.1.4.1.34230.3.1.1.3.1.1.8"}). %apMAC地址
-define(ApState, {apState, "1.3.6.1.4.1.34230.3.1.1.3.1.1.9"}).
-define(ApName,{apName,"1.3.6.1.4.1.34230.3.1.1.3.1.1.11"}).
-define(ApSerialNo,{apSerialNo,"1.3.6.1.4.1.34230.3.1.1.3.1.1.12"}).
-define(ApType,{apType,"1.3.6.1.4.1.34230.3.1.1.3.1.1.14"}).
-define(ApSoftVersion,{apSoftVersion,"1.3.6.1.4.1.34230.3.1.1.3.1.1.16"}).


-define(ApIp,{apIp,"1.3.6.1.4.1.34230.3.1.1.3.1.1.3"}).
-define(ApMask,{apMask,"1.3.6.1.4.1.34230.3.1.1.3.1.1.4"}).

%%interface --- APIndex + WirelessIFIndex
-define(WireIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.34230.3.1.1.3.6.1.8"}).

-define(WirelessIfDescr, {ifDescr, "1.3.6.1.4.1.34230.3.1.1.3.6.1.2"}).
-define(WirelessIfType, {ifType, "1.3.6.1.4.1.34230.3.1.1.3.6.1.3"}).
-define(WirelessIfMTU, {ifMtu, "1.3.6.1.4.1.34230.3.1.1.3.6.1.4"}).
-define(WirelessIfSpeed, {ifSpeed, "1.3.6.1.4.1.34230.3.1.1.3.6.1.5"}).
-define(WirelessIfMacAddress, {ifPhysAddress, "1.3.6.1.4.1.34230.3.1.1.3.6.1.6"}).
-define(WirelessIfAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.34230.3.1.1.3.6.1.23"}).
-define(WirelessIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.34230.3.1.1.3.6.1.8"}).

%Radio
-define(FitapRadioChannel, {radioChannel, "1.3.6.1.4.1.34230.3.1.1.3.2.1.4"}).
-define(FitapRadioModel,   {radioModel, "1.3.6.1.4.1.34230.3.1.1.3.2.1.3"   }).
-define(FitapRadioPower,   {radioPower, "1.3.6.1.4.1.34230.3.1.1.3.2.1.24"  }).

-define(FitapRadioPeriod, {radioPeriod, "1.3.6.1.4.1.34230.3.1.1.3.2.1.30"}).
-define(FitapRadioDtim, {radioDtim, "1.3.6.1.4.1.34230.3.1.1.3.2.1.31"}).
-define(FitapRadioRts, {radioRts, "1.3.6.1.4.1.34230.3.1.1.3.2.1.7"}).
-define(FitapRadioSlice, {radioSlice, "1.3.6.1.4.1.34230.3.1.1.3.2.1.6"}).

%%security -- ssidIndex
-define(WlanSecurity, {wlanSecurity, "1.3.6.1.4.1.34230.3.2.1.3.1.5"}).

%%----------------------------------------------------------------------
%% ApSsid
%%----------------------------------------------------------------------
-define(SsidAssoc,{ssidAssoc,"1.3.6.1.4.1.34230.3.2.1.2.1.2"}).
-define(DiscoApSsid,[
    {ssidName,             "1.3.6.1.4.1.34230.3.2.1.3.1.6" },
    {ssidEnabled,          "1.3.6.1.4.1.34230.3.2.1.3.1.17"},
    {ssidHidden,           "1.3.6.1.4.1.34230.3.2.1.3.1.7" },
    {staIsolate,           "1.3.6.1.4.1.34230.3.2.1.3.1.18"},
    {dot11Auth,            "1.3.6.1.4.1.34230.3.2.1.3.1.4" },
    {security,             "1.3.6.1.4.1.34230.3.2.1.3.1.5" },
    {authenMode,           "1.3.6.1.4.1.34230.3.2.1.3.1.19"},
    {securityCiphers,      "1.3.6.1.4.1.34230.3.2.1.3.1.20"},
    {vlanId,               "1.3.6.1.4.1.34230.3.2.1.3.1.8" },
    {maxSimultUsers,       "1.3.6.1.4.1.34230.3.2.1.3.1.11"}
]).











