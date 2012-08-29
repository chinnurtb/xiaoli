%%-------------------------- ruckus auteX5612  -----------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ac discover -- ac配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(AcName, {acName, "1.3.6.1.4.1.31656.6.1.2.1.2.1.0"}).
-define(MaxApLimit, {maxApLimit, "1.3.6.1.4.1.31656.6.1.2.3.2.1.0"}).
-define(SysModel, {sysModel, "1.3.6.1.4.1.31656.6.1.2.1.2.4.0"}).
-define(SoftwareVersion, {softVersion, "1.3.6.1.4.1.31656.6.1.2.1.1.2.0"}).

-define(PortalServerURL, {portalServerURL, "1.3.6.1.4.1.31656.6.1.2.2.6.0"}).
-define(AsIPAddress, {asIPAddress, "1.3.6.1.4.1.31656.6.1.2.14.2.1.16.0"}).

%%% interface
%-define(AcIfDescr, {ifDescr, "1.3.6.1.2.1.2.2.1.2"}).
%-define(AcIfType, {ifType, "1.3.6.1.2.1.2.2.1.3"}).
%-define(AcIfMTU, {ifMtu, "1.3.6.1.2.1.2.2.1.4"}).
%-define(AcIfSpeed, {ifSpeed, "1.3.6.1.2.1.2.2.1.5"}).
%-define(AcIfMacAddress, {ifPhysAddress, "1.3.6.1.2.1.2.2.1.6"}).
%-define(AcIfAdminStatus, {ifAdminStatus, "1.3.6.1.2.1.2.2.1.7"}).
%-define(AcIfOperStatus, {ifOperStatus, "1.3.6.1.2.1.2.2.1.8"}).

% --- RadiusServerIndex 
-define(RadiusAuthServerIPAdd, {radiusAuthServerIPAdd, "1.3.6.1.4.1.31656.6.1.2.27.1.1.2"}).
-define(RadiusAuthServerPort, {radiusAuthServerPort, "1.3.6.1.4.1.31656.6.1.2.27.1.1.3"}).

% ----  vlaninterface
-define(IPPoolName, {ipPoolName, "1.3.6.1.4.1.31656.6.1.2.6.5.1.10"}).
-define(IPPoolStartAddr, {ipPoolStartAddr, "1.3.6.1.4.1.31656.6.1.2.6.5.1.2"}).
-define(IPPoolStopAddr, {ipPoolStopAddr, "1.3.6.1.4.1.31656.6.1.2.6.5.1.3"}).

%%----------------------------------------------------------------------
%% ac monet --ac 性能采集
%%----------------------------------------------------------------------
-define(Acinfo, [
     {cpuRTUsage,               "1.3.6.1.4.1.31656.6.1.2.1.2.13.0"   },
     {memRTUsage,               "1.3.6.1.4.1.31656.6.1.2.1.2.8.0"    },
     {dHCPReqTimes,             "1.3.6.1.4.1.31656.6.1.2.6.3.2.0"    },
     {dHCPReqSucTimes,          "1.3.6.1.4.1.31656.6.1.2.6.3.3.0"    },
     {onlineNum,                "1.3.6.1.4.1.31656.6.1.2.3.1.3.0"    },
     {authNum,                  "1.3.6.1.4.1.31656.6.1.2.3.1.13.0"   },
     {maxNum,                   "1.3.6.1.4.1.31656.6.1.2.3.1.19.0"   },
     {normalNum,                "1.3.6.1.4.1.31656.6.1.2.13.4.1.6"   },
     {deauthNum,                "1.3.6.1.4.1.31656.6.1.2.3.1.23.0"   },
     {authReqNum,               "1.3.6.1.4.1.31656.6.1.2.3.1.9.0"    },
     {authSucNum,               "1.3.6.1.4.1.31656.6.1.2.3.1.10.0"   },
     {radiusAvgDelay,           "1.3.6.1.4.1.31656.6.1.2.14.2.1.14.0"},
     {dHCPIpPoolUsage,          "1.3.6.1.4.1.31656.6.1.2.6.3.1.0"    }

    %{bandWidth,                ""},
    %{accReqNum,                ""},
    %{accSucNum,                ""},
    %{radiusReqPkts,            ""},
    %{radiusRepPkts,            ""},
    %{leaveReqPkts,             ""},
    %{leaveRepPkts,             ""},
    %{portalChallengeReqCount,  ""},
    %{portalChallengeRespCount, ""},
    %{portalAuthReqCount,       ""},
    %{portalAuthRespCount,      ""},
    %{leaveReqCount,            ""},
    %{leaveRepCount,            ""},
    %{addressCount,             ""},
]).


-define(Acintf, [
     {ifInUcastPkts,  "1.3.6.1.4.1.31656.6.1.2.4.3.1.1" },
     {ifInNUcastPkts, "1.3.6.1.4.1.31656.6.1.2.4.3.1.2" },
     {ifInOctets,     "1.3.6.1.4.1.31656.6.1.2.4.3.1.3" },
     {ifInDiscards,   "1.3.6.1.4.1.31656.6.1.2.4.3.1.4" },
     {ifInErrors,     "1.3.6.1.4.1.31656.6.1.2.4.3.1.5" },
     {ifOutUcastPkts, "1.3.6.1.4.1.31656.6.1.2.4.3.1.6" },
     {ifOutNUcastPkts,"1.3.6.1.4.1.31656.6.1.2.4.3.1.7" },
     {ifOutOctets,    "1.3.6.1.4.1.31656.6.1.2.4.3.1.8" },
     {ifOutDiscards,  "1.3.6.1.4.1.31656.6.1.2.4.3.1.9" },
     {ifOutErrors,    "1.3.6.1.4.1.31656.6.1.2.4.3.1.10"},
     {ifUpDwnTimes,   "1.3.6.1.4.1.31656.6.1.2.4.3.1.11"}
]).

-define(MonAcOid,[
    {mon_acinfo,?Acinfo},
    {mon_acintf,?Acintf}
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ap monet --ap 性能采集% 
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%ap 无线接口性能  
-define(FitapWirelessOutPkts,{ifOutPkts, "1.3.6.1.4.1.31656.6.1.1.3.5.1.8"}).
-define(FitapWirelessInPkts,{ifInPkts, "1.3.6.1.4.1.31656.6.1.1.3.5.1.9"}).
-define(FitapWirelessInOctets,{ifInOctets, "1.3.6.1.4.1.31656.6.1.1.3.5.1.10"}).
-define(FitapWirelessOutOctets,{ifOutOctets, "1.3.6.1.4.1.31656.6.1.1.3.5.1.11"}).
-define(FitapWirelessInErrors,{ifInErrors, "1.3.6.1.4.1.31656.6.1.1.3.5.1.21"}).

-define(FitapWirelessInAvgSignal,{ifInAvgSignal, "1.3.6.1.4.1.31656.6.1.1.3.5.1.1"}).
-define(FitapWirelessInHighSignal,{ifInHighSignal, "1.3.6.1.4.1.31656.6.1.1.3.5.1.2"}).
-define(FitapWirelessInLowSignal,{ifInLowSignal, "1.3.6.1.4.1.31656.6.1.1.3.5.1.3"}).


%%ap 有线接口性能  
-define(FitapWireInUcastPkts, {ifInUcastPkts, "1.3.6.1.4.1.31656.6.1.1.3.2.1.1"}).
-define(FitapWireInNUcastPkts,{ifInNUcastPkts, "1.3.6.1.4.1.31656.6.1.1.3.2.1.2"}).
-define(FitapWireInOctets,{ifInOctets, "1.3.6.1.4.1.31656.6.1.1.3.2.1.12"}).
-define(FitapWireInDiscards,{ifInDiscards, "1.3.6.1.4.1.31656.6.1.1.3.2.1.4"}).
-define(FitapWireInErrors,{ifInErrors, "1.3.6.1.4.1.31656.6.1.1.3.2.1.5"}).

-define(FitapWireOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.4.1.31656.6.1.1.3.2.1.6"}).  
-define(FitapWireOutNUcastPkts,{ifOutNUcastPkts, "1.3.6.1.4.1.31656.6.1.1.3.2.1.7"}).
-define(FitapWireOutOctets,{ifOutOctets, "1.3.6.1.4.1.31656.6.1.1.3.2.1.13"}).
-define(FitapWireOutDiscards,{ifOutDiscards, "1.3.6.1.4.1.31656.6.1.1.3.2.1.9"}).
-define(FitapWireOutErrors,{ifOutErrors, "1.3.6.1.4.1.31656.6.1.1.3.2.1.10"}).

-define(MonAssoc,[
     {assocNum,           "1.3.6.1.4.1.31656.6.1.1.2.4.1.2" },
     {assocFailNum,       "1.3.6.1.4.1.31656.6.1.1.2.4.1.3" },
     {reAssocNum,         "1.3.6.1.4.1.31656.6.1.1.2.4.1.4" },
     {assocRefusedNum,    "1.3.6.1.4.1.31656.6.1.1.2.4.1.10"},
     {deauthNum,          "1.3.6.1.4.1.31656.6.1.1.2.4.1.11"},
     {apStationAssocSum,  "1.3.6.1.4.1.31656.6.1.1.2.4.1.6" },   
     {apStationOnlineSum, "1.3.6.1.4.1.31656.6.1.1.17.1.1.1"},
     {cpuRTUsage,         "1.3.6.1.4.1.31656.6.1.1.1.2.1.3" },
     {memRTUsage,         "1.3.6.1.4.1.31656.6.1.1.1.2.1.9" }
    %{reAssocFailNum,     "" },
]).

-define(ApSta, [
    {staMac,       "1.3.6.1.4.1.31656.6.1.1.8.1.1.1" },
    {staIp,        "1.3.6.1.4.1.31656.6.1.1.8.2.1.2" },
    {staRssi,      "1.3.6.1.4.1.31656.6.1.1.8.1.1.15"},
    {staNoiseRate, "1.3.6.1.4.1.31656.6.1.1.8.1.1.3" },
    {staChannel,   "1.3.6.1.4.1.31656.6.1.1.8.2.1.4" },
    {staVlan,      "1.3.6.1.4.1.31656.6.1.1.8.2.1.7" },
    {staSsid,      "1.3.6.1.4.1.31656.6.1.1.8.2.1.8" },
    {staRxframe,   "1.3.6.1.4.1.31656.6.1.1.8.1.1.6" },
    {staTxframe,   "1.3.6.1.4.1.31656.6.1.1.8.1.1.4" },
    {staRxbytes,   "1.3.6.1.4.1.31656.6.1.1.8.1.1.7" },
    {staTxbytes,   "1.3.6.1.4.1.31656.6.1.1.8.1.1.5" }

    %{apMac,        ""}).
    ]).

%%----------------------------------------------------------------------
%% ap discover -- ap配置(发现)
%%----------------------------------------------------------------------
%系统信息配置
-define(ApMacAddr,{apMac,"1.3.6.1.4.1.31656.6.1.1.1.1.1.1"}). %apMAC地址
-define(ApName,{apName,"1.3.6.1.4.1.31656.6.1.1.1.1.1.2"}).
-define(ApType,{apType,"1.3.6.1.4.1.31656.6.1.1.1.1.1.5"}).
-define(ApSerialNo,{apSerialNo,"1.3.6.1.4.1.31656.6.1.1.1.1.1.11"}).
-define(ApSoftVersion,{apSoftVersion,"1.3.6.1.4.1.31656.6.1.1.1.1.1.6"}).
-define(ApIp,{apIp,"1.3.6.1.4.1.31656.6.1.1.1.5.1.1"}).
-define(ApMask,{apMask,"1.3.6.1.4.1.31656.6.1.1.1.5.1.2"}).

-define(ApRadio,[
     {radioPeriod,   "1.3.6.1.4.1.31656.6.1.1.3.4.1.1" },
     {radioDtim,     "1.3.6.1.4.1.31656.6.1.1.3.4.1.2" },
     {radioRts,      "1.3.6.1.4.1.31656.6.1.1.4.3.1.8" },
     {radioSlice,    "1.3.6.1.4.1.31656.6.1.1.4.3.1.6" },
     {radioModel,    "1.3.6.1.4.1.31656.6.1.1.4.1.1.6" },
     {radioChannel,  "1.3.6.1.4.1.31656.6.1.1.4.1.1.7" },
     {radioPower,    "1.3.6.1.4.1.31656.6.1.1.3.3.1.20"}
]).

-define(ApWirelessIf,[
     {ifDescr,       "1.3.6.1.4.1.31656.6.1.1.3.3.1.2" },
     {ifType,        "1.3.6.1.4.1.31656.6.1.1.3.3.1.3" },
     {ifMtu,         "1.3.6.1.4.1.31656.6.1.1.3.3.1.4" },
     {ifSpeed,       "1.3.6.1.4.1.31656.6.1.1.3.3.1.5" },
     {ifPhysAddress, "1.3.6.1.4.1.31656.6.1.1.3.3.1.6" },
     {ifAdminStatus, "1.3.6.1.4.1.31656.6.1.1.3.3.1.7" },
     {ifOperStatus,  "1.3.6.1.4.1.31656.6.1.1.3.3.1.8" }
]).

-define(WireIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.31656.6.1.1.3.1.1.9"}).
-define(ApState, {apState, "1.3.6.1.4.1.31656.6.1.1.2.5.1.10"}).

%%----------------------------------------------------------------------
%% ApSsid
%%----------------------------------------------------------------------
-define(SsidAssoc,{ssidAssoc,"1.3.6.1.4.1.31656.6.1.2.12.4.1.1"}).
-define(DiscoApSsid,[
    {ssidName,             "1.3.6.1.4.1.31656.6.1.2.13.6.1.1"},
    {ssidEnabled,          "1.3.6.1.4.1.31656.6.1.2.13.6.1.2"},
    {ssidHidden,           "1.3.6.1.4.1.31656.6.1.2.13.6.1.3"},
    {staIsolate,           "1.3.6.1.4.1.31656.6.1.2.13.6.1.4"},
    {dot11Auth,            "1.3.6.1.4.1.31656.6.1.2.13.6.1.5"},
    {security,             "1.3.6.1.4.1.31656.6.1.2.13.6.1.6"},
    {authenMode,           "1.3.6.1.4.1.31656.6.1.2.13.6.1.7"},
    {securityCiphers,      "1.3.6.1.4.1.31656.6.1.2.13.6.1.8"},
    {vlanId,               "1.3.6.1.4.1.31656.6.1.2.13.6.1.18"},
    {maxSimultUsers,       "1.3.6.1.4.1.31656.6.1.2.13.6.1.19"}
]).
-define(MonApSsid,[
     %{ssidName,      "1.3.6.1.4.1.31656.6.1.1.5.4.1.4"},
     {ifOutPkts,   "1.3.6.1.4.1.31656.6.1.1.5.6.1.3"},
     {ifInPkts,    "1.3.6.1.4.1.31656.6.1.1.5.6.1.4"},
     {ifOutOctets, "1.3.6.1.4.1.31656.6.1.1.5.6.1.6"},
     {ifInOctets,  "1.3.6.1.4.1.31656.6.1.1.5.6.1.5"}
]).


