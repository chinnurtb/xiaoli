%%----------------------------------------------------------------------
%% ac discover -- ac配置(发现)
%%----------------------------------------------------------------------
-define(Ac,[
     {acName,          ""},
     {softVersion,     ""},
     {portalServerURL, ""}
]).

-define(AcIf,[
     {ifDescr,       ""},
     {ifType,        ""},
     {ifMtu,         ""},
     {ifSpeed,       ""},
     {ifPhysAddress, ""},
     {ifAdminStatus, ""},
     {ifOperStatus,  ""}
]).

-define(AcRadio,[
     {radiusAuthServerIPAdd, ""},
     {radiusAuthServerPort,  ""}
]).

-define(AcVlan,[
     {ipPoolName,      ""},
     {ipPoolStartAddr, "" },
     {ipPoolStopAddr,  "" }
]).

-define(Ap,[
     {apName,       "" },
     {apSerialNo,   ""},
     {apType,       "" },
     {apIp,         "" },
     {apMask,       "" },
     {apMac,        "" },
     {apSoftVersion,"" }
]).

-define(DiscoAc,[
    {ac,        ?Ac},
    {ac_radios, ?AcRadio},
    {ac_vlan,   ?AcVlan},
    {ac_intfs,  ?AcIf},
    {aps_mac,   ?Ap}
]).
%%----------------------------------------------------------------------
%% ac monet --ac 性能采集 
%%----------------------------------------------------------------------
-define(Acinfo, [
     {cpuRTUsage,               ""  },
     {memRTUsage,               ""   },
     {dHCPReqTimes,             ""   },
     {dHCPReqSucTimes,          ""   },
     {onlineNum,                ""   },
     {authNum,                  ""  },
     {maxNum,                   ""  },
     {authReqNum,               ""   },
     {authSucNum,               ""  },
     {dHCPIpPoolUsage,          ""   }

     %{bandWidth,               ""},
     %{accReqNum,               ""},
     %{accSucNum,               ""},
     %{radiusReqPkts,           ""},
     %{radiusRepPkts,           ""},
     %{leaveReqPkts,            ""},
     %{leaveRepPkts,            ""},
     %{leaveReqCount,           ""},
     %{leaveRepCount,           ""},

     %{normalNum,               "" }, %多值
     %{deauthNum,               "" }, %多值
     %{radiusAvgDelay,          ""},
     %{portalChallengeReqCount, ""   },
     %{portalChallengeRespCount,""   },
     %{portalAuthReqCount,      ""   },
     %{portalAuthRespCount,     ""   },
  ]).

-define(Acintf, [
     {ifInUcastPkts,  "" },
     {ifInNUcastPkts,       "" },
     {ifInOctets,     "" },   
     {ifInDiscards,   "" }, 
     {ifInErrors,     "" },   
     {ifOutUcastPkts, "" },
     {ifOutNUcastPkts,      "" },
     {ifOutOctets,    "" },  
     {ifOutDiscards,  "" },
     {ifOutErrors,    ""}, 
     {ifUpDwnTimes,   ""}
  ]).

-define(MonAcOid,[
     {mon_acinfo,?Acinfo},
     {mon_acintf,?Acintf}
]).

%%----------------------------------------------------------------------
%% ap discover -- ap配置(发现)
%%----------------------------------------------------------------------
%%radio
-define(ApRadio,[
     {radioPeriod,  "" },
     {radioDtim,    "" },
     {radioRts,     "" },
     {radioSlice,   "" }
     %{radioModel,  ""},
     %{radioChannel,"" }
     %{radioPower,  ""},
  ]).

%%interface 
-define(WireIfOperStatus, {ifOperStatus, ""}).

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
    {apState, ""}
]).

%%----------------------------------------------------------------------
%% ap monet --ap 性能采集 
%%----------------------------------------------------------------------
-define(MonWireless, [
     {ifInOctets,    ""},
     {ifOutPkts,     "" },
     {ifInPkts,      "" },
     {ifOutOctets,   ""},
     {ifInAvgSignal, "" },
     {ifInHighSignal,"" },
     {ifInLowSignal, "" }

     %{ifInErrors,      ""}
     %{ifFrameRetryRate,""}
]).

-define(MonWire, [
    {ifInUcastPkts,  "" },
    {ifInNUcastPkts,       "" },
    {ifInOctets,     ""},
    {ifInDiscards,   "" },
    {ifInErrors,     "" },
    {ifOutUcastPkts, "" },
    {ifOutNUcastPkts,      "" },
    {ifOutOctets,    ""},
    {ifOutDiscards,  "" },
    {ifOutErrors,    ""}  
  ]).

%%ap连接信息统计
-define(MonAssoc, [
    {assocNum,           "" },
    {assocFailNum,       "" },
    {reAssocNum,         "" },
    {deauthNum,          ""},
    {apStationAssocSum,  "" },
    {apStationOnlineSum, "" },
    {cpuRTUsage,         "" },
    {memRTUsage,         "" }

    %{assocRefusedNum,   ""},
    %{reAssocFailNum,    ""},
  ]).

-define(ApSta,[
    {apId,         "" },
    {staMac,       "" },      
    {staIp,        "" },       
    {staRssi,      ""},    
    {staNoiseRate, "" },
    {staChannel,   "" },  
    {staVlan,      "" },     
    {staSsid,      "" }

    %{staTxPkts,   "" },   
    %{staTxbytes,  "" },  
    %{staRxPkts,   "" },   
    %{staRxbytes,  "" } 
  ]).
