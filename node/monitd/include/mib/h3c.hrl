%%huawei wa1208
-define(H3cWirelessStaUserStats, {h3cWirelessStaUserStats, [1,3,6,1,4,1,2720,1,1,2,36,0]}).
-define(H3cWirelessStaEntry, [?H3cWirelessStaIndex, ?H3cWirelessStaMac]).
-define(H3cWirelessStaIndex, {h3cWirelessStaIndex, [1,3,6,1,4,1,2720,1,1,2,9,1,1]}).
-define(H3cWirelessStaMac, {h3cWirelessStaMac, [1,3,6,1,4,1,2720,1,1,2,9,1,2]}).

%h3c wa1208v5
-define(H3cDot11ApStationCurAssocSum, {h3cDot11ApStationCurAssocSum, [1,3,6,1,4,1,2011,10,2,75,2,2,3,1,6]}).
-define(H3cDot11StationAssociateTable, {h3cDot11StationAssociateTable, [1,3,6,1,4,1,2011,10,2,75,3,1,1]}).
-define(H3cDot11StationMAC, {h3cDot11StationMAC, [1,3,6,1,4,1,2011,10,2,75,3,1,1,1,1]}).
-define(H3cDot11StationSSIDName, {h3cDot11StationSSIDName, [1,3,6,1,4,1,2011,10,2,75,3,1,1,1,12]}).
-define(H3cDot11CurrAPID, {h3cDot11CurrAPID, [1,3,6,1,4,1,2011,10,2,75,3,1,2,1,1]}).

-define(H3cDot11SSIDName, {ssid, [1,3,6,1,4,1,2011,10,2,75,4,2,2,1,2,1]}).

-define(H3cDot11RadioCfgDtimIntvl, {h3cDot11RadioCfgDtimIntvl, [1,3,6,1,4,1,2011,10,2,75,4,4,1,1,3]}). %DTIM时间间隔
-define(H3cDot11RadioCfgRtsThreshold, {h3cDot11RadioCfgRtsThreshold, [1,3,6,1,4,1,2011,10,2,75,4,4,1,1,4]}). %RTS阈值
-define(H3cDot11RadioCfgFragThreshold, {h3cDot11RadioCfgFragThreshold, [1,3,6,1,4,1,2011,10,2,75,4,4,1,1,5]}). %分片阈值
-define(H3cDot11RadioCfgType, {h3cDot11RadioCfgType, [1,3,6,1,4,1,2011,10,2,75,4,4,1,1,9]}).   %射频模式
-define(H3cDot11RadioCfgChannel, {h3cDot11RadioCfgChannel, [1,3,6,1,4,1,2011,10,2,75,4,4,1,1,10]}). %信道

-define(H3cDot11RadioCfgEntry, [?H3cDot11RadioCfgDtimIntvl, ?H3cDot11RadioCfgRtsThreshold, ?H3cDot11RadioCfgFragThreshold, ?H3cDot11RadioCfgType, ?H3cDot11RadioCfgChannel]).

 %h3c old wirless traffic
-define(OHh3cDot11RxFrameCnt, {ifInPkts, [1,3,6,1,4,1,2011,10,2,75,2,2,1,1,2]}). %        无线侧接收帧数  即指标中无线侧接收包数
-define(OHh3cDot11RxUcastFrameCnt, {ifInUcastPkts, [1,3,6,1,4,1,2011,10,2,75,2,2,1,1,3]}).    %无线侧接收单播帧数  即指标中无线侧接收单播包数
-define(OHh3cDot11RxBcastFrameCnt, {hh3cDot11RxBcastFrameCnt, [1,3,6,1,4,1,2011,10,2,75,2,2,1,1,4]}).    %无线侧接收多播帧数
-define(OHh3cDot11RxMcastFrameCnt, {hh3cDot11RxMcastFrameCnt, [1,3,6,1,4,1,2011,10,2,75,2,2,1,1,5]}).    %无线侧接收广播帧数
-define(OHh3cDot11RxDiscardFrameCnt, {ifInDiscards, [1,3,6,1,4,1,2011,10,2,75,2,2,1,1,6]}).    %无线侧接收丢弃帧数  即指标中的无线侧接收丢包数
-define(OHh3cDot11RxFcsErrCnt, {ifInErrors, [1,3,6,1,4,1,2011,10,2,75,2,2,1,1,8]}).   %无线侧接收FCS误帧数 即指标中的无线侧接收误包数
-define(OHh3cDot11RxFrameBytes, {ifInOctets, [1,3,6,1,4,1,2011,10,2,75,2,2,1,1,9]}).    %无线侧接收帧字节数  即指标中的无线侧接收字节数

-define(OHh3cDot11AckFailCnt, {ifOutErrors, [1,3,6,1,4,1,2011,10,2,75,2,2,2,1,7]}). %    无线侧发送应答错误帧数  即指标中的无线侧发送误包数
-define(OHh3cDot11TxFrameCnt, {ifOutPkts, [1,3,6,1,4,1,2011,10,2,75,2,2,2,1,8]}). %    无线侧发送帧数  即指标中的无线侧发送包数
-define(OHh3cDot11TxUcastFrameCnt, {ifOutUcastPkts, [1,3,6,1,4,1,2011,10,2,75,2,2,2,1,9]}). %    无线侧发送单播帧数  即指标中的无线侧发送单播包数
-define(OHh3cDot11TxBcastFrameCnt, {hh3cDot11TxBcastFrameCnt, [1,3,6,1,4,1,2011,10,2,75,2,2,2,1,10]}). %   无线侧发送多播帧数
-define(OHh3cDot11TxMcastFrameCnt, {hh3cDot11TxMcastFrameCnt, [1,3,6,1,4,1,2011,10,2,75,2,2,2,1,11]}). %   无线侧发送广播帧数
-define(OHh3cDot11TxDiscardFrameCnt, {ifOutDiscards, [1,3,6,1,4,1,2011,10,2,75,2,2,2,1,12]}). %   无线侧发送丢弃帧数  即指标中的无线侧发送丢包数
-define(OHh3cDot11TxFrameBytes, {ifOutOctets, [1,3,6,1,4,1,2011,10,2,75,2,2,2,1,13]}). %   无线侧发送帧字节数  即指标中的无线侧发送字节数

 %h3c ac wired traffic
 -define(Hh3cDot11APIfDescr,{hh3cDot11APIfDescr,"1.3.6.1.4.1.2011.10.2.75.2.1.6.1.2"}).
-define(MonWire, [
     {ifInUcastPkts,  "1.3.6.1.4.1.2011.10.2.75.2.2.17.1.9" },
     {ifInNUcastPkts, "1.3.6.1.4.1.2011.10.2.75.2.2.17.1.10"},
     {ifInOctets,     "1.3.6.1.4.1.2011.10.2.75.2.2.17.1.5" },
     {ifInDiscards,   "1.3.6.1.4.1.2011.10.2.75.2.2.17.1.11"},
     {ifInErrors,     "1.3.6.1.4.1.2011.10.2.75.2.2.17.1.3" },
     {ifOutUcastPkts, "1.3.6.1.4.1.2011.10.2.75.2.2.17.1.12"},
     {ifOutNUcastPkts,"1.3.6.1.4.1.2011.10.2.75.2.2.17.1.13"},
     {ifOutOctets,    "1.3.6.1.4.1.2011.10.2.75.2.2.17.1.6" },
     {ifOutDiscards,  "1.3.6.1.4.1.2011.10.2.75.2.2.17.1.14"},
     {ifOutErrors,    "1.3.6.1.4.1.2011.10.2.75.2.2.17.1.15"}
]).

-define(MonWireless, [
     {ifInOctets,    "1.3.6.1.4.1.2011.10.2.75.2.2.1.1.9" },
     {ifOutOctets,   "1.3.6.1.4.1.2011.10.2.75.2.2.2.1.13"},
     {ifInPkts,      "1.3.6.1.4.1.2011.10.2.75.2.2.2.1.8" },
     {ifOutPkts,     "1.3.6.1.4.1.2011.10.2.75.2.2.1.1.2" },
     {ifInAvgSignal, "1.3.6.1.4.1.2011.10.2.75.7.1.1.1.4" },
     {ifInHighSignal,"1.3.6.1.4.1.2011.10.2.75.7.1.1.1.5" },
     {ifInLowSignal, "1.3.6.1.4.1.2011.10.2.75.7.1.1.1.6" }

     %{ifInErrors,      ""}
     %{ifFrameRetryRate,""}
]).

-define(MonAssoc, [
    {assocNum,           "1.3.6.1.4.1.2011.10.2.75.2.2.3.1.1" },
    {assocFailNum,       "1.3.6.1.4.1.2011.10.2.75.2.2.3.1.2" },
    {reAssocNum,         "1.3.6.1.4.1.2011.10.2.75.2.2.3.1.3" },
    {reAssocFailNum,     "1.3.6.1.4.1.2011.10.2.75.2.2.22.1.4"},
    {deauthNum,          "1.3.6.1.4.1.2011.10.2.75.2.2.3.1.5" },
    {assocRefusedNum,    "1.3.6.1.4.1.2011.10.2.75.2.2.3.1.4" },
    {apStationAssocSum,  "1.3.6.1.4.1.2011.10.2.75.2.2.3.1.6" },
    {apStationOnlineSum, "1.3.6.1.4.1.2011.10.2.75.2.2.3.1.7" },
    {cpuRTUsage,         "1.3.6.1.4.1.2011.10.2.75.2.1.8.1.2" },
    {memRTUsage,         "1.3.6.1.4.1.2011.10.2.75.2.1.8.1.4" }
]).

%soft version
-define(hh3cLswSlotSoftwareVersion, {softVersion, "1.3.6.1.4.1.2011.10.8.35.18.4.3.1.6"}).
%cpu usage
-define(H3cEntityExtCpuUsage, {h3cEntityExtCpuUsage, [1,3,6,1,4,1,2011,10,2,6,1,1,1,1,6]}).
%memeory usage
-define(H3cEntityExtMemUsage, {h3cEntityExtMemUsage, [1,3,6,1,4,1,2011,10,2,6,1,1,1,1,8]}).

%总内存。
-define(HwMemTotal, {hwMemTotal, [1,3,6,1,4,1,2011,6,1,2,1,1,2]}).
%总空闲内存
-define(HwMemFree, {hwMemFree, [1,3,6,1,4,1,2011,6,1,2,1,1,3]}).

%%H3C AC wx6100LSQ MIB
-define(H3cDot11APIPAddress, {ipAddr, "1.3.6.1.4.1.2011.10.2.75.2.1.1.1.2"}).%  AP的IP地址
-define(H3cDot11APMacAddress, {macAddr, "1.3.6.1.4.1.2011.10.2.75.2.1.1.1.3"}).  %AP的MAC地址
-define(H3cDot11APOperationStatus, {operationStatus,"1.3.6.1.4.1.2011.10.2.75.2.1.1.1.4"}).  %AP运行状态
-define(H3cDot11APTemplateNameOfAP, {templateName, "1.3.6.1.4.1.2011.10.2.75.2.1.1.1.5"}). %AP Cn
-define(H3cDot11APReset, {reset, "1.3.6.1.4.1.2011.10.2.75.2.1.1.1.6"}).  %AP重启
-define(H3cDot11APCpuUsage, {cpuUsage, "1.3.6.1.4.1.2011.10.2.75.2.1.1.1.7"}). %AP的CPU利用率

-define(H3cDot11CurrAPIPAddress, {currIpAddr, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.2"}). % AP的IP地址
-define(H3cDot11CurrAPMacAddress, {currMacAddr, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.3"}). % AP的MAC地址
-define(H3cDot11CurrAPStationAssocCount, {currAssocCount, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.7"}). % AP当前连接用户数
-define(H3cDot11CurrAPModelName, {currModelName, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.9"}). % AP型号
-define(H3cDot11CurrAPSoftwareVersion, {currSoftwareVersion, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.11"}). %AP软件版本
-define(H3cDot11CurrAPIPNetMask, {currNetMask, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.12"}). %%AP子网掩码
-define(H3cDot11CurrRadioModeSupport, {currRadioModeSupport, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.13"}). %射频模式

-define(H3cDot11CurrApEntry, [?H3cDot11CurrAPIPAddress, ?H3cDot11APTemplateNameOfAP, ?H3cDot11CurrAPMacAddress, ?H3cDot11CurrAPIPNetMask, ?H3cDot11CurrAPModelName]).

%%H3C AC hh3c-wx6100EWPX MIB
-define(NH3cDot11APOperationStatus, {operationStatus,"1.3.6.1.4.1.2011.10.2.75.2.1.1.1.4"}).  %AP运行状态

-define(NH3cDot11APConnectionType, {connectionType,"1.3.6.1.4.1.2011.10.2.75.2.1.1.1.8"}). %AP master or slave by hejin 10.14
-define(NH3cDot11CurrAPIPAddress, {currIpAddr, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.2"}). % AP的IP地址
-define(NH3cDot11CurrAPTemplateName, {currTemplateName, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.6"}). % AP的名称
-define(NH3cDot11CurrAPSoftwareVersion, {currSoftwareVersion, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.11"}). %AP软件版本
-define(NH3cDot11CurrAPMacAddress, {currMacAddr, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.3"}). % AP的MAC地址
-define(NH3cDot11CurrAPStationAssocCount, {currAssocCount, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.7"}). % AP当前连接用户数
-define(NH3cDot11CurrAPModelName, {currModelName, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.9"}). % AP型号
-define(NH3cDot11CurrAPIPNetMask, {currNetMask, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.12"}). %%AP子网掩码
-define(NH3cDot11CurrRadioModeSupport, {currRadioModeSupport, "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.13"}). %射频模式

-define(Ap,[
     %{connectionType, "1.3.6.1.4.1.2011.10.2.75.2.1.1.1.8" }, %AP master or slave 
     {apName,       "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.6" },
     {apType,       "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.9" },
     {apIp,         "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.2" },
     {apMask,       "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.12"},
     {apMac,        "1.3.6.1.4.1.2011.10.2.75.2.1.2.1.3" },
     {apSoftVersion,"1.3.6.1.4.1.2011.10.2.75.2.1.2.1.11"}
]).

-define(SerialID,{serialId, "1.3.6.1.4.1.2011.10.2.75.12.1.1.1.3"}).


-define(NHh3cDot11CurrAPID, {currAPID, "1.3.6.1.4.1.2011.10.2.75.3.1.2.1.1"}). %当前apid
-define(NHh3cDot11CurrRadioID, {currRadioID, "1.3.6.1.4.1.2011.10.2.75.3.1.2.1.2"}). %当前radioid

-define(RadioPeriod, {radioPeriod,  "1.3.6.1.4.1.2011.10.2.75.4.4.1.1.2"}).
-define(RadioDtim,   {radioDtim,    "1.3.6.1.4.1.2011.10.2.75.4.4.1.1.3"}).
-define(RadioRts,    {radioRts,     "1.3.6.1.4.1.2011.10.2.75.4.4.1.1.4"}).
-define(RadioSlice,  {radioSlice,   "1.3.6.1.4.1.2011.10.2.75.4.4.1.1.5"}).
-define(RadioModel,  {radioModel,   "1.3.6.1.4.1.2011.10.2.75.4.4.1.1.9"}).
-define(RadioChannel,{radioChannel, "1.3.6.1.4.1.2011.10.2.75.4.4.1.1.10"}).
-define(RadioPower,  {radioPower,   "1.3.6.1.4.1.2011.10.2.75.4.4.1.1.11"}).

%%fit_ap radio
-define(FitRadioAdminStatus, {radioAdminStatus, "1.3.6.1.4.1.2011.10.2.75.2.1.3.1.3"}).
-define(FitRadioOperStatus, {radioOperStatus, "1.3.6.1.4.1.2011.10.2.75.2.1.3.1.4"}).
-define(FitRadioIndex, {radioIndex, "1.3.6.1.4.1.2011.10.2.75.2.1.3.1.7"}).
-define(FitRadioPlus, {radioPlus, "1.3.6.1.4.1.2011.10.2.75.2.1.3.1.8"}).
-define(FitRadioChannel, {radioChannel, "1.3.6.1.4.1.2011.10.2.75.2.1.3.1.5"}).
-define(FitRadioPower, {radioPower, "1.3.6.1.4.1.2011.10.2.75.2.1.3.1.6"}).
-define(FitRadioModel, {radioModel, "1.3.6.1.4.1.2011.10..2.75.2.1.2.1.13"}).

-define(H3cDot11BeaconInterval , {radioPeriod, "1.3.6.1.4.1.2011.10.2.75.4.2.1.1.2"}).
-define(H3cDot11DtimInterval,    {radioDtim, "1.3.6.1.4.1.2011.10.2.75.4.2.1.1.3"}).
-define(H3cDot11RtsThreshold ,   {radioRts, "1.3.6.1.4.1.2011.10.2.75.4.2.1.1.4"}).
-define(H3cDot11FragThreshold ,  {radioSlice, "1.3.6.1.4.1.2011.10.2.75.4.2.1.1.5"}).

-define(H3cDot11CfgRdElementID, {radioEid, "1.3.6.1.4.1.2011.10.2.75.4.3.2.1.9"}).   %h3cDot11CfgRdElementID
-define(H3cDot11CfgRadioPolicyName, {radioPname, "1.3.6.1.4.1.2011.10.2.75.4.3.2.1.3"}).   %h3cDot11CfgRadioPolicyName
-define(Hh3cDot11CfgRadioStatus, {radioStatus, "1.3.6.1.4.1.2011.10.2.75.4.3.2.1.8"}).   %hh3cDot11CfgRadioStatus

%%sta
-define(StaIP, {staIp, "1.3.6.1.4.1.2011.10.2.75.3.1.1.1.2"}).
-define(StaRssi, {staRssi, "1.3.6.1.4.1.2011.10.2.75.3.1.1.1.6"}).
-define(StaNoiseRate, {staNoiseRate, "1.3.6.1.4.1.2011.10.2.75.3.1.1.1.7"}).
-define(StaChannel, {staChannel, "1.3.6.1.4.1.2011.10.2.75.3.1.1.1.8"}).
-define(StaVlan, {staVlan, "1.3.6.1.4.1.2011.10.2.75.3.1.1.1.11"}).
-define(StaSsid, {staSsid, "1.3.6.1.4.1.2011.10.2.75.3.1.1.1.12"}).
-define(StaRxframe, {staRxframe, "1.3.6.1.4.1.2011.10.2.75.3.1.3.1.1"}).
-define(StaTxframe, {staTxframe, "1.3.6.1.4.1.2011.10.2.75.3.1.3.1.2"}).
-define(StaDropframe, {staDropframe, "1.3.6.1.4.1.2011.10.2.75.3.1.3.1.3"}).
-define(StaRxbytes, {staRxbytes, "1.3.6.1.4.1.2011.10.2.75.3.1.3.1.4"}).
-define(StaTxbytes, {staTxbytes, "1.3.6.1.4.1.2011.10.2.75.3.1.3.1.5"}).
-define(StaDropbytes, {staDropbytes, "1.3.6.1.4.1.2011.10.2.75.3.1.3.1.6"}).

%%ssid
-define(SsidName,      {ssidName, "1.3.6.1.4.1.2011.10.2.75.4.2.2.1.2"}).

-define(PolicyId,      {policyId, "1.3.6.1.4.1.2011.10.2.75.2.1.4.1.3"}). %hh3cDot11CurrSvcPolicyID

-define(SsidInOctets,  {ssidInOctets, "1.3.6.1.4.1.2011.10.2.75.2.2.4.1.2"}).
-define(SsidInPkts,    {ssidInPkts, "1.3.6.1.4.1.2011.10.2.75.2.2.4.1.1"}).
-define(SsidOutOctets, {ssidOutOctets, "1.3.6.1.4.1.2011.10.2.75.2.2.5.1.2"}).
-define(SsidOutPkts,   {ssidOutPkts, "1.3.6.1.4.1.2011.10.2.75.2.2.5.1.1"}).

%%fitap_ssid
-define(FitapSsidName, {ssidName, "1.3.6.1.4.1.2011.10.2.75.3.1.1.1.12"}).

%%fitap_reset
-define(FitapReset, {reset, "1.3.6.1.4.1.2011.10.2.75.2.1.1.1.6"}).

%%% ac
-define(AsIPAddress, {asIPAddress, "1.3.6.1.4.1.2011.10.2.12.1.2.1.4"}).


-define(AcRadio,[
     {radiusAuthServerIPAdd, "1.3.6.1.4.1.2011.10.2.13.5.2.1.1.2"},
     {radiusAuthServerPort,  "1.3.6.1.4.1.2011.10.2.13.5.2.1.1.3"}
]).

-define(AcVlan,[
     {ipPoolName,      "1.3.6.1.4.1.2011.10.2.101.2.2.1.1"},
     {ipPoolStartAddr, "1.3.6.1.4.1.2011.10.2.101.2.3.1.8"},
     {ipPoolStopAddr,  "1.3.6.1.4.1.2011.10.2.101.2.3.1.9"}
]).
-define(PoolOids,[
    {ipPoolName, "1.3.6.1.4.1.2011.10.2.101.2.2.1.1"},
    {ipPoolUsage,"1.3.6.1.4.1.2011.10.2.101.2.6.1.1"}
]).

-define(ApState,{apState,"1.3.6.1.4.1.2011.10.2.75.2.1.1.1.4"}).
%%----------------------------------------------------------------------
%% ac monet --ac 性能采集 
%%----------------------------------------------------------------------
-define(Acinfo, [
     {cpuRTUsage,               "1.3.6.1.4.1.2011.10.2.6.1.1.1.1.6.97"},
     {memRTUsage,               "1.3.6.1.4.1.2011.10.2.6.1.1.1.1.8.97"},
     {dHCPIpPoolUsage,          "1.3.6.1.4.1.2011.10.2.101.1.1.0" },
     {dHCPReqTimes,             "1.3.6.1.4.1.2011.10.2.101.1.2.0" },
     {dHCPReqSucTimes,          "1.3.6.1.4.1.2011.10.2.101.1.3.0" },
     {onlineNum,                "1.3.6.1.4.1.2011.10.2.75.1.1.2.2.0"},
     {normalNum,                "1.3.6.1.4.1.2011.10.2.75.1.1.5.2.0"},
     {deauthNum,                "1.3.6.1.4.1.2011.10.2.75.1.1.3.5.0"},
     {authReqNum,               "1.3.6.1.4.1.2011.10.2.75.1.1.5.3.0"},
     {authSucNum,               "1.3.6.1.4.1.2011.10.2.75.1.1.5.4.0"},
     {flashMemTotal,            "1.3.6.1.4.1.2011.10.2.5.1.1.4.1.1.4.1.1"},
     {flashMemFree,             "1.3.6.1.4.1.2011.10.2.5.1.1.4.1.1.5.1.1"}

     %{bandWidth,               ""},
     %{accReqNum,               ""},
     %{accSucNum,               ""},
     %{radiusReqPkts,           ""},
     %{radiusRepPkts,           ""},
     %{leaveReqPkts,            ""},
     %{leaveRepPkts,            ""},
     %{leaveReqCount,           ""},
     %{leaveRepCount,           ""},
     %{authNum,                 ""},
     %{maxNum,                  ""},
     %{radiusAvgDelay,          ""},
     %{portalChallengeReqCount, ""},
     %{portalChallengeRespCount,""},
     %{portalAuthReqCount,      ""},
     %{portalAuthRespCount,     ""},
]).

%-define(Acintf, [
%     {ifInUcastPkts,  "1.3.6.1.4.1.31656.6.1.2.4.3.1.1" },
%     {ifInNUcastPkts, "1.3.6.1.4.1.31656.6.1.2.4.3.1.2" },
%     {ifInOctets,     "1.3.6.1.4.1.31656.6.1.2.4.3.1.3" },   
%     {ifInDiscards,   "1.3.6.1.4.1.31656.6.1.2.4.3.1.4" }, 
%     {ifInErrors,     "1.3.6.1.4.1.31656.6.1.2.4.3.1.5" },   
%     {ifOutUcastPkts, "1.3.6.1.4.1.31656.6.1.2.4.3.1.6" },
%     {ifOutNUcastPkts,"1.3.6.1.4.1.31656.6.1.2.4.3.1.7" },
%     {ifOutOctets,    "1.3.6.1.4.1.31656.6.1.2.4.3.1.8" },  
%     {ifOutDiscards,  "1.3.6.1.4.1.31656.6.1.2.4.3.1.9" },
%     {ifOutErrors,    "1.3.6.1.4.1.31656.6.1.2.4.3.1.10"}, 
%     {ifUpDwnTimes,   "1.3.6.1.4.1.31656.6.1.2.4.3.1.11"}
%  ]).

%%----------------------------------------------------------------------
%% ac discover -- ac配置(发现)
%%----------------------------------------------------------------------
-define(Ac,[
    {acName,          "1.3.6.1.2.1.1.5.0"                 },
    {softVersion,     "1.3.6.1.2.1.47.1.1.1.1.10.1"       },
    {maxApLimit,      "1.3.6.1.4.1.2011.10.2.75.1.1.1.2.0"}
]).


%%----------------------------------------------------------------------
%% ApSsid
%%----------------------------------------------------------------------
-define(SsidAssoc,{ssidAssoc,"1.3.6.1.4.1.2011.10.2.75.2.1.4.1.3"}).
-define(DiscoApSsid,[
    {ssidName,             "1.3.6.1.4.1.2011.10.2.75.4.2.2.1.2" },
    {ssidEnabled,          "1.3.6.1.4.1.2011.10.2.75.4.2.2.1.8" },
    {ssidHidden,           "1.3.6.1.4.1.2011.10.2.75.4.2.2.1.3" },
    {staIsolate,           "1.3.6.1.4.1.2011.10.2.75.4.2.2.1.17"},
    {dot11Auth,            "1.3.6.1.4.1.2011.10.2.75.4.2.2.1.4" },
    %{security,             "1.3.6.1.4.1.2011.10.2.75.4.2.3.1.2"},
    {authenMode,           "1.3.6.1.4.1.2011.10.2.75.4.2.2.1.4" },
    %{securityCiphers,      "1.3.6.1.4.1.2011.10.2.75.4.2.3.1.3"},
    {vlanId,               "1.3.6.1.4.1.2011.10.2.75.4.2.2.1.14"},
    {maxSimultUsers,       "1.3.6.1.4.1.2011.10.2.75.4.2.2.1.9" }
]).

-define(MonApSsid,[
     {ifOutPkts,   "1.3.6.1.4.1.2011.10.2.75.2.2.5.1.3"},
     {ifInPkts,    "1.3.6.1.4.1.2011.10.2.75.2.2.4.1.3"},
     {ifOutOctets, "1.3.6.1.4.1.2011.10.2.75.2.2.4.1.4"},
     {ifInOctets,  "1.3.6.1.4.1.2011.10.2.75.2.2.5.1.4"}
]).

-define(StaLogin,[
    {loginNum,     "1.3.6.1.4.1.2011.10.2.75.2.2.25.1.3"}
  ]).

