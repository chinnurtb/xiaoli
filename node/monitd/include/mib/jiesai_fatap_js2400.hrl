%jiesai JS2400_InTeleAP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fatap discover -- ap配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%系统信息配置
-define(ApMac,{apMac,"1.3.6.1.4.1.34230.1.1.2"}).
-define(ApName,{apName,"1.3.6.1.4.1.34230.1.1.3"}).
-define(ApType,{apType,"1.3.6.1.4.1.34230.1.1.14"}).
-define(ApSerialNo,{apSerialNo,"1.3.6.1.4.1.34230.1.1.30"}).
-define(ApSoftVersion,{apSoftVersion,"1.3.6.1.4.1.34230.1.1.1"}).
-define(ApIp,{apIp,"1.3.6.1.4.1.34230.1.2.5.2"}).
-define(ApMask,{apMask,"1.3.6.1.4.1.34230.1.2.5.3"}).

%%radio   
-define(RadioPeriod, {radioPeriod, "1.3.6.1.4.1.34230.1.4.3.1.3"}).
-define(RadioDtim, {radioDtim, "1.3.6.1.4.1.34230.1.4.3.1.4"}).
-define(RadioRts, {radioRts, "1.3.6.1.4.1.34230.1.4.1.1.6"}).
-define(RadioSlice, {radioSlice, "1.3.6.1.4.1.34230.1.4.1.1.5"}).

%-define(RadioChannel, {radioChannel, ""}).
-define(RadioModel, {radioModel, "1.3.6.1.4.1.34230.1.4.1.1.2"}).
%-define(RadioPower, {radioPower, "1.3.6.1.4.1.34230.1.4.1.1.64"}).

%%If
%-define(WirelessIfDescr, {ifDescr, "1.3.6.1.4.1.34230.1.17.5.1.3"}).
%-define(WirelessIfType, {ifType, "1.3.6.1.4.1.34230.1.17.5.1.4"}).
%-define(WirelessIfMTU, {ifMtu, "1.3.6.1.4.1.34230.1.17.5.1.5"}).
%-define(WirelessIfSpeed, {ifSpeed, "1.3.6.1.4.1.34230.1.17.5.1.2"}).
%-define(WirelessIfMacAddress, {ifPhysAddress, "1.3.6.1.4.1.34230.1.17.5.1.6"}).
%-define(WirelessIfAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.34230.1.17.5.1.14"}).
%-define(WirelessIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.34230.1.17.5.1.13"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fatap monet --ap 性能采集% 
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%ap连接信息统计
%-define(ApStationAssocSum, {apStationAssocSum, "1.3.6.1.4.1.34230.1.4.1.1.240"}).
%-define(ApStationOnlineSum, {apStationOnlineSum, "1.3.6.1.4.1.34230.1.1.187"}).
-define(AssocNum, {assocNum, "1.3.6.1.4.1.34230.1.5.2.1.18"}).
-define(AssocSuccNum, {assocSuccNum, "1.3.6.1.4.1.34230.1.5.2.1.48"}).
%-define(AssocFailNum, {assocFailNum, ""}).
-define(ReAssocNum, {reAssocNum, "1.3.6.1.4.1.34230.1.5.2.1.19"}).
-define(DeauthNum, {deauthNum, "1.3.6.1.4.1.34230.1.5.2.1.66"}).
-define(AssocRefusedNum, {assocRefusedNum, "1.3.6.1.4.1.34230.1.5.2.1.51"}).

%%ap 有线接口性能  
%-define(WireInUcastPkts, {ifInUcastPkts, ""}).
%-define(WireInNUcastPkts,{ifInNUcastPkts, ""}).
-define(WireInOctets,{ifInOctets, "1.3.6.1.4.1.34230.1.5.1.2"}).
-define(WireInDiscards,{ifInDiscards, "1.3.6.1.4.1.34230.1.5.1.6"}).
-define(WireInErrors,{ifInErrors, "1.3.6.1.4.1.34230.1.5.1.5"}).

%-define(WireOutUcastPkts, {ifOutUcastPkts, ""}).  
%-define(WireOutNUcastPkts,{ifOutNUcastPkts, ""}).
-define(WireOutOctets,{ifOutOctets, "1.3.6.1.4.1.34230.1.5.1.4"}).
-define(WireOutDiscards,{ifOutDiscards, "1.3.6.1.4.1.34230.1.5.1.8"}).
-define(WireOutErrors,{ifOutErrors, "1.3.6.1.4.1.34230.1.5.1.7"}).

%%ap 无线接口性能  
-define(WirelessInAvgSignal,{ifInAvgSignal, "1.3.6.1.4.1.34230.1.5.2.1.99"}).
%-define(WirelessInHighSignal,{ifInHighSignal, "1.3.6.1.4.1.34230.1.5.2.1.100"}).
%-define(WirelessInLowSignal,{ifInLowSignal, "1.3.6.1.4.1.34230.1.5.2.1.101"}).
-define(WirelessFrameRetryRate,{ifFrameRetryRate, "1.3.6.1.4.1.34230.1.5.2.1.39"}).

-define(WirelessOutPkts,{ifOutPkts, "1.3.6.1.4.1.34230.1.5.2.1.9"}).
-define(WirelessInPkts,{ifInPkts, "1.3.6.1.4.1.34230.1.5.2.1.4"}).
-define(WirelessInOctets,{ifInOctets, "1.3.6.1.4.1.34230.1.5.2.1.5"}).
-define(WirelessOutOctets,{ifOutOctets, "1.3.6.1.4.1.34230.1.5.2.1.10"}).
-define(WirelessInErrors,{ifInErrors, "1.3.6.1.4.1.34230.1.5.2.1.11"}).

%%ssid性能统计 
%-define(SsidName,{ssidName, ""}).
%-define(SsidInOctets, {ssidInOctets, ""}).
%-define(SsidInPkts, {ssidInPkts, ""}).
%-define(SsidOutOctets, {ssidOutOctets, ""}).
%-define(SsidOutPkts, {ssidOutPkts, ""}).

%%sta终端性能统计  --- StaIndex
-define(StaMac, {staMac, "1.3.6.1.4.1.34230.1.5.4.1.2"}).
-define(StaRssi, {staRssi, "1.3.6.1.4.1.34230.1.5.4.1.3"}).
%-define(StaNoiseRate, {staNoiseRate, ""}).
%-define(StaIp, {staIp, ""}).
%-define(StaChannel, {staChannel, ""}).
%-define(StaVlan, {staVlan, ""}).
%-define(StaSsid, {staSsid, ""}).
-define(StaTxframe, {staTxframe, "1.3.6.1.4.1.34230.1.5.4.1.6"}).
-define(StaTxbytes, {staTxbytes, "1.3.6.1.4.1.34230.1.5.4.1.8"}).
-define(StaRxframe, {staRxframe, "1.3.6.1.4.1.34230.1.5.4.1.5"}).
-define(StaRxbytes, {staRxbytes, "1.3.6.1.4.1.34230.1.5.4.1.7"}).

