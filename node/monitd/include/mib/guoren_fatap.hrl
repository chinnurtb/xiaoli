%guoren SGR-W500-EBI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fatap discover -- ap配置(发现)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%系统信息配置
-define(ApMac,{apMac,"1.3.6.1.4.1.18603.50.1.14"}). %apMAC地址
-define(ApName,{apName,"1.3.6.1.4.1.18603.50.1.2"}).
-define(ApType,{apType,"1.3.6.1.4.1.18603.50.1.6"}).
%-define(ApSerialNo,{apSerialNo,""}).
-define(ApSoftVersion,{apSoftVersion,"1.3.6.1.4.1.18603.50.1.22"}).
-define(ApIp,{apIp,"1.3.6.1.4.1.18603.50.1.9"}).
-define(ApMask,{apMask,"1.3.6.1.4.1.18603.50.1.10"}).

%%radio   
-define(RadioPeriod, {radioPeriod, "1.3.6.1.4.1.18603.50.2.4.1.26"}).
-define(RadioDtim, {radioDtim, "1.3.6.1.4.1.18603.50.2.4.1.27"}).
-define(RadioRts, {radioRts, "1.3.6.1.4.1.18603.50.2.4.1.28"}).
-define(RadioSlice, {radioSlice, "1.3.6.1.4.1.18603.50.2.4.1.29"}).

-define(RadioChannel, {radioChannel, "1.3.6.1.4.1.18603.50.2.4.1.12"}).
-define(RadioModel, {radioModel, "1.3.6.1.4.1.18603.50.2.4.1.14"}).
-define(RadioPower, {radioPower, "1.3.6.1.4.1.18603.50.2.4.1.16"}).

%%WirelessIF
%-define(WirelessIfDescr, {ifDescr, "1.3.6.1.4.1.33940.5000.17.5.1.3"}).
%-define(WirelessIfType, {ifType, "1.3.6.1.4.1.33940.5000.17.5.1.4"}).
%-define(WirelessIfMTU, {ifMtu, "1.3.6.1.4.1.33940.5000.17.5.1.5"}).
%-define(WirelessIfSpeed, {ifSpeed, "1.3.6.1.4.1.33940.5000.17.5.1.2"}).
%-define(WirelessIfMacAddress, {ifPhysAddress, "1.3.6.1.4.1.33940.5000.17.5.1.6"}).
%-define(WirelessIfAdminStatus, {ifAdminStatus, "1.3.6.1.4.1.33940.5000.17.5.1.14"}).
%-define(WirelessIfOperStatus, {ifOperStatus, "1.3.6.1.4.1.33940.5000.17.5.1.13"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fatap monet --ap 性能采集% 
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%ap连接信息统计
%-define(ApStationAssocSum, {apStationAssocSum, "1.3.6.1.4.1.33940.5000.4.1.1.240"}).
%-define(ApStationOnlineSum, {apStationOnlineSum, "1.3.6.1.4.1.33940.5000.1.187"}).
-define(AssocNum, {assocNum, "1.3.6.1.4.1.18603.50.6.2.1.4"}).
%失败数＝连接总数 － 连接成功数
-define(AssocFailNum, {assocFailNum, "1.3.6.1.4.1.18603.50.6.2.1.5"}).
-define(ReAssocNum, {reAssocNum, "1.3.6.1.4.1.18603.50.6.2.1.6"}).
-define(DeauthNum, {deauthNum, "1.3.6.1.4.1.18603.50.6.2.1.8"}).
%-define(AssocRefusedNum, {assocRefusedNum, "1.3.6.1.4.1.33940.5000.5.2.1.51"}).

%%ap 有线接口性能  
-define(WireInUcastPkts, {ifInUcastPkts, "1.3.6.1.4.1.18603.50.6.3.1.2"}).
%-define(WireInNUcastPkts,{ifInNUcastPkts, ""}).
-define(WireInOctets,{ifInOctets, "1.3.6.1.4.1.18603.50.6.3.1.4"}).
-define(WireInDiscards,{ifInDiscards, "1.3.6.1.4.1.18603.50.6.3.1.5"}).
-define(WireInErrors,{ifInErrors, "1.3.6.1.4.1.18603.50.6.3.1.6"}).

-define(WireOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.4.1.18603.50.6.3.1.7"}).  
%-define(WireOutNUcastPkts,{ifOutNUcastPkts, ""}).
-define(WireOutOctets,{ifOutOctets, "1.3.6.1.4.1.18603.50.6.3.1.9"}).
-define(WireOutDiscards,{ifOutDiscards, "1.3.6.1.4.1.18603.50.6.3.1.10"}).
-define(WireOutErrors,{ifOutErrors, "1.3.6.1.4.1.18603.50.6.3.1.11"}).

%%ap 无线接口性能  
%-define(WirelessInAvgSignal,{ifInAvgSignal, "1.3.6.1.4.1.33940.5000.5.2.1.99"}).
%-define(WirelessInHighSignal,{ifInHighSignal, "1.3.6.1.4.1.33940.5000.5.2.1.100"}).
%-define(WirelessInLowSignal,{ifInLowSignal, "1.3.6.1.4.1.33940.5000.5.2.1.101"}).
%-define(WirelessFrameRetryRate,{wirelessFrameRetryRate, "1.3.6.1.4.1.33940.5000.5.2.1.39"}).

-define(WirelessOutPkts,{ifOutPkts, "1.3.6.1.4.1.18603.50.6.4.1.6"}).
-define(WirelessInPkts,{ifInPkts, "1.3.6.1.4.1.18603.50.6.4.1.7"}).
-define(WirelessInOctets,{ifInOctets, "1.3.6.1.4.1.18603.50.6.4.1.9"}).
-define(WirelessOutOctets,{ifOutOctets, "1.3.6.1.4.1.18603.50.6.4.1.8"}).
%-define(WirelessInErrors,{ifInErrors, ""}).

%%ssid性能统计 
%-define(SsidName,{ssidName, ""}).
%-define(SsidInOctets, {ssidInOctets, ""}).
%-define(SsidInPkts, {ssidInPkts, ""}).
%-define(SsidOutOctets, {ssidOutOctets, ""}).
%-define(SsidOutPkts, {ssidOutPkts, ""}).

%%sta终端性能统计  --- StaIndex
-define(StaMac, {staMac, "1.3.6.1.4.1.18603.50.4.2.1.2"}).
%-define(StaRssi, {staRssi, ""}).
%-define(StaNoiseRate, {staNoiseRate, ""}).
-define(StaIp, {staIp, "1.3.6.1.4.1.18603.50.4.2.1.4"}).
%-define(StaChannel, {staChannel, ""}).
-define(StaVlan, {staVlan, "1.3.6.1.4.1.18603.50.4.2.1.9"}).
-define(StaSsid, {staSsid, "1.3.6.1.4.1.18603.50.4.2.1.10"}).
-define(StaTxframe, {staTxframe, "1.3.6.1.4.1.18603.50.4.2.1.18"}).
-define(StaTxbytes, {staTxbytes, "1.3.6.1.4.1.18603.50.4.2.1.19"}).
-define(StaRxframe, {staRxframe, "1.3.6.1.4.1.18603.50.4.2.1.20"}).
-define(StaRxbytes, {staRxbytes, "1.3.6.1.4.1.18603.50.4.2.1.21"}).
