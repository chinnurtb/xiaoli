-ifndef('HUAWEI-MIB').
-define('HUAWEI-MIB', true).

%======================================================================
% OLT版卡参数
%======================================================================
-define(hwMusaBoardCpuRate,     {cpuload,       "1.3.6.1.4.1.2011.2.6.7.1.1.2.1.5"}).
-define(hwMusaBoardRamUseRate,  {memusage,      "1.3.6.1.4.1.2011.2.6.7.1.1.2.1.6"}).
-define(hwMusaBoardSlotDesc,    {boardDescr,    "1.3.6.1.4.1.2011.2.6.7.1.1.2.1.7"}).
%-define(hwMusaBoardOnlineState, {onlineState,   "1.3.6.1.4.1.2011.2.6.7.1.1.2.1.8"}).
-define(hwSlotIndex,            {index,         "1.3.6.1.4.1.2011.6.3.3.2.1.1"}).
-define(hwSlotType,             {type,          "1.3.6.1.4.1.2011.6.3.3.2.1.2"}).
-define(hwSlotVersion,          {version,       "1.3.6.1.4.1.2011.6.3.3.2.1.5"}).
-define(hwSlotOperStatus,       {operstatus,    "1.3.6.1.4.1.2011.6.3.3.2.1.8"}).
-define(hwSlotAdminStatus,      {adminstatus,   "1.3.6.1.4.1.2011.6.3.3.2.1.9"}).
-define(GOnuPWD,                {authpassword, "1.3.6.1.4.1.2011.6.128.1.1.2.43.1.4"}).
-define(hwSlotTemperature,                {temperature, "1.3.6.1.4.1.2011.6.3.3.2.1.13"}).


%-define(hwSlotDesc,             {slotDescr,     "1.3.6.1.4.1.2011.6.3.3.2.1.3"}).
%-define(hwSlotPcbVersion,       {pcbVersion,    "1.3.6.1.4.1.2011.6.3.3.2.1.4"}).
-define(hwSlotWorkMode,         {workMode,      "1.3.6.1.4.1.2011.6.3.3.2.1.6"}).
%-define(hwSubSlots,             {subSlots,      "1.3.6.1.4.1.2011.6.3.3.2.1.7"}).
%-define(hwSlotPhySerialNum,     {phySerialNum,  "1.3.6.1.4.1.2011.6.3.3.2.1.11"}).

-define(hwSlotEntry, [
        ?hwMusaBoardCpuRate,
        ?hwMusaBoardRamUseRate,
        ?hwMusaBoardSlotDesc,
        ?hwSlotIndex,
        ?hwSlotType,
        ?hwSlotVersion,
        ?hwSlotOperStatus,
        ?GOnuPWD,
        ?hwSlotWorkMode,
        ?hwSlotAdminStatus]).

%======================================================================
% OLT PON口参数
%======================================================================
-define(Temperature,        {temperature,       "1.3.6.1.4.1.2011.6.128.1.1.2.33.1.1"}).
-define(Voltage,            {voltage,           "1.3.6.1.4.1.2011.6.128.1.1.2.33.1.2"}).
-define(Current,            {current,           "1.3.6.1.4.1.2011.6.128.1.1.2.33.1.3"}).
-define(TxPower,            {txPower,           "1.3.6.1.4.1.2011.6.128.1.1.2.33.1.4"}).
-define(RxPower,            {rxPower,           "1.3.6.1.4.1.2011.6.128.1.1.2.33.1.5"}).
%======================================================================
% olt pon状态
%======================================================================
-define(hwPortType,         {hwPortType,        "1.3.6.1.4.1.2011.6.3.3.4.1.2"}).
-define(hwPortOperStatus,   {ponOperStatus,     "1.3.6.1.4.1.2011.6.3.3.4.1.5"}).
-define(hwPortAdminStatus,  {ponAdminStatus,    "1.3.6.1.4.1.2011.6.3.3.4.1.6"}).

%======================================================================
% ONU   disco
%======================================================================
-define(hwOntProfileName,       {ontProfile,        "1.3.6.1.4.1.2011.6.128.1.1.2.55.1.4"}).
-define(hwOntDescr,             {ontDescr,          "1.3.6.1.4.1.2011.6.128.1.1.2.53.1.9"}).
-define(hwOntLineProfileName,   {ontLineProfile,    "1.3.6.1.4.1.2011.6.128.1.1.2.53.1.7"}).
-define(hwOntDbaProfileName,    {ontDbaProfile,     "1.3.6.1.4.1.2011.6.128.1.1.3.42.1.4"}).
-define(hwOntDbaFixedRate,      {ontDbaFixedRate,   "1.3.6.1.4.1.2011.6.128.1.1.3.21.1.3"}).
-define(hwOntDbaAssuredRate,    {ontDbaAssuredRate, "1.3.6.1.4.1.2011.6.128.1.1.3.21.1.4"}).
-define(hwOntDbaMaxRate,        {ontDbaMaxRate,     "1.3.6.1.4.1.2011.6.128.1.1.3.21.1.5"}).
-define(hwOntMacAddress,        {ontMacAddr,        "1.3.6.1.4.1.2011.6.128.1.1.2.53.1.3"}).
-define(opticalRoundTripTime,   {rtt,     "1.3.6.1.4.1.2011.6.128.1.1.2.57.1.20"}).
-define(onuloid,                   {loid,     "1.3.6.1.4.1.2011.6.128.1.1.2.53.1.12"}).



-define(hwOntHardwareVersion,   {ontHardVer,        "1.3.6.1.4.1.2011.6.128.1.1.2.55.1.4"}).
-define(hwOntSoftwareVersion,   {ontSoftVer,        "1.3.6.1.4.1.2011.6.128.1.1.2.55.1.5"}).

-define(hwOntCtrlReset,         {ontReset,          "1.3.6.1.4.1.2011.6.128.1.1.2.57.1.2"}).
-define(hwOntRunStatus,         {ontRunStatus,      "1.3.6.1.4.1.2011.6.128.1.1.2.57.1.15"}).
-define(hwOntConfigStatus,      {ontConfigStatus,      "1.3.6.1.4.1.2011.6.128.1.1.2.57.1.16"}).

-define(hwOntMatchStatus,       {ontMatchStatus,    "1.3.6.1.4.1.2011.6.128.1.1.2.57.1.18"}).
-define(hwOntDiscoveryState,    {ontDiscStatus,     "1.3.6.1.4.1.2011.6.128.1.1.2.57.1.17"}).
-define(hwOntUpStreamBandWidth, {ontUpBandWidth,    "1.3.6.1.4.1.2011.6.128.1.1.2.57.1.21"}).
-define(hwOntDownStreamBandWidth, {ontDownBandWidth, "1.3.6.1.4.1.2011.6.128.1.1.2.57.1.22"}).

-define(hwOntIpAddress,         {ipAddress,         "1.3.6.1.4.1.2011.6.128.1.1.2.60.1.2"}).

-define(hwOnuEntry, [
        ?hwOntDescr,
        ?hwOntProfileName,
        ?hwOntLineProfileName,
        ?hwOntMacAddress,
        ?hwOntHardwareVersion,
        ?hwOntSoftwareVersion,
		?hwOntConfigStatus,
        ?hwOntRunStatus,
        ?hwOntIpAddress,
        ?onuloid,
    	?opticalRoundTripTime]).

-define(hwOnuDbaRateEntry, [
        ?hwOntDbaAssuredRate,
        ?hwOntDbaMaxRate]).

%======================================================================
% PON口粒度性能指标
%======================================================================
%接收正确包数高字节
%接收正确包数低字节
%接收包数
-define(hwEponOltRecvRightPackNum, {recvRightPackNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.1"}).
%接收单播包数
-define(hwEponOltRecvUcastPackNum, {recvUcastPackNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.4"}).
%接收组播包数
-define(hwEponOltRecvMulticastPackNum, {recvMulticastPackNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.3"}).
%接收广播包数
-define(hwEponOltRecvBroadcastPackNum, {recvBroadcastPackNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.2"}).
%接收错误包数高字节
%接收错误包数低字节
%接收错误包数
-define(hwEponOltRecvErrPackNum, {recvErrPackNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.15"}).
%发送正确包数高字节
%发送正确包数低字节
%发送包数
-define(hwEponOltTranRightPackNum, {tranRightPackNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.17"}).
%发送单播包数
-define(hwEponOltTranUcastPackNum, {tranUcastPackNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.20"}).
%发送组播包数
-define(hwEponOltTranMulticastPackNum, {tranMulticastPackNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.19"}).
%发送广播包数
-define(hwEponOltTranBroadcastPackNum, {tranBroadcastPackNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.18"}).
%接收正确字节数
-define(hwEponOltRecvRightByteNum, {recvRightByteNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.12"}).
%接收错误字节数
%发送正确字节数
-define(hwEponOltTranRightByteNum, {tranRightByteNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.28"}).

%下行丢包数
-define(hwEponOltDownStreamDropRightFrameNum, {downStreamDropRightFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.16"}).
%上行丢包数
%下行丢弃字节数高字节
%下行丢弃字节数低字节
%上行丢弃包数高字节
%上行丢弃包数低字节

-define(hwPonPerfEntry, [
        ?hwEponOltRecvRightPackNum,
        ?hwEponOltRecvUcastPackNum,
        ?hwEponOltRecvMulticastPackNum,
        ?hwEponOltRecvBroadcastPackNum,
        ?hwEponOltRecvErrPackNum,
        ?hwEponOltTranRightPackNum,
        ?hwEponOltTranUcastPackNum,
        ?hwEponOltTranMulticastPackNum,
        ?hwEponOltTranBroadcastPackNum,
        ?hwEponOltRecvRightByteNum,
        ?hwEponOltTranRightByteNum,
        ?hwEponOltDownStreamDropRightFrameNum
        ]).

%接收的位长小于64字节的包数高字节
%接收的位长小于64字节的包数低字节
%接收的位长小于64字节的包数
-define(hwEponOltRcv64BytesFrames, {rcv64BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.5"}).
%发送的位长小于64字节的包数高字节
%发送的位长小于64字节的包数低字节
%发送的位长小于64字节的包数
-define(hwEponOltSnd64BytesFrames, {snd64BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.21"}).
%接收的位长在65-127字节的包数高字节
%接收的位长在65-127字节的包数低字节
%接收的位长在65-127字节的包数
-define(hwEponOltRcv65to127BytesFrames, {rcv65to127BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.6"}).
%发送的位长在65-127字节的包数高字节
%发送的位长在65-127字节的包数低字节
%发送的位长在65-127字节的包数
-define(hwEponOltSnd65to127BytesFrames, {snd65to127BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.22"}).
%接收的位长在128-255字节的包数高字节
%接收的位长在128-255字节的包数低字节
%接收的位长在128-255字节的包数
-define(hwEponOltRcv128to255BytesFrames, {rcv128to255BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.7"}).
%发送的位长在128-255字节的包数高字节
%发送的位长在128-255字节的包数低字节
%发送的位长在128-255字节的包数
-define(hwEponOltSnd128to255BytesFrames, {snd128to255BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.23"}).
%接收的位长在256-511字节的包数高字节
%接收的位长在256-511字节的包数低字节
%接收的位长在256-511字节的包数
-define(hwEponOltRcv256to511BytesFrames, {rcv256to511BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.8"}).
%发送的位长在256-511字节的包数高字节
%发送的位长在256-511字节的包数低字节
%发送的位长在256-511字节的包数
-define(hwEponOltSnd256to511BytesFrames, {snd256to511BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.24"}).
%接收的位长在512-1023字节的包数高字节
%接收的位长在512-1023字节的包数低字节
%接收的位长在512-1023字节的包数
-define(hwEponOltRcv512to1023BytesFrames, {rcv512to1023BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.9"}).
%发送的位长在512-1023字节的包数高字节
%发送的位长在512-1023字节的包数低字节
%发送的位长在512-1023字节的包数
-define(hwEponOltSnd512to1023BytesFrames, {snd512to1023BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.25"}).
%接收1024-1518字节数的包数高字节
%接收的位长在1024-1518字节的包数低字节
%接收的位长在1024-1518字节的包数
-define(hwEponOltRcv1024to1518BytesFrames, {rcv1024to1518BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.10"}).
%发送的位长在1024-1518字节的包数高字节
%发送的位长在1024-1518字节的包数低字节
%发送的位长在1024-1518字节的包数
-define(hwEponOltSnd1024to1518BytesFrames, {snd1024to1518BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.26"}).
%接收的位长大于1519字节的包数
-define(hwEponOltRcvOver1518BytesFrames, {rcvOver1518BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.11"}).
%发送的位长大于1519字节的包数
-define(hwEponOltSndOver1518BytesFrames, {sndOver1518BytesFrames, "1.3.6.1.4.1.2011.6.128.1.1.4.41.1.27"}).

-define(hwPonPerfExtEntry, [
  ?hwEponOltRcv64BytesFrames,
  ?hwEponOltSnd64BytesFrames,
  ?hwEponOltRcv65to127BytesFrames,
  ?hwEponOltSnd65to127BytesFrames,
  ?hwEponOltRcv128to255BytesFrames,
  ?hwEponOltSnd128to255BytesFrames,
  ?hwEponOltRcv256to511BytesFrames,
  ?hwEponOltSnd256to511BytesFrames,
  ?hwEponOltRcv512to1023BytesFrames,
  ?hwEponOltSnd512to1023BytesFrames,
  ?hwEponOltRcv1024to1518BytesFrames,
  ?hwEponOltSnd1024to1518BytesFrames,
  ?hwEponOltRcvOver1518BytesFrames,
  ?hwEponOltSndOver1518BytesFrames
  ]).

%======================================================================
% ONU粒度性能指标 TODO: 指标不全？
%======================================================================
%接收前向纠错
%接收错误字节数
%接收总字节数
-define(hwEponELportRecSumByteNum, {recSumByteNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.3"}).
%接收有效字节数
-define(hwEponELportRecByteUsefulNum, {recByteUsefulNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.8"}).
%接收正确包数
-define(hwEponELportRecRightFrameNum, {recRightFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.2"}).
%接收单播包数
-define(hwEponELportRecUcastFrameNum, {recUcastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.4"}).
%接收组播包数
-define(hwEponELportRecMulticastFrameNum, {recMulticastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.5"}).
%接收广播包数
-define(hwEponELportRecBroadcastFrameNum, {recBroadcastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.6"}).
%接收错误包数
-define(hwEponELportRecErrFrameNum, {recErrFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.9"}).
%发送总字节数
-define(hwEponELportTranSumByteNum, {tranSumByteNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.22"}).
%发送正确包数
-define(hwEponELportTranRightFrameNum, {tranRightFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.21"}).
%发送单播包数
-define(hwEponELportTranUcastFrameNum, {tranUcastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.23"}).
%发送组播包数
-define(hwEponELportTranMulticastFrameNum, {tranMulticastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.24"}).
%发送广播包数
-define(hwEponELportTranBroadcastFrameNum, {tranBroadcastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.25"}).
%发送错误包数
-define(hwEponELportTranErrFrameNum, {tranErrFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.33"}).


-define(hwOnuPerfEntry, [
        ?hwEponELportRecSumByteNum,
        ?hwEponELportRecRightFrameNum,
        ?hwEponELportRecUcastFrameNum,
        ?hwEponELportRecMulticastFrameNum,
        ?hwEponELportRecBroadcastFrameNum,
        ?hwEponELportRecErrFrameNum,
        ?hwEponELportTranSumByteNum,
        ?hwEponELportTranRightFrameNum,
        ?hwEponELportTranUcastFrameNum,
        ?hwEponELportTranMulticastFrameNum,
        ?hwEponELportTranBroadcastFrameNum,
        ?hwEponELportTranErrFrameNum]).


%======================================================================
% ONU用户口粒度性能指标
%======================================================================

%ont lan 端口在线状态，管理状态
-define(hwEponDeviceOntPortOperateStatus,       {adminstate, "1.3.6.1.4.1.2011.6.128.1.1.2.81.1.7"}).
-define(hwEponDeviceOntifEthernetOnlineState,   {operstate,  "1.3.6.1.4.1.2011.6.128.1.1.2.81.1.31"}).
-define(hwEponDeviceOntifEthernetSpeed,         {speed,      "1.3.6.1.4.1.2011.6.128.1.1.2.81.1.4"}).

%ont pstn 端口管理状态
-define(hwEponDeviceOntPOTSPortOperateStatus,   {adminstate,      "1.3.6.1.4.1.2011.6.128.1.1.2.83.1.2"}).



%接收前向纠错
%接收错误字节数
%接收总字节数
-define(hwEponUportRecSumByteNum, {recUportSumByteNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.20"}).
%接收有效字节数
-define(hwEponUportRecByteUsefulNum, {recUportByteUsefulNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.8"}).
%接收正确包数
-define(hwEponUportRecRightFrameNum, {recUportRightFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.1"}).
%接收单播包数
%-define(hwEponUportRecUcastFrameNum, {recUportUcastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.3"}).
%接收组播包数
-define(hwEponUportRecMulticastFrameNum, {recUportMulticastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.2"}).
%接收广播包数
-define(hwEponUportRecBroadcastFrameNum, {recUportBroadcastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.3"}).
%接收错误包数
-define(hwEponUportRecErrFrameNum, {recUportErrFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.17"}).
%接收丢包数
-define(hwEponUportRecDropFrameNum, {recUportDropFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.15"}).
%发送总字节数
-define(hwEponUportTranSumByteNum, {tranUportSumByteNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.32"}).
%发送正确包数
-define(hwEponUportTranRightFrameNum, {tranUportRightFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.21"}).
%发送单播包数
%-define(hwEponUportTranUcastFrameNum, {tranUportUcastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.27"}).
%发送组播包数
%-define(hwEponUportTranMulticastFrameNum, {tranUportMulticastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.28"}).
%发送广播包数
-define(hwEponUportTranBroadcastFrameNum, {tranUportBroadcastFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.43.1.27"}).
%发送错误包数
-define(hwEponUportTranErrFrameNum, {tranUportErrFrameNum, "1.3.6.1.4.1.2011.6.128.1.1.4.42.1.33"}).


-define(hwOnuUportEntry, [
        ?hwEponUportRecSumByteNum,
        ?hwEponUportRecRightFrameNum,
        ?hwEponUportRecMulticastFrameNum,
        ?hwEponUportRecBroadcastFrameNum,
        ?hwEponUportRecErrFrameNum,
        ?hwEponUportRecDropFrameNum,
        ?hwEponUportTranSumByteNum,
        ?hwEponUportTranRightFrameNum,
        %?hwEponUportTranUcastFrameNum,
        %?hwEponUportTranMulticastFrameNum,
        ?hwEponUportTranBroadcastFrameNum
        %?hwEponUportTranErrFrameNum
        ]).

-endif.
