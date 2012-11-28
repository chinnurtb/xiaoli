%%%%%%%%%%%%%%%%%%huawei gpon onu disco %%%%%%%%%%%%%%%%%%%%%%

-define(hwGponDeviceOntAuthMethod, {onuAuthMethod, "1.3.6.1.4.1.2011.6.128.1.1.2.43.1.2"}).
-define(hwGponDeviceOntSn, {onuAuthSn, "1.3.6.1.4.1.2011.6.128.1.1.2.43.1.3"}).
-define(hwGponDeviceOntPassword, {onuPassword, "1.3.6.1.4.1.2011.6.128.1.1.2.43.1.4"}).
-define(hwGponDeviceOntVersion, {hardVersion, "1.3.6.1.4.1.2011.6.128.1.1.2.45.1.1"}).
-define(hwGponDeviceOntMainSoftVer, {softVersion, "1.3.6.1.4.1.2011.6.128.1.1.2.45.1.5"}).
%-define(hwGponDeviceOntRegisterSn, {ontRegisterSn, "1.3.6.1.4.1.2011.6.128.1.1.2.52.1.2"}).  %很多
-define(hwGponDeviceOntControlRunStatus, {ontRunStatus, "1.3.6.1.4.1.2011.6.128.1.1.2.46.1.15"}).
-define(hwGponDeviceOntControlConfigStatus, {ontConfigStatus, "1.3.6.1.4.1.2011.6.128.1.1.2.46.1.16"}).
-define(hwGponDeviceOntIpAddress, {ontIpAddress, "1.3.6.1.4.1.2011.6.128.1.1.2.49.1.2"}).
-define(hwGponDeviceOntNetMask, {ontNetMask, "1.3.6.1.4.1.2011.6.128.1.1.2.49.1.3"}).
-define(gOnuPWD,                {authpassword, "1.3.6.1.4.1.2011.6.128.1.1.2.53.1.4"}).
-define(gponRoundTripTime,      {rtt,     "1.3.6.1.4.1.2011.6.128.1.1.2.46.1.20"}).
-define(gonuloid,                   {loid,     "1.3.6.1.4.1.2011.6.128.1.1.2.53.1.13"}).

-define(hwGponOnuEntry, [
    ?hwGponDeviceOntAuthMethod,
    ?hwGponDeviceOntSn,
    ?hwGponDeviceOntPassword,
    ?hwGponDeviceOntVersion,
    ?hwGponDeviceOntMainSoftVer,
    ?hwGponDeviceOntControlRunStatus,
    ?hwGponDeviceOntControlConfigStatus,
    ?hwGponDeviceOntIpAddress,
    ?gOnuPWD,
    ?gonuloid,
    ?gponRoundTripTime,
    ?hwGponDeviceOntNetMask
]).

%% onu --- 线路模板
-define(hwGponDeviceOntLineProfName, {ontLineProfName, "1.3.6.1.4.1.2011.6.128.1.1.2.43.1.7"}).

%% olt gem ++ mapping --- vlanid
-define(hwGponDeviceLineProfMappingCfgVlanId, {vlanId, "1.3.6.1.4.1.2011.6.128.1.1.3.64.1.8"}).

%% onu 线路模板 ++ gem --- tcount
-define(hwGponDeviceLineProfGemCfgTcontIndex, {tcontIndex, "1.3.6.1.4.1.2011.6.128.1.1.3.63.1.4"}).

%% onu 线路模板 ++ tcount --- dba模板
-define(hwGponDeviceLineProfTcontCfgDbaProfileName, {ontDbaProfile, "1.3.6.1.4.1.2011.6.128.1.1.3.62.1.3"}).

%% onu dba模板 --- rate
-define(hwXponDeviceDbaProfileType, {ontDbaType, "1.3.6.1.4.1.2011.6.128.1.1.3.21.1.2"}).
-define(hwXponDeviceDbaProfileFixedRate, {ontDbaFixedRate, "1.3.6.1.4.1.2011.6.128.1.1.3.21.1.3"}).
-define(hwXponDeviceDbaProfileAssuredRate, {ontDbaAssuredRate, "1.3.6.1.4.1.2011.6.128.1.1.3.21.1.4"}).
-define(hwXponDeviceDbaProfileMaxRate, {ontDbaMaxRate, "1.3.6.1.4.1.2011.6.128.1.1.3.21.1.5"}).

-define(hwXponOnuDbaRateEntry, [
        ?hwXponDeviceDbaProfileType,
        ?hwXponDeviceDbaProfileFixedRate,
        ?hwXponDeviceDbaProfileAssuredRate,
        ?hwXponDeviceDbaProfileMaxRate
    ]).

%%%%%%%%%%%%%%%%%%huawei gpon olt pon %%%%%%%%%%%%%%%%%%%%%%
-define(hwGponOltEthernetStatisticReceivedBytes, {ifInOctets, "1.3.6.1.4.1.2011.6.128.1.1.4.21.1.15"}).
-define(hwGponOltEthernetStatisticSendBytes, {ifOutOctets, "1.3.6.1.4.1.2011.6.128.1.1.4.21.1.30"}).

%%%%%%%%%%%%%%%%%%huawei gpon onu %%%%%%%%%%%%%%%%%%%%%%
-define(hwGponOntStatisticUpBytes, {ifInOctets, "1.3.6.1.4.1.2011.6.128.1.1.4.23.1.3"}).
-define(hwGponOntStatisticDownBytes, {ifOutOctets, "1.3.6.1.4.1.2011.6.128.1.1.4.23.1.4"}).


%ftth onu 端口在线状态，管理状态 通过olt采集
-define(hwGponDeviceOntPortOperateStatus,       {adminstate, "1.3.6.1.4.1.2011.6.128.1.1.2.62.1.5"}).
-define(hwGponDeviceOntifEthernetOnlineState,   {operstate,  "1.3.6.1.4.1.2011.6.128.1.1.2.62.1.22"}).
-define(hwGponDeviceOntEthernetSpeed,           {speed,  "1.3.6.1.4.1.2011.6.128.1.1.2.62.1.4"}).



-define(GTemperature,        {temperature,       "1.3.6.1.4.1.2011.6.128.1.1.2.23.1.1"}).
-define(GVoltage,            {voltage,           "1.3.6.1.4.1.2011.6.128.1.1.2.23.1.2"}).
-define(GCurrent,            {current,           "1.3.6.1.4.1.2011.6.128.1.1.2.23.1.3"}).
-define(GTxPower,            {txPower,           "1.3.6.1.4.1.2011.6.128.1.1.2.23.1.4"}).
-define(GRxPower,            {rxPower,           "1.3.6.1.4.1.2011.6.128.1.1.2.23.1.5"}).