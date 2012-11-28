-ifndef('ZTE-MIB').
-define('ZTE-MIB', true).


-define(zxAnOltTemperature,  {temperature, "1.3.6.1.4.1.3902.1015.2.1.3.2"}).


%zxAnCardTable
-define(zxAnCardCfgMainType,  {cfgmaintype, [1,3,6,1,4,1,3902,1015,2,1,1,3,1,2]}).
-define(zxAnCardActMainType,  {actmaintype, [1,3,6,1,4,1,3902,1015,2,1,1,3,1,3]}).
-define(zxAnCardActType, {cardacttype, [1,3,6,1,4,1,3902,1015,2,1,1,3,1,4]}).
-define(zxAnCardOperStatus, {operstatus, [1,3,6,1,4,1,3902,1015,2,1,1,3,1,5]}).
-define(zxAnCardAdminStatus, {adminstatus, [1,3,6,1,4,1,3902,1015,2,1,1,3,1,6]}).
-define(zxAnCardCpuLoad, {cpuload, [1,3,6,1,4,1,3902,1015,2,1,1,3,1,9]}).
-define(zxAnCardMemUsage, {memusage,  [1,3,6,1,4,1,3902,1015,2,1,1,3,1,11]}).
-define(zxAnCardStandbyStatus, {standbystatus, [1,3,6,1,4,1,3902,1015,2,1,1,3,1,13]}).
-define(zxAnCardLockstatus, {lockstatus, [1,3,6,1,4,1,3902,1015,2,1,1,3,1,18]}).

%zxAnCardVersionTable
-define(zxAnCardHardVersion, {hardversion, [1,3,6,1,4,1,3902,1015,2,1,2,2,1,1]}).
-define(zxAnVersionFileType, {masterversiontype, [1,3,6,1,4,1,3902,1015,2,1,2,2,1,3]}).
-define(zxAnVersionTag,  {masterversiontag, [1,3,6,1,4,1,3902,1015,2,1,2,2,1,4]}).
-define(zxAnBootromFileType,  {bootromfiletype,[1,3,6,1,4,1,3902,1015,2,1,2,2,1,8]}).
-define(zxAnBootromTag,  {bootromtag,[1,3,6,1,4,1,3902,1015,2,1,2,2,1,9]}).
-define(zxAnBootromBuildTime, {bootrombuildtime, [1,3,6,1,4,1,3902,1015,2,1,2,2,1,11]}).

-define(zxAnCard, [
        ?zxAnCardCfgMainType,
        ?zxAnCardActMainType,
        ?zxAnCardActType,
        ?zxAnCardOperStatus,
        ?zxAnCardAdminStatus,
        ?zxAnCardCpuLoad,
        ?zxAnCardMemUsage,
        ?zxAnCardStandbyStatus,
        ?zxAnCardLockstatus
        ]).

-define(zxAnCardVersion,[
        ?zxAnCardHardVersion,
        ?zxAnVersionFileType,
        ?zxAnVersionTag,
        ?zxAnBootromFileType,
        ?zxAnBootromTag,
        ?zxAnBootromBuildTime
    ]).

%TODO:
%dot3MpcpRoundTripTime
%dot3MpcpRoundTripTime表示OLT和ONU之间的RoundTrip时间
%光纤长度 = dot3MpcpRoundTripTim * 1.635 / 1000
-define(dot3MpcpRoundTripTime, {roundtriptime, [1,3,6,1,4,1,3902,1015,1010,1,2,1,1,10]}).

%onuAdminObjectTable
-define(onuDescript,  {name, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,1]}).
-define(onuUserInfo,  {userinfo, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,4]}).
-define(onuType, {type, "1.3.6.1.4.1.3902.1015.1010.1.7.4.1.5"}).   %use zxAnEponOnuModel
-define(onuAdminState,   {adminstate, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,6]}).
-define(onuAuthMACAddress,   {authmacaddr, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,7]}).
-define(onuRegisterMACAddress,  {registermacaddress, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,8]}).
-define(onuAuthMACSn,  {authmacsn, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,9]}).
-define(onuRegisterSn,  {regsn, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,10]}).
-define(onuCurrentRegState,  {regstate, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,11]}).
-define(onuRegisterTime, {onuRegisterTime, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,12]}).
-define(onuCurrAdminAuthState,    {onuCurrAdminAuthState, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,13]}).
-define(onuLatelyPassAdminAuthTime,   {onuLatelyPassAdminAuthTime, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,14]}).
-define(onuCurrDot1xAuthState,   {onuCurrDot1xAuthState, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,15]}).
-define(onuLatelyPassDot1xAuthTime,  {onuLatelyPassDot1xAuthTime, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,16]}).
-define(onuMgmtOnlineStatus, {onlinestatus, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,17]}).
-define(onuActiveStatus,  {activestatus, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,18]}).
-define(onuMgmtEntryStatus,  {onuMgmtEntryStatus, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,19]}).
-define(onuMgmtIpCfgMode,   {cfgmode, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,20]}).
-define(onuLoid,   {loid, [1,3,6,1,4,1,3902,1015,1010,1,7,4,1,23]}).

-define(zxAnOnu, [
        ?onuDescript,
        ?onuUserInfo,
        ?onuType,
        ?onuAdminState,
        ?onuAuthMACAddress,
        ?onuRegisterMACAddress,
        ?onuAuthMACSn,
        ?onuRegisterSn,
        ?onuCurrentRegState,
        ?onuLoid,
        %?onuRegisterTime,
        %?onuCurrAdminAuthState,
        %?onuLatelyPassAdminAuthTime,
        %?onuCurrDot1xAuthState,
        %?onuLatelyPassDot1xAuthTime,
        ?onuMgmtOnlineStatus,
        ?onuActiveStatus,
        ?dot3MpcpRoundTripTime,
        %?onuMgmtEntryStatus, TODO
        ?onuMgmtIpCfgMode]).

%zxAnEtherIfConfTable
-define(zxAnEtherIfConfDuplexSpeed, {zxAnEtherIfConfDuplexSpeed, [1,3,6,1,4,1,3902,1015,3,1,2,1,1]}).
-define(zxAnEtherIfActualDuplex, {zxAnEtherIfActualDuplex, [1,3,6,1,4,1,3902,1015,3,1,2,1,2]}).
-define(zxAnEtherIfActualSpeed, {zxAnEtherIfActualSpeed, [1,3,6,1,4,1,3902,1015,3,1,2,1,3]}).
-define(zxAnEtherIfPhyType, {zxAnEtherIfPhyType, [1,3,6,1,4,1,3902,1015,3,1,2,1,4]}).
-define(zxAnEtherIfConnectorType, {zxAnEtherIfConnectorType, [1,3,6,1,4,1,3902,1015,3,1,2,1,5]}).
-define(zxAnEtherIfFlowCtrl, {zxAnEtherIfFlowCtrl, [1,3,6,1,4,1,3902,1015,3,1,2,1,7]}).

%slaDownAdminObjectTable(该表每一行为一个ONU)
 -define(downAssuredBw, {downassuredbw, [1,3,6,1,4,1,3902,1015,1010,1,7,8,1,1]}).
 -define(downMaximumBw, {downmaximumbw, [1,3,6,1,4,1,3902,1015,1010,1,7,8,1,2]}).
 -define(downMaxBurstSize, {downmaxburstsize, [1,3,6,1,4,1,3902,1015,1010,1,7,8,1,3]}).

-define(slaDown, [
        ?downAssuredBw,
        ?downMaximumBw,
        ?downMaxBurstSize
        ]).

%slaUpAdminObjectTable(该表每一行为一个ONU)
-define(upAssuredBw, {upassuredbw, [1,3,6,1,4,1,3902,1015,1010,1,7,7,1,2]}).
-define(upMaximumBw, {upmaximumbw, [1,3,6,1,4,1,3902,1015,1010,1,7,7,1,3]}).
-define(upMaxBurstSize, {upmaxburstsize, [1,3,6,1,4,1,3902,1015,1010,1,7,7,1,4]}).

-define(slaUp, [
        ?upAssuredBw,
        ?upMaximumBw,
        ?upMaxBurstSize
        ]).


%zxAnEponOnuSnTable(该表每一行为一个ONU)
%-define(zxAnEponOnuVendorId, {vendor, [1,3,6,1,4,1,3902,1015,1010,1,1,1,1,1,2]}).
-define(zxAnEponOnuModel, {onuModel, [1,3,6,1,4,1,3902,1015,1010,1,1,1,1,1,3]}).
-define(zxAnEponOnuMacAddr, {macaddr, [1,3,6,1,4,1,3902,1015,1010,1,1,1,1,1,4]}).
-define(zxAnEponOnuHardwareVersion, {hardwareversion, [1,3,6,1,4,1,3902,1015,1010,1,1,1,1,1,5]}).
-define(zxAnEponOnuSoftwareVersion, {softwareversion, [1,3,6,1,4,1,3902,1015,1010,1,1,1,1,1,6]}).

-define(onuVerTable, [
        ?zxAnEponOnuModel,
        ?zxAnEponOnuMacAddr,
        ?zxAnEponOnuHardwareVersion,
        ?zxAnEponOnuSoftwareVersion
        ]).

-define(zxEponOnuIPAddress, {ip, [1,3,6,1,4,1,3902,1015,1010,1,1,1,24,1,1]}).
-define(zxEponOnuIPMask, {mask, [1,3,6,1,4,1,3902,1015,1010,1,1,1,24,1,2]}).

%c300
-define(zxponOnuIPAddress, {ip, "1.3.6.1.4.1.3902.1015.1010.5.9.1.1"}).
-define(zxponOnuIPMask, {mask, "1.3.6.1.4.1.3902.1015.1010.5.9.1.2"}).

-define(zxGponOntDevMgmtPassword, {authpassword, "1.3.6.1.4.1.3902.1012.3.28.1.1.7"}).

-define(gonuIpTable, [
        ?zxponOnuIPAddress,
        ?zxponOnuIPMask
        ]).

-define(onuIpTable, [
        ?zxEponOnuIPAddress,
        ?zxEponOnuIPMask
        ]).

%c300 gonu
-define(zxGponOnuIPAddress, {ip, "1.3.6.1.4.1.3902.1015.1010.5.9.1.1"}).
-define(zxGponOnuIPMask, {mask, "1.3.6.1.4.1.3902.1015.1010.5.9.1.2"}).

-define(zxGponOntDevMgmtTypeName, {type, "1.3.6.1.4.1.3902.1012.3.28.1.1.1"}).
-define(zxGponOntDevMgmtName, {userinfo, "1.3.6.1.4.1.3902.1012.3.28.1.1.2"}).
-define(zxGponOntDevMgmtDescription, {name, "1.3.6.1.4.1.3902.1012.3.28.1.1.3"}).
-define(zxGponOntDevMgmtProvisionSn, {authmacsn, "1.3.6.1.4.1.3902.1012.3.28.1.1.5"}).
-define(zxGponOntRegMode, {authmode, "1.3.6.1.4.1.3902.1012.3.28.1.1.12"}).
-define(zxGponOntRegId, {loid, "1.3.6.1.4.1.3902.1012.3.28.1.1.13"}).

-define(zxGponOntAdminState, {adminstate, "1.3.6.1.4.1.3902.1012.3.28.2.1.1"}).
-define(zxGponOntPhaseState, {operstate, "1.3.6.1.4.1.3902.1012.3.28.2.1.4"}).


-define(zxAnGOnu, [
        ?zxGponOntDevMgmtTypeName,
        ?zxGponOntDevMgmtName,
        ?zxGponOntDevMgmtDescription,
        ?zxGponOntDevMgmtProvisionSn,
        ?zxGponOntDevMgmtPassword,
        % ?zxGponOntRegMode,
        ?zxGponOnuIPAddress,
        ?zxGponOnuIPMask,
        ?zxGponOntRegId,
        ?zxGponOntAdminState,
        ?zxGponOntPhaseState]).

%(每一行为ONU的一个用户口？)
-define(zxAnEponOnuPhyAdminState,       {adminstate, "1.3.6.1.4.1.3902.1015.1010.1.1.3.1.1.1"}).
-define(zxAnEponOnuEthPortLinkState,    {operstate, "1.3.6.1.4.1.3902.1015.1010.1.1.1.5.1.2"}).
-define(zxAnEponOnuEthIfActualSpeed,    {speed, "1.3.6.1.4.1.3902.1015.1010.1.1.3.2.1.6"}).
-define(zxAnEponOnuPfmncStatisEntryin,  {ifInOctets, "1.3.6.1.4.1.3902.1015.1010.1.1.7.1.1.2"}).
-define(zxAnEponOnuPfmncStatisEntryout, {ifOutOctets, "1.3.6.1.4.1.3902.1015.1010.1.1.7.1.1.7"}).
-define(zxAnEponOnuVoipPortEnable,      {adminstate, "1.3.6.1.4.1.3902.1015.1010.1.1.1.8.1.1"}).
-define(zxAnEponRmVoipPortOperStatus,   {operstate, "1.3.6.1.4.1.3902.1015.1010.1.1.1.36.11.1.1"}).





%%perfomance mib

%======================================================================
% OLT PON口参数
%======================================================================
-define(Temperature, {temperature, "1.3.6.1.4.1.3902.1015.1010.11.1.1.2"}).
-define(Voltage, {voltage, "1.3.6.1.4.1.3902.1015.1010.11.1.1.3"}).
-define(Current, {current, "1.3.6.1.4.1.3902.1015.1010.11.1.1.4"}).
-define(TxPower, {txPower, "1.3.6.1.4.1.3902.1015.1010.11.1.1.5"}).
-define(RxPower, {rxPower, "1.3.6.1.4.1.3902.1015.1010.11.2.1.2"}).

%zxAnEponIfXOltTable: each is a pon
-define(zxAnEponIfOltHCInOctets, {ifInOctets, "1.3.6.1.4.1.3902.1015.1010.1.9.1.8.1.5"}).
-define(zxAnEponIfOltHCInUcastPkts, {ifInUcastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.8.1.6"}).
-define(zxAnEponIfOltHCInMulticastPkts, {ifInMulticastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.8.1.7"}).
-define(zxAnEponIfOltHCInBroadcastPkts, {ifInBroadcastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.8.1.8"}).
-define(zxAnEponIfOltHCOutOctets, {ifOutOctets, "1.3.6.1.4.1.3902.1015.1010.1.9.1.8.1.9"}).
-define(zxAnEponIfOltHCOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.8.1.10"}).
-define(zxAnEponIfOltHCOutMulticastPkts, {ifOutMulticastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.8.1.11"}).
-define(zxAnEponIfOltHCOutBroadcastPkts, {ifOutBroadcastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.8.1.12"}).

-define(zxAnEponIfXOltTable, [
        ?zxAnEponIfOltHCInOctets,
        ?zxAnEponIfOltHCInUcastPkts,
        ?zxAnEponIfOltHCInMulticastPkts,
        ?zxAnEponIfOltHCInBroadcastPkts,
        ?zxAnEponIfOltHCOutOctets,
        ?zxAnEponIfOltHCOutUcastPkts,
        ?zxAnEponIfOltHCOutMulticastPkts
        %?zxAnEponIfOltHCOutBroadcastPkts
        ]).

%zxAnEponIfEntry: each is a onu
-define(zxAnEponIfInDiscards, {ifInDiscards, "1.3.6.1.4.1.3902.1015.1010.1.9.1.4.1.4"}).
-define(zxAnEponIfInErrors, {ifInErrors, "1.3.6.1.4.1.3902.1015.1010.1.9.1.4.1.5"}).
-define(zxAnEponIfOutDiscards, {ifOutDiscards, "1.3.6.1.4.1.3902.1015.1010.1.9.1.4.1.10"}).
-define(zxAnEponIfOutErrors, {ifOutErrors, "1.3.6.1.4.1.3902.1015.1010.1.9.1.4.1.11"}).

-define(zxAnEponIfTable, [
        ?zxAnEponIfInDiscards,
        ?zxAnEponIfInErrors,
        ?zxAnEponIfOutDiscards,
        ?zxAnEponIfOutErrors
        ]).

%zxAnEponIfXTable: each is a onu
-define(zxAnEponIfHCInOctets, {ifInOctets, "1.3.6.1.4.1.3902.1015.1010.1.9.1.5.1.5"}).
-define(zxAnEponIfHCInUcastPkts, {ifInUcastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.5.1.6"}).
-define(zxAnEponIfHCInMulticastPkts, {ifInMulticastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.5.1.7"}).
-define(zxAnEponIfHCInBroadcastPkts, {ifInBroadcastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.5.1.8"}).
-define(zxAnEponIfHCOutOctets, {ifOutOctets, "1.3.6.1.4.1.3902.1015.1010.1.9.1.5.1.9"}).
-define(zxAnEponIfHCOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.5.1.10"}).
-define(zxAnEponIfHCOutMulticastPkts, {ifOutMulticastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.5.1.11"}).
-define(zxAnEponIfHCOutBroadcastPkts, {ifOutBroadcastPkts, "1.3.6.1.4.1.3902.1015.1010.1.9.1.5.1.12"}).

-define(zxAnEponIfXTable, [
        ?zxAnEponIfHCInOctets,
        ?zxAnEponIfHCInUcastPkts,
        ?zxAnEponIfHCInMulticastPkts,
        ?zxAnEponIfHCInBroadcastPkts,
        ?zxAnEponIfHCOutOctets,
        ?zxAnEponIfHCOutUcastPkts,
        ?zxAnEponIfHCOutMulticastPkts,
        ?zxAnEponIfHCOutBroadcastPkts
        ]).

%%reset
-define(dot3ExtPkgObjectReset, [{onuReset, "1.3.6.1.4.1.3902.1015.1010.1.5.1.1.1.1"}]).
-define(oltPonAttrReset, [{oltPonReset, "1.3.6.1.4.1.3902.1015.1010.1.7.16.1.3"}]).
-endif.
