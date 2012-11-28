-ifndef('UT-MIB').
-define('UT-MIB', true).

-define(utsEponModuleBoardPhyId, {boardId, "1.3.6.1.4.1.7064.1800.2.1.1.1.1.1"}).
-define(utsEponModuleBoardType, {boardType, "1.3.6.1.4.1.7064.1800.2.1.1.1.1.2"}).
-define(utsEponModuleBoardState, {boardState, "1.3.6.1.4.1.7064.1800.2.1.1.1.1.3"}).
-define(utsEponModuleBoardSwVer, {boardSwVer, "1.3.6.1.4.1.7064.1800.2.1.1.1.1.7"}).
-define(utsEponModuleBoardBootromVer, {boardBootromVer, "1.3.6.1.4.1.7064.1800.2.1.1.1.1.9"}).
-define(utsEponModuleBoardVer, {boardVer, "1.3.6.1.4.1.7064.1800.2.1.1.1.1.10"}).

-define(utsEponModule, [?utsEponModuleBoardType,
    ?utsEponModuleBoardState,
    ?utsEponModuleBoardSwVer,
    ?utsEponModuleBoardBootromVer,
    ?utsEponModuleBoardVer]).

-define(utsEthIfExtModuleId,{moduleId,"1.3.6.1.4.1.7064.1800.2.2.1.1.1.1"}).
-define(utsEthIfExtPortId,{portId,"1.3.6.1.4.1.7064.1800.2.2.1.1.1.2"}).
-define(utsEthIfExtPortType,{portType,"1.3.6.1.4.1.7064.1800.2.2.1.1.1.6"}).
-define(utsEthIfExtAdminStatus,{adminStatus,"1.3.6.1.4.1.7064.1800.2.2.1.1.1.10"}).
-define(utsEthIfExtIfIndex,{ifIndex,"1.3.6.1.4.1.7064.1800.2.2.1.1.1.11"}).
-define(utsEthIfExtDescription,{description,"1.3.6.1.4.1.7064.1800.2.2.1.1.1.12"}).

-define(utsEthIf, [?utsEthIfExtModuleId,
    ?utsEthIfExtPortId,
    ?utsEthIfExtPortType,
    ?utsEthIfExtAdminStatus,
    ?utsEthIfExtIfIndex,
    ?utsEthIfExtDescription]).

-define(utsDot3OltModuleId,{moduleId,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.1"}).
-define(utsDot3OltPortId,{portId,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.2"}).
-define(utsDot3OltOperStatus,{operStatus,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.3"}).
-define(utsDot3OltMACAddress,{macAddress,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.4"}).
-define(utsDot3OltHwVersion,{hwVersion,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.16"}).
-define(utsDot3OltFwVersion,{fwVersion,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.17"}).
-define(utsDot3OltTransceiverType,{transceiverType,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.18"}).
-define(utsDot3OltAdminStatus,{adminStatus,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.19"}).
-define(utsDot3OltRegisteredONUCount,{regONUCount,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.21"}).
-define(utsDot3OltOamLimit,{oamLimit,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.22"}).
-define(utsDot3OltRunningMode,{runningMode,"1.3.6.1.4.1.7064.1800.2.3.1.1.1.1.1.23"}).

-define(utsPonIf, [?utsDot3OltModuleId,
    ?utsDot3OltPortId,
    ?utsDot3OltOperStatus,
    ?utsDot3OltMACAddress,
    ?utsDot3OltAdminStatus]).

-define(utsDot3OnuCtcModuleId,{moduleId,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.10.1.1"}).
-define(utsDot3OnuCtcDeviceId,{deviceId,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.10.1.2"}).
-define(utsDot3OnuCtcPortId,{portId,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.10.1.3"}).
-define(utsDot3OnuCtcLogicalPortId,{logicalPortId,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.10.1.4"}).
-define(utsDot3OnuOui,{onuOui,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.10.1.6"}).
-define(utsDot3Onu2SerialNumber,{serialNumber,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.10.1.8"}).
-define(utsDot3OnuChipsetId,{chipsetId,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.10.1.10"}).

-define(utsOnu, [?utsDot3OnuCtcModuleId,
    ?utsDot3OnuCtcDeviceId,
    ?utsDot3OnuCtcPortId,
    ?utsDot3OnuCtcLogicalPortId,
    ?utsDot3OnuOui,
    ?utsDot3Onu2SerialNumber,
    ?utsDot3OnuChipsetId]).
-define(utsDot3OnuUpstreamPir,{ustreamPir,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.5.1.5"}).
-define(utsDot3OnuDownstreamPir,{dstreamPir,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.5.1.6"}).
-define(utsDot3OnuUpstreamCir,{ustreamCir,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.5.1.7"}).
-define(utsDot3OnuDownstreamCir,{dstreamCir,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.5.1.8"}).
-define(utsDot3OnuUpstreamMaxBurstSize,{uMaxBSize,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.5.1.9"}).
-define(utsDot3OnuDownstreamMaxBurstSize,{dMaxBSize,"1.3.6.1.4.1.7064.1800.2.3.1.2.1.5.1.10"}).
-define(utsOnuBw,[?utsDot3OnuUpstreamPir,
				  ?utsDot3OnuDownstreamPir,
				  ?utsDot3OnuUpstreamCir,
				  ?utsDot3OnuDownstreamCir,
				  ?utsDot3OnuUpstreamMaxBurstSize,
				  ?utsDot3OnuDownstreamMaxBurstSize]).


-define(utsPonIf2ExtStatModule,{statModule,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.1"}).
-define(utsPonIf2ExtStatDevice,{statDevice,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.2"}).
-define(utsPonIf2ExtStatPort,{statPort,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.3"}).
-define(utsPonIf2ExtStatLogicalLinkId,{statLinkId,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.4"}).
-define(utsPonIfExtSysFrameTxOk,{ftxOk,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.5"}).
-define(utsPonIfExtSysFrameRxOk,{frxOk,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.6"}).
-define(utsPonIfExtSysFrameTxErr,{txErr,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.7"}).
-define(utsPonIfExtSysFrameRxErr,{rxErr,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.8"}).
-define(utsPonIfExtSysFrameTxUnicast,{txUnicast,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.9"}).
-define(utsPonIfExtSysFrameRxUnicast,{rxUnicast,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.10"}).
-define(utsPonIfExtSysFrameTxMulticast,{txMulticast,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.11"}).
-define(utsPonIfExtSysFrameRxMulticast,{rxMulticast,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.12"}).
-define(utsPonIfExtSysFrameTxBroadcast,{txBroadcast,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.13"}).
-define(utsPonIfExtSysFrameRxBroadcast,{rxBroadcast,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.14"}).
-define(utsPonIfExtSysOctetTxOk,{otxOk,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.30"}).
-define(utsPonIfExtSysOctetRxOk,{orxOk,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.31"}).
-define(utsPonIfExtPonFrameTxOk,{ponftxOk,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.60"}).
-define(utsPonIfExtPonFrameRxOk,{ponfrxOk,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.61"}).
-define(utsPonIfExtPonOctetTxOk,{ponotxOk,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.80"}).
-define(utsPonIfExtPonOctetRxOk,{ponorxOk,"1.3.6.1.4.1.7064.1800.4.2.1.1.6.1.81"}).

-define(utsPonIfExt,[?utsPonIf2ExtStatModule,
 ?utsPonIf2ExtStatDevice,
 ?utsPonIf2ExtStatPort,
 ?utsPonIf2ExtStatLogicalLinkId,
?utsPonIfExtSysFrameTxOk,
?utsPonIfExtSysFrameRxOk,
?utsPonIfExtSysFrameTxErr,
?utsPonIfExtSysFrameRxErr,
?utsPonIfExtSysFrameTxUnicast,
?utsPonIfExtSysFrameRxUnicast,
?utsPonIfExtSysFrameTxMulticast,
?utsPonIfExtSysFrameRxMulticast,
?utsPonIfExtSysFrameTxBroadcast,
?utsPonIfExtSysFrameRxBroadcast,
?utsPonIfExtSysOctetTxOk,
?utsPonIfExtSysOctetRxOk,
?utsPonIfExtPonFrameTxOk,
?utsPonIfExtPonFrameRxOk,
?utsPonIfExtPonOctetTxOk,
?utsPonIfExtPonOctetRxOk]).

-endif.

