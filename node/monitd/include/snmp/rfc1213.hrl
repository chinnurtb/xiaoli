%system group
-define(SysDescr, {sysDescr, [1,3,6,1,2,1,1,1,0]}).
-define(SysObjectID, {sysObjectID, [1,3,6,1,2,1,1,2,0]}).
-define(SysUpTime, {sysUpTime, [1,3,6,1,2,1,1,3,0]}).
-define(SysContact, {sysContact, [1,3,6,1,2,1,1,4,0]}).
-define(SysName, {sysName, [1,3,6,1,2,1,1,5,0]}).
-define(SysLocation, {sysLocation, [1,3,6,1,2,1,1,6,0]}).

%?SysContact, ?SysName, ?SysLocation
-define(System, [?SysDescr, ?SysObjectID, ?SysName]).

-define(IfIndex, {ifIndex, [1,3,6,1,2,1,2,2,1,1]}).
-define(IfDescr, {ifDescr, [1,3,6,1,2,1,2,2,1,2]}).
-define(IfType, {ifType, [1,3,6,1,2,1,2,2,1,3]}).
-define(IfMtu, {ifMtu, [1,3,6,1,2,1,2,2,1,4]}).
-define(IfSpeed, {ifSpeed, [1,3,6,1,2,1,2,2,1,5]}).
-define(IfPhysAddress, {ifPhysAddress, [1,3,6,1,2,1,2,2,1,6]}).
-define(IfAdminStatus, {ifAdminStatus, [1,3,6,1,2,1,2,2,1,7]}).
-define(IfOperStatus, {ifOperStatus, [1,3,6,1,2,1,2,2,1,8]}).
-define(IfInOctets, {ifInOctets, [1,3,6,1,2,1,2,2,1,10]}).
-define(IfInUcastPkts, {ifInUcastPkts, [1,3,6,1,2,1,2,2,1,11]}).
-define(IfInNUcastPkts, {ifInNUcastPkts, [1,3,6,1,2,1,2,2,1,12]}).
-define(IfInDiscards, {ifInDiscards, [1,3,6,1,2,1,2,2,1,13]}).
-define(IfInErrors, {ifInErrors, [1,3,6,1,2,1,2,2,1,14]}).
-define(IfInUnknownProtos, {ifInUnknownProtos, [1,3,6,1,2,1,2,2,1,15]}).
-define(IfOutOctets, {ifOutOctets, [1,3,6,1,2,1,2,2,1,16]}).
-define(IfOutUcastPkts, {ifOutUcastPkts, [1,3,6,1,2,1,2,2,1,17]}).
-define(IfOutNUcastPkts, {ifOutNUcastPkts, [1,3,6,1,2,1,2,2,1,18]}).
-define(IfOutDiscards, {ifOutDiscards, [1,3,6,1,2,1,2,2,1,19]}).
-define(IfOutErrors, {ifOutErrors, [1,3,6,1,2,1,2,2,1,20]}).

-define(IfEntry, [?IfIndex, ?IfDescr, ?IfType,
        ?IfMtu, ?IfSpeed, ?IfPhysAddress,
        ?IfAdminStatus, ?IfOperStatus]).

-define(DslamIfEntry, [?IfIndex, ?IfDescr, ?IfType,
        ?IfMtu, ?IfSpeed).

-define(IfColumns, [?IfIndex, ?IfType, ?IfDescr,
                    ?IfOperStatus, ?IfAdminStatus,
					?IfInOctets, ?IfOutOctets,
                    ?IfInDiscards, ?IfOutDiscards,
					?IfInUcastPkts, ?IfInNUcastPkts,
					?IfOutUcastPkts, ?IfOutNUcastPkts,
					?IfInErrors, ?IfOutErrors,
                    ?IfInUnknownProtos]).

-define(IfName, {ifName, [1,3,6,1,2,1,31,1,1,1,1]}).
-define(IfHCInOctets, {ifInOctets, [1,3,6,1,2,1,31,1,1,1,6]}).
-define(IfHCInUcastPkts, {ifInUcastPkts, [1,3,6,1,2,1,31,1,1,1,7]}).
-define(IfHCInMulticastPkts, {ifInMulticastPkts, [1,3,6,1,2,1,31,1,1,1,8]}).
-define(IfHCInBroadcastPkts, {ifInBroadcastPkts, [1,3,6,1,2,1,31,1,1,1,9]}).
-define(IfHCOutOctets, {ifOutOctets, [1,3,6,1,2,1,31,1,1,1,10]}).
-define(IfHCOutUcastPkts, {ifOutUcastPkts, [1,3,6,1,2,1,31,1,1,1,11]}).
-define(IfHCOutMulticastPkts, {ifOutMulticastPkts, [1,3,6,1,2,1,31,1,1,1,12]}).
-define(IfHCOutBroadcastPkts, {ifOutBroadcastPkts, [1,3,6,1,2,1,31,1,1,1,13]}).

-define(IfXColumns, [?IfIndex, ?IfType, ?IfDescr,
                    ?IfOperStatus, ?IfAdminStatus,
					?IfHCInOctets, ?IfHCOutOctets,
                    ?IfHCInUcastPkts, ?IfHCOutUcastPkts,
					?IfInNUcastPkts, ?IfOutNUcastPkts,
                    ?IfHCInMulticastPkts, ?IfHCOutMulticastPkts,
                    ?IfHCInBroadcastPkts, ?IfHCOutBroadcastPkts,
                    ?IfInDiscards, ?IfOutDiscards,
					?IfInErrors, ?IfOutErrors,
                    ?IfInUnknownProtos]).


-define(adslLineConfProfile, {adslLineConfProfile, "1.3.6.1.2.1.10.94.1.1.1.1.4"}).
-define(adslAtucChanConfInterleaveMaxTxRate, {adslAtucMaxTxRate, "1.3.6.1.2.1.10.94.1.1.14.1.14"}).
-define(adslAturChanConfInterleaveMaxTxRate, {adslAturMaxTxRate, "1.3.6.1.2.1.10.94.1.1.14.1.28"}).

-define(adslAtucPerfLofs, {adslAtucPerfLofs, "1.3.6.1.2.1.10.94.1.1.6.1.1"}).
-define(adslAtucPerfLoss, {adslAtucPerfLoss, "1.3.6.1.2.1.10.94.1.1.6.1.2"}).
-define(adslAtucPerfLols, {adslAtucPerfLols, "1.3.6.1.2.1.10.94.1.1.6.1.3"}).
-define(adslAtucPerfLprs, {adslAtucPerfLprs, "1.3.6.1.2.1.10.94.1.1.6.1.4"}).
-define(adslAtucPerfESs, {adslAtucPerfESs, "1.3.6.1.2.1.10.94.1.1.6.1.5"}).

-define(adslAturPerfLofs, {adslAturPerfLofs, "1.3.6.1.2.1.10.94.1.1.7.1.1"}).
-define(adslAturPerfLoss, {adslAturPerfLoss, "1.3.6.1.2.1.10.94.1.1.7.1.2"}).
-define(adslAturPerfLprs, {adslAturPerfLprs, "1.3.6.1.2.1.10.94.1.1.7.1.3"}).
-define(adslAturPerfESs, {adslAturPerfESs, "1.3.6.1.2.1.10.94.1.1.7.1.4"}).

-define(adslAtucPerfStatFastR, {adslAtucPerfStatFastR, "1.3.6.1.2.1.10.94.3.1.18.1.1"}).
-define(adslAtucPerfStatFailedFastR, {adslAtucPerfStatFailedFastR, "1.3.6.1.2.1.10.94.3.1.18.1.2"}).
-define(adslAtucPerfStatSesL, {adslAtucPerfStatSesL, "1.3.6.1.2.1.10.94.3.1.18.1.3"}).
-define(adslAtucPerfStatUasL, {adslAtucPerfStatUasL, "1.3.6.1.2.1.10.94.3.1.18.1.4"}).

-define(adslAturPerfStatSesL, {adslAturPerfStatSesL, "1.3.6.1.2.1.10.94.3.1.20.1.1"}).
-define(adslAturPerfStatUasL, {adslAturPerfStatUasL, "1.3.6.1.2.1.10.94.3.1.20.1.2"}).

-define(adslAtucChanReceivedBlks, {adslAtucChanReceivedBlks, "1.3.6.1.2.1.10.94.1.1.10.1.1"}).
-define(adslAtucChanTransmittedBlks, {adslAtucChanTransmittedBlks, "1.3.6.1.2.1.10.94.1.1.10.1.2"}).
-define(adslAtucChanCorrectedBlks, {adslAtucChanCorrectedBlks, "1.3.6.1.2.1.10.94.1.1.10.1.3"}).
-define(adslAtucChanUncorrectBlks, {adslAtucChanUncorrectBlks, "1.3.6.1.2.1.10.94.1.1.10.1.4"}).
-define(hwadslAtucChanFecBlks, {hwadslAtucChanFecBlks, "1.3.6.1.2.1.10.94.1.1.10.1.65536"}).
-define(hwadslAtucChanHecBlks, {hwadslAtucChanHecBlks, "1.3.6.1.2.1.10.94.1.1.10.1.65537"}).
-define(hwadslAtucChanCrcBlks, {hwadslAtucChanCrcBlks, "1.3.6.1.2.1.10.94.1.1.10.1.65538"}).

-define(adslAturChanReceivedBlks, {adslAturChanReceivedBlks, "1.3.6.1.2.1.10.94.1.1.11.1.1"}).
-define(adslAturChanTransmittedBlks, {adslAturChanTransmittedBlks, "1.3.6.1.2.1.10.94.1.1.11.1.2"}).
-define(adslAturChanCorrectedBlks, {adslAturChanCorrectedBlks, "1.3.6.1.2.1.10.94.1.1.11.1.3"}).
-define(adslAturChanUncorrectBlks, {adslAturChanUncorrectBlks, "1.3.6.1.2.1.10.94.1.1.11.1.4"}).
-define(hwadslAturChanFecBlks, {hwadslAturChanFecBlks, "1.3.6.1.2.1.10.94.1.1.11.1.65536"}).
-define(hwadslAturChanHecBlks, {hwadslAturChanHecBlks, "1.3.6.1.2.1.10.94.1.1.11.1.65537"}).
-define(hwadslAturChanCrcBlks, {hwadslAturChanCrcBlks, "1.3.6.1.2.1.10.94.1.1.11.1.65538"}).



%%dslam
%上行
-define(AdslAturChanConfFastMinTxRate,              {rfastMinTxRate,               "1.3.6.1.2.1.10.94.1.1.14.1.25"}).
-define(AdslAturChanConfInterleaveMinTxRate,        {rinterleaveMinTxRate,         "1.3.6.1.2.1.10.94.1.1.14.1.26"}).
-define(AdslAturChanConfFastMaxTxRate,              {rfastMaxTxRate,               "1.3.6.1.2.1.10.94.1.1.14.1.27"}).
-define(AdslAturChanConfInterleaveMaxTxRate,        {rinterleaveMaxTxRate,         "1.3.6.1.2.1.10.94.1.1.14.1.28"}).

%下行
-define(AdslAtucChanConfFastMinTxRate,              {cfastMinTxRate,               "1.3.6.1.2.1.10.94.1.1.14.1.11"}).
-define(AdslAtucChanConfInterleaveMinTxRate,        {cinterleaveMinTxRate,         "1.3.6.1.2.1.10.94.1.1.14.1.12"}).
-define(AdslAtucChanConfFastMaxTxRate,              {cfastMaxTxRate,               "1.3.6.1.2.1.10.94.1.1.14.1.13"}).
-define(AdslAtucChanConfInterleaveMaxTxRate,        {cinterleaveMaxTxRate,         "1.3.6.1.2.1.10.94.1.1.14.1.14"}).

-define(AdslChanConf, [
        ?AdslAturChanConfFastMinTxRate,
        ?AdslAturChanConfInterleaveMinTxRate,
        ?AdslAturChanConfFastMaxTxRate,
        ?AdslAturChanConfInterleaveMaxTxRate,
        ?AdslAtucChanConfFastMinTxRate,
        ?AdslAtucChanConfInterleaveMinTxRate,
        ?AdslAtucChanConfFastMaxTxRate,
        ?AdslAtucChanConfInterleaveMaxTxRate
        ]).

