%clt
-define(headendNwHdId,			{headendNwHdId,			"1.3.6.1.4.1.5959.1.1.1.1.1"}).
-define(headendNwIpAddr,		{ip,		"1.3.6.1.4.1.5959.1.1.1.1.1.2"}).
-define(headendNwNetMask,		{mask,		"1.3.6.1.4.1.5959.1.1.1.1.1.3"}).
-define(headendNwEmac,			{mac,			"1.3.6.1.4.1.5959.1.1.1.1.1.9"}).


-define(headendSysHdId,			{headendSysHdId,		"1.3.6.1.4.1.5959.1.1.4.1.1.1"}).
-define(headendSysInfoBrdName,	{type,	"1.3.6.1.4.1.5959.1.1.4.1.1.2"}).
-define(headendSysInfoHwVer,	{hardware_version,	"1.3.6.1.4.1.5959.1.1.4.1.1.3"}).
-define(headendSysInfoSwVer,	{software_version,   "1.3.6.1.4.1.5959.1.1.4.1.1.4"}).


-define(headendOpHdId,			{headendOpHdId,			"1.3.6.1.4.1.5959.1.1.3.1.1.1"}).
-define(headendOpMode,			{oper_mode,			"1.3.6.1.4.1.5959.1.1.3.1.1.2"}).
-define(headendOpRadioChnn,		{radio_channel,	"1.3.6.1.4.1.5959.1.1.3.1.1.3"}).


-define(cnuNumHdId,				{cnuNumHdId,			"1.3.6.1.4.1.5959.1.3.1.1.1.1"}).
-define(cnuNumCnuNumber,		{cnu_num,		"1.3.6.1.4.1.5959.1.3.1.1.1.2"}).
-define(cnuSysRuntime ,			{cnuSysRuntime,	"1.3.6.1.4.1.5959.1.3.4.1.1.6"}).


-define(headendStatHdId, 		{headendStatHdId , 		"1.3.6.1.4.1.5959.1.1.2.1.1.1"}).
-define(headendStatIf,			{headendStatIf,			"1.3.6.1.4.1.5959.1.1.2.1.1.2"}).

-define(headendStatInOctets,	{headendStatInOctets ,	"1.3.6.1.4.1.5959.1.1.2.1.1.3"}).
-define(headendStatOutOctets,	{headendStatOutOctets,	"1.3.6.1.4.1.5959.1.1.2.1.1.8"}).



-define(cnuNwStatus,   {cnu_status,	"1.3.6.1.4.1.5959.1.3.1.2.1.3"}).
-define(cnuNwEmac0 ,   {mac	,"1.3.6.1.4.1.5959.1.3.1.2.1.4"}).
-define(cnuNwEmac1 ,   {mac1	,"1.3.6.1.4.1.5959.1.3.1.2.1.5"}).
-define(cnuNwType  ,   {type	,"1.3.6.1.4.1.5959.1.3.1.2.1.6"}).
-define(cnuNwBoardType,{ port_num,	"1.3.6.1.4.1.5959.1.3.1.2.1.9"}).

-define(cnuSysBrdName,{ type	,"1.3.6.1.4.1.5959.1.3.4.1.1.3"}).
-define(cnuSysHwVer,{ hardware_version 	,"1.3.6.1.4.1.5959.1.3.4.1.1.4"}).
-define(cnuSysSwVer,{ software_version,	"1.3.6.1.4.1.5959.1.3.4.1.1.5"}).

-define(cnuVlanCfgVlanEnable ,{vlan_enable,	"1.3.6.1.4.1.5959.1.3.5.1.1.3"}).
-define(cnuVlanCfgVlan0ID ,{vlan0	,"1.3.6.1.4.1.5959.1.3.5.1.1.4"}).
-define(cnuVlanCfgVlan1ID ,{vlan1,	"1.3.6.1.4.1.5959.1.3.5.1.1.5"}).

-define(cnuPortCfgDlBwLmt ,{cnuPortCfgDlBwLmt ,	"1.3.6.1.4.1.5959.1.3.7.1.1.4"}).
-define(cnuPortCfgUlBwLmt ,{cnuPortCfgUlBwLmt ,	"1.3.6.1.4.1.5959.1.3.7.1.1.5"}).


-define(cnuStatInOctets ,{cnuStatInOctets,	"1.3.6.1.4.1.5959.1.3.2.1.1.4"}).
-define(cnuStatOutOctets,{ cnuStatOutOctets,	"1.3.6.1.4.1.5959.1.3.2.1.1.9"}).


-define(zxCltTable, [
		?headendOpMode,
		?headendOpRadioChnn,
		?cnuNumCnuNumber,
        ?headendNwIpAddr,
        ?headendNwNetMask,
        ?headendSysInfoBrdName,
        ?headendSysInfoHwVer,
        ?headendSysInfoSwVer
        ]).



-define(zxCltPort, [
		?headendStatInOctets,
		?headendStatOutOctets
		]).


-define(zxCnuTable0, [
        ?cnuNwStatus,
        ?cnuNwEmac0,
        ?cnuNwEmac1,
        ?cnuNwBoardType
        ]).


-define(zxCnuTable1, [
        ?cnuSysBrdName,
        ?cnuSysHwVer,
        ?cnuSysSwVer
        ]).

-define(zxCnuTable2, [
        ?cnuVlanCfgVlanEnable,
        ?cnuVlanCfgVlan0ID,
        ?cnuVlanCfgVlan1ID
        ]).


-define(cnuRuntime, [
		?cnuSysRuntime
		]).
-define(zxCnuPortD, [
		?cnuPortCfgDlBwLmt,
		?cnuPortCfgUlBwLmt
		]).
-define(zxCnuPortM, [
		?cnuStatInOctets,
		?cnuStatOutOctets
		]).


