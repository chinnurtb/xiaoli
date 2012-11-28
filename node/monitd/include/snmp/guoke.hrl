%clt
-define(cltConfigNetworkSubnetMask,	        {mask,	            "1.3.6.1.4.1.36368.1.1.1.1.2.1.3.4"}).
-define(cltInfoMacAddress,		            {mac,	            "1.3.6.1.4.1.36368.1.1.1.1.1.2"}).
%-define(cltConfigNetworkIPAddress,	        {ip,            	"1.3.6.1.4.1.36368.1.1.1.1.2.1.3.3"}).
-define(cltInfoProductModel,		        {type,	            "1.3.6.1.4.1.36368.1.1.1.1.1.1"}).
-define(cltInfoHardwareVersion,		        {hardware_version,	"1.3.6.1.4.1.36368.1.1.1.1.1.5"}).
-define(cltInfoSoftwareVersion,             {software_version,	"1.3.6.1.4.1.36368.1.1.1.1.1.4"}).
-define(cltNetworkUnitTopologyTotalNumber,  {cnu_num,	        "1.3.6.1.4.1.36368.1.1.1.1.3.3.4"}).
-define(CltTable, [
		?cltConfigNetworkSubnetMask,
		?cltInfoMacAddress,
		?cltInfoProductModel,
        ?cltInfoHardwareVersion,
        ?cltInfoSoftwareVersion,
        ?cltNetworkUnitTopologyTotalNumber
        ]).

%cnu
-define(cltNetworkUnitOnlineStatus,	        {cnu_status,	"1.3.6.1.4.1.36368.1.1.1.1.3.3.5.1.5"}).
-define(cltNetworkUnitMACAddress,		    {mac,	            "1.3.6.1.4.1.36368.1.1.1.1.3.3.5.1.3"}).
-define(cltNetworkUnitDeviceModel,		    {type,	            "1.3.6.1.4.1.36368.1.1.1.1.3.3.5.1.4"}).
-define(cltNetworkUnitHardwareVersion,		{hardware_version,	"1.3.6.1.4.1.36368.1.1.1.1.3.3.5.1.14"}).
-define(cltNetworkUnitSoftwareVersion,      {software_version,	"1.3.6.1.4.1.36368.1.1.1.1.3.3.5.1.15"}).
-define(CnuTable, [
		?cltNetworkUnitOnlineStatus,
		?cltNetworkUnitMACAddress,
		?cltNetworkUnitDeviceModel,
        ?cltNetworkUnitHardwareVersion,
        ?cltNetworkUnitSoftwareVersion
        ]).


%cnu port
-define(cltNetworkUnitIfIndex,      {index,     	"1.3.6.1.4.1.36368.1.1.1.1.3.3.6.1.2"}).
-define(cltNetworkUnitIfRXPackets,  {ifInOctets,	"1.3.6.1.4.1.36368.1.1.1.1.3.3.6.1.2"}).
-define(cltNetworkUnitIfTXPackets,  {ifOutOctets,	"1.3.6.1.4.1.36368.1.1.1.1.3.3.6.1.2"}).
-define(CnuPort, [
		?cltNetworkUnitIfRXPackets,
		?cltNetworkUnitIfTXPackets
        ]).