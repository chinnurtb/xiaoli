%clt
-define(apIpSubnetMask,	            {mask,	            "1.3.6.1.4.1.2863.1.1.1.2.4"}).
-define(apMAC,		                {mac,	            "1.3.6.1.4.1.2863.1.1.1.2.1"}).
%-define(apIpAddress,	            {ip,            	"1.3.6.1.4.1.2863.1.1.1.2.3"}).

-define(apModelType,		        {type,	            "1.3.6.1.4.1.2863.1.1.1.8.1"}).
-define(apBootloaderVersion,		{hardware_version,	"1.3.6.1.4.1.2863.1.1.1.8.4"}).
-define(apCurrentFwVersion,         {software_version,	"1.3.6.1.4.1.2863.1.1.1.8.5"}).
-define(apCurrentSecurityMode,      {oper_mode,	        "1.3.6.1.4.1.2863.1.1.1.11.8"}).
-define(apCurrentChannel,           {radio_channel,	    "1.3.6.1.4.1.2863.1.1.1.11.3"}).
-define(apMaxModemNumber,           {cnu_num,	        "1.3.6.1.4.1.2863.1.1.1.11.1"}).


-define(inOctets,                   {ifInOctets,	        "1.3.6.1.4.1.2863.1.1.1.13.1"}).
-define(outOctets,                  {ifOutOctets,	        "1.3.6.1.4.1.2863.1.1.1.13.6"}).



-define(CltTable, [
		?apIpSubnetMask,
		?apMAC,
		?apModelType,
        ?apBootloaderVersion,
        ?apCurrentFwVersion,
        ?apCurrentSecurityMode,
        ?apCurrentChannel,
        ?apMaxModemNumber
        ]).

%cnu 索引为mac
-define(modemCurrentState,	        {cnu_status,	    "1.3.6.1.4.1.2863.1.1.2.1.1.3"}).
-define(modemMacAddress,		    {mac,	            "1.3.6.1.4.1.2863.1.1.2.1.1.1"}).
-define(modemModelType,		        {type,	            "1.3.6.1.4.1.2863.1.1.2.6.1.1"}).
-define(modemBootloaderVersion,		{hardware_version,	"1.3.6.1.4.1.2863.1.1.2.6.1.4"}).
-define(modemCurrentFwVersion,      {software_version,	"1.3.6.1.4.1.2863.1.1.2.6.1.5"}).
-define(modemVlanEnable,            {vlan_enable,	    "1.3.6.1.4.1.2863.1.1.2.4.1.6"}).




-define(CnuTable, [
		?modemCurrentState,
		?modemModelType,
        ?modemBootloaderVersion,
        ?modemCurrentFwVersion,
        ?modemVlanEnable
        ]).


%cnu port
-define(modemPortVlanId,            {cvlan,     	"1.3.6.1.4.1.2863.1.1.2.7.1.2"}).
-define(modemPortUplinkBandwidth,   {upassuredbw,	"1.3.6.1.4.1.2863.1.1.2.7.1.4"}).
-define(modemDownlinkBandwidth,     {downassuredbw,	"1.3.6.1.4.1.2863.1.1.2.7.1.5"}).
-define(modemPortAdminStatus,       {vlan_enable,	"1.3.6.1.4.1.2863.1.1.2.7.1.7"}).
-define(CnuPort, [
		?modemPortVlanId,
		?modemPortUplinkBandwidth,
		?modemDownlinkBandwidth,
		?modemPortAdminStatus
        ]).


-define(modemEthInOctets,   {ifInOctets,	"1.3.6.1.4.1.2863.1.1.2.9.1.1"}).
-define(modemEthOutOctets,  {ifOutOctets,   "1.3.6.1.4.1.2863.1.1.2.9.1.6"}).
