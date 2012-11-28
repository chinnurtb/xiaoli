%clt 索引为mac地址
-define(systemMask,			    {mask,	            "1.3.6.1.4.1.17409.2.1.1.1.1.2.6"}).
-define(systemIp,		        {ip,	            "1.3.6.1.4.1.17409.2.1.1.1.1.3.6"}).
-define(systemType,		        {type,	            "1.3.6.1.4.1.17409.2.1.1.1.1.18.6"}).
-define(systemHwVersion,		{hardware_version,	"1.3.6.1.4.1.17409.2.1.1.1.1.5.6"}).
-define(systemSwVersion,        {software_version,	"1.3.6.1.4.1.17409.2.1.1.1.1.8.6"}).
-define(systemOperMode,         {oper_mode,	        "1.3.6.1.4.1.17409.2.3.1.1.1.3.6"}).

-define(CltTable, [
		?systemMask,
		?systemIp,
		?systemType,
        ?systemHwVersion,
        ?systemSwVersion,
        ?systemOperMode
        ]).



%cnu 索引为mac地址
-define(slaveState,		{cnu_status,	"1.3.6.1.4.1.17409.2.4.1.1.1.4.6"}).
-define(slaveType,		{type,	        "1.3.6.1.4.1.17409.2.4.1.1.1.3.6"}).


-define(CnuTable, [?slaveState,?slaveType]).

%cnu port 索引为mac地址
-define(eocPortUpLimitSpeed,		{upassuredbw,	"1.3.6.1.4.1.17409.2.4.1.4.1.7.6"}).
-define(eocPortDownLimitSpeed,		{downassuredbw,	"1.3.6.1.4.1.17409.2.4.1.4.1.8.6"}).
-define(eocPortState,		        {oper_status,	"1.3.6.1.4.1.17409.2.4.1.4.1.3.6"}).
-define(eocPortVlanId,              {cvlan,     	"1.3.6.1.4.1.17409.2.4.1.4.1.5.6"}).

-define(CnuPort, [
		?eocPortUpLimitSpeed,
		?eocPortDownLimitSpeed,
		?eocPortState,
		?eocPortVlanId
        ]).