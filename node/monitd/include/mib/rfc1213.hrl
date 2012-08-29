%system group
-define(SysDescr,    {sysDescr,    [1,3,6,1,2,1,1,1,0]}).
-define(SysObjectID, {sysObjectID, [1,3,6,1,2,1,1,2,0]}).
-define(SysUpTime,   {sysUpTime,   [1,3,6,1,2,1,1,3,0]}).
-define(SysContact,  {sysContact,  [1,3,6,1,2,1,1,4,0]}).
-define(SysName,     {sysName,     [1,3,6,1,2,1,1,5,0]}).
-define(SysLocation, {sysLocation, [1,3,6,1,2,1,1,6,0]}).
-define(System, [?SysDescr, ?SysObjectID, ?SysName]). 

-define(IfIndex,        {ifIndex,        [1,3,6,1,2,1,2,2,1,1]}).
-define(IfDescr,        {ifDescr,        [1,3,6,1,2,1,2,2,1,2]}).
-define(IfType,         {ifType,         [1,3,6,1,2,1,2,2,1,3]}).
-define(IfMtu,          {ifMtu,          [1,3,6,1,2,1,2,2,1,4]}).
-define(IfSpeed,        {ifSpeed,        [1,3,6,1,2,1,2,2,1,5]}).
-define(IfPhysAddress,  {ifPhysAddress,  [1,3,6,1,2,1,2,2,1,6]}).
-define(IfAdminStatus,  {ifAdminStatus,  [1,3,6,1,2,1,2,2,1,7]}).
-define(IfOperStatus,   {ifOperStatus,   [1,3,6,1,2,1,2,2,1,8]}).

-define(IfInOctets,     {ifInOctets,     "1.3.6.1.2.1.2.2.1.10"}).
-define(IfInUcastPkts,  {ifInUcastPkts,  "1.3.6.1.2.1.2.2.1.11"}).
-define(IfInNUcastPkts, {ifInNUcastPkts, "1.3.6.1.2.1.2.2.1.12"}).
-define(IfInDiscards,   {ifInDiscards,   "1.3.6.1.2.1.2.2.1.13"}).
-define(IfInErrors,     {ifInErrors,     "1.3.6.1.2.1.2.2.1.14"}).
-define(IfOutOctets,    {ifOutOctets,    "1.3.6.1.2.1.2.2.1.16"}).
-define(IfOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.2.1.2.2.1.17"}).
-define(IfOutNUcastPkts,{ifOutNUcastPkts,"1.3.6.1.2.1.2.2.1.18"}).
-define(IfOutDiscards,  {ifOutDiscards,  "1.3.6.1.2.1.2.2.1.19"}).
-define(IfOutErrors,    {ifOutErrors,    "1.3.6.1.2.1.2.2.1.20"}).

-define(IfEntry, [
     ?IfIndex, 
     ?IfDescr, 
     ?IfType, 
     ?IfMtu, 
     ?IfSpeed, 
     ?IfPhysAddress, 
     ?IfAdminStatus, 
     ?IfOperStatus
]).

%iftable
-define(IfColumns, [
     ?IfDescr,
     ?IfInOctets,     
     ?IfInUcastPkts,  
     ?IfInNUcastPkts, 
     ?IfInDiscards,   
     ?IfInErrors,     

     ?IfOutOctets,    
     ?IfOutUcastPkts, 
     ?IfOutNUcastPkts,
     ?IfOutDiscards,  
     ?IfOutErrors    
]).

-define(IfName,           {ifName,         "1.3.6.1.2.1.31.1.1.1.1" }).
-define(IfHCInOctets,     {ifInOctets,     "1.3.6.1.2.1.31.1.1.1.6" }).
-define(IfHCInUcastPkts,  {ifInUcastPkts,  "1.3.6.1.2.1.31.1.1.1.7" }).
-define(IfHCOutOctets,    {ifOutOctets,    "1.3.6.1.2.1.31.1.1.1.10"}).
-define(IfHCOutUcastPkts, {ifOutUcastPkts, "1.3.6.1.2.1.31.1.1.1.11"}).
%ifxtable
-define(IfXColumns, [
     ?IfDescr,
     ?IfHCInOctets, 
     ?IfInDiscards, 
     ?IfInUcastPkts, 
     ?IfInNUcastPkts,
     ?IfInErrors, 

     ?IfHCOutOctets, 
     ?IfOutUcastPkts, 
     ?IfOutNUcastPkts,
     ?IfOutDiscards, 
     ?IfOutErrors
]).
 

