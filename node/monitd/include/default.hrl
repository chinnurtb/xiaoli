-define(DEFAULT,[
{assoc,[                                 %=AP性能:连接信息
  {assocNum,                 0},         %counter%   关联总次数
  {assocSuccNum,             0},         %counter%   关联总次数-关联失败总次数
  {reAssocNum,               0},         %counter%   重新关联总次数
  {reAssocSuccNum,           0},         %counter%   重新关联总次数 - 重新关联失败总次数(没有失败总次数则等于关联总次数)
  {assocRefusedNum,          0},         %counter%   由于接入点资源有限而拒绝关联的总次数 
  {deauthNum,                0},         %counter%   终端异常断开连接的总次数 
  {apStationAssocSum,        0},         %guage  %   当前与AP关联的终端数
  {apStationOnlineSum,       0},         %guage  %   当前AP下在线的终端数
  {cpuRTUsage,               0},         %guage  %   Ap CPU实时利用率
  {memRTUsage,               0}          %guage  %   Ap 内存实时利用率

]},
{wire,[                                  %=AP性能:有线接口
  {ifInOctets,               0},         %counter%   端口接收的总字节数
  {ifOutOctets,              0},         %counter%   端口发送的总字节数
  {ifInNUcastPkts,           0},         %counter%   端口接收非单播包数
  {ifOutNUcastPkts,          0},         %counter%   端口发送非单播包数
  {ifInUcastPkts,            0},         %counter%   端口接收单播包数
  {ifOutUcastPkts,           0},         %counter%   端口发送单播包数
  {ifInDiscards,             0},         %counter%   端口丢弃接收到的包数
  {ifOutDiscards,            0},         %counter%   端口丢弃要发送的包数
  {ifInErrors,               0},         %counter%   端口接收到的错误包数
  {ifOutErrors,              0}          %counter%   端口发送错误的包数
]},
{wireless,[                              %=AP性能:无线接口
  {ifInOctets,               0},         %counter%   接收的数据包字节数
  {ifOutOctets,              0},         %counter%   发送的数据字包节数
  {ifInPkts,                 0},         %counter%   接收的数据包数
  {ifOutPkts,                0},         %counter%   发送的数据包数
  {ifInErrors,               0},         %counter%   信道接收的总错包数 
  {ifInAvgSignal,            0},         %guage  %   接收的信号平均强度
  {ifInHighSignal,           0},         %guage  %   接收的最高信号强度
  {ifInLowSignal,            0},         %guage  %   接收的最低信号强度
  {ifFrameRetryRate,         0}          %guage  %   信道上帧的重传率
  ]},
{acinfo, [                               %=AC性能
  {cpuRTUsage,               0},         %guage  %   CPU实时利用率
  {memRTUsage,               0},         %guage  %   内存实时利用率
  {flashMemRTUsage,          0},         %guage  %   闪存实时利用率
  {dHCPReqTimes,             0},         %counter%   DHCP请求次数
  {dHCPReqSucTimes,          0},         %counter%   DHCP请求成功数
  {bandWidth,                0},         %guage  %   带宽
  {upOctets,                 0},         %counter%   AC接收总字节数:端口接收的总字节数(ifInOctets) 之和
  {downOctets,               0},         %counter%   AC发送总字节数:端口发送的总字节数(ifOutOctets) 之和
  {discardPkts,              0},         %counter%   AC丢弃包数    :端口丢弃接收到的包数(ifInDiscards),端口丢弃要发送的包数(ifOutDiscards) 之和
  {upInPkts,                 0},         %counter%   AC接收总包数  :端口接收非单播包数(ifInNUcastPkts),端口接收单播包数(ifInUcastPkts) 之和
  {upOutPkts,                0},         %counter%   AC发送总包数  :端口发送非单播包数(ifOutNUcastPkts),端口发送单播包数(ifOutUcastPkts) 之和
  {onlineNum,                0},         %guage  %   当前所有在线终端数
  {authNum,                  0},         %counter%   用户认证总次数 
  {maxNum,                   0},         %guage  %   峰值在线用户数
  {normalNum,                0},         %counter%   正常断开次数
  {deauthNum,                0},         %counter%   异常断开次数
  {authReqNum,               0},         %counter%   认证请求数
  {authSucNum,               0},         %counter%   认证成功数
  {accReqNum,                0},         %counter%   计费请求数
  {accSucNum,                0},         %counter%   计费成功数
  {radiusReqPkts,            0},         %guage  %   AC发送给RADIUS服务器的认证请求包数
  {radiusRepPkts,            0},         %guage  %   AC接收到来自RADIUS服务器的响应包数
  {leaveReqPkts,             0},         %guage  %   AC发送给RADIUS服务器的下线请求包数
  {leaveRepPkts,             0},         %guage  %   AC接收到来自RADIUS服务器的下线响应包数
  {radiusAvgDelay,           0},         %guage  %   AC Radius平均时延
  {portalChallengeReqCount,  0},         %counter%   AC收到Portal服务器的Challenge请求数(Portal请求CHALLENGE数)
  {portalChallengeRespCount, 0},         %counter%   AC响应Portal服务器Challenge请求的次数(Portal请求CHALLENGE成功数)
  {portalAuthReqCount,       0},         %counter%   AC收到Portal服务器的鉴权请求数(用户上线请求数) 
  {portalAuthRespCount,      0},         %counter%   AC响应Portal服务器鉴权请求的次数(用户上线成功数)
  {leaveReqCount,            0},         %counter%   AC下线请求次数(用户下线请求数)
  {leaveRepCount,            0},         %counter%   AC下线响应次数(用户下线成功数)
  {addressCount,             0},         %guage  %   当前使用地址数*
  {dHCPIpPoolUsage,          0}          %guage  %   DHCP地址利用率
  ]},
{acintf,[                                %=AC接口性能
  {ifInOctets,               0},         %counter%   端口接收的总字节数 
  {ifOutOctets,              0},         %counter%   端口发送的总字节数
  {ifInNUcastPkts,           0},         %counter%   端口接收非单播包数
  {ifOutNUcastPkts,          0},         %counter%   端口发送非单播包数
  {ifInUcastPkts,            0},         %counter%   端口接收单播包数
  {ifOutUcastPkts,           0},         %counter%   端口发送单播包数
  {ifInDiscards,             0},         %counter%   端口丢弃接收到的包数 
  {ifOutDiscards,            0},         %counter%   端口丢弃要发送的包数
  {ifInErrors,               0},         %counter%   端口接收到的错误包数
  {ifOutErrors,              0},         %counter%   端口发送错误的包数
  {ifUpDwnTimes,             0}          %counter%   端口updown次数
]},
{ssid,[
  {ifInOctets,               0},         %counter%   接收的数据包字节数
  {ifOutOctets,              0},         %counter%   发送的数据包字节数   
  {ifInPkts,                 0},         %counter%   接收的数据包数
  {ifOutPkts,                0}          %counter%   发送的数据包数
]}

]).


