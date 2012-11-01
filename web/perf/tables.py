# coding: utf-8

from tango.ui import tables as t
from .models import *

__all__ = ['NodePerfTable',
           'CpuMemTable',
           'PingTable',
           'IntfUsageTable',
           'IntfTrafficPerfTable',
           'PonPowerTable']

class NodeMixin():
    node_alias      = t.Column(u'节点', accessor='node.alias')
    sampletime      = t.Column(u'时间')

    
class PingTable(t.Table):

    node_alias = t.Column(u'节点', accessor='node.alias')
    sampletime = t.Column(u'时间')
    pingrta    = t.Column(u'平均时延(ms)', orderable=True)
    pingrtmax  = t.Column(u'最大时延(ms)', orderable=True)
    pingrtmin  = t.Column(u'最小时延(ms)', orderable=True)
    pingloss   = t.Column(u'丢包率(%)', orderable=True)
    pingstdev  = t.Column(u'抖动率', orderable=True)
    
    class Meta():
        model = PingPerf
    
class NodePerfTable(t.Table):

    node_alias = t.Column(u'节点', accessor='node.alias')
    sampletime = t.Column(u'时间')
    pingrta    = t.Column(u'平均时延(ms)', orderable=True)
    pingrtmax  = t.Column(u'最大时延(ms)', orderable=True)
    pingrtmin  = t.Column(u'最小时延(ms)', orderable=True)
    pingloss   = t.Column(u'丢包率(%)', orderable=True)
    pingstdev  = t.Column(u'抖动率', orderable=True)
    
    class Meta():
        model = PingPerf
        
class CpuMemTable(t.Table, NodeMixin):

    helpdoc = u"CPU内存占用"

    cpu  = t.Column(u'CPU利用率', subcolumns=[(u'均值', 'cpuavg'), (u'峰值', 'cpumax')])
    mem  = t.Column(u'内存', subcolumns=[(u'均值', 'memavg'), (u'峰值', 'memmax')])
    temp = t.Column(u'温度', subcolumns=[(u'均值', 'tempavg'), (u'峰值', 'tempmax')])
    powerstate = t.Column(u'电源状态')
    fanstate   = t.Column(u'风扇状态')

    class Meta():
        model = CpuMemPerf

class BoardTable(t.Table, NodeMixin):
    boardtype = t.Column('板卡类型', )
    cpu  = t.Column(u'CPU利用率', subcolumns=[(u'均值', 'cpuavg'), (u'峰值', 'cpumax')])
    mem  = t.Column(u'内存', subcolumns=[(u'均值', 'memavg'), (u'峰值', 'memmax')])
    temp = t.Column(u'温度', subcolumns=[(u'均值', 'tempavg'), (u'峰值', 'tempmax')])
    
    class Meta():
        model = BoardPerf
    
    
class IntfUsageTable(NodeMixin):
    intftotal      = t.Column(u'端口总数')
    intfused       = t.Column(u'端口使用数')
    intfusage      = t.Column(u'端口占用率')   
    intfidle       = t.Column(u'端空空闲率') 

    class Meta():
        model = IntfUsagePerf

        
class IntfTrafficPerfTable(t.Table):

    helpdoc         = u'端口性能'

    node_alias      = t.Column(u'节点', accessor='node.alias')
    intf_alias      = t.Column(u'端口', accessor='intf.alias')
    sampletime      = t.Column(u'时间')

    inoctets        = t.Column(u'接收速率(均值/峰值)')
    #inoctetsmax     = t.Column(u'接收速率(峰值)')

    inpkts          = t.Column(u'接收包数(均值/峰值)')
    #inpktsmax       = t.Column(u'接收包数(峰值)')
    inucastpkts     = t.Column(u'单播包数')

    outoctets       = t.Column(u'发送速率(均值/峰值)')
    #outoctetsmax    = t.Column(u'发送速率(峰值)')
    outpkts         = t.Column(u'发送包数(均值/峰值)')
    #outpktsmax      = t.Column(u'发送包数(峰值)')
    outucastpkts    = t.Column(u'单播包数')
    
    class Meta():
        model = IntfTrafficPerf

class PonPowerTable(t.Table):
    helpdoc         = u'端口性能'

    node_alias      = t.Column(u'节点', accessor='node.alias')
    intf_alias      = t.Column(u'PON口', accessor='intf.alias')
    sampletime      = t.Column(u'时间')

    #发送光功率(均值、最大、最小）Output OpticalPower  发送光接口（dbm）
    txpoweravg      = t.Column(u'发送光功率(均值dbm)')
    txpowermax      = t.Column(u'发送光功率(峰值dbm)')

    #接收光功率(均值、最大、最小） Input OpticalPower  接收光功率（dbm）
    rxpoweravg      = t.Column(u'接收光功率(均值dbm)')
    rxpowermax      = t.Column(u'接收光功率(峰值dbm)')
    
    #偏置电流(均值、最大、最小）Bias Currency  光模块偏置电流(mA)
    biascurrentavg = t.Column(u'偏置电流(均值mA)')
    biascurrentmax = t.Column(u'偏置电流(峰值mA)')
    
    #供电电压(均值、最大、最小） Supply Voltage 供电电压(V)
    supplyvolageavg = t.Column(u'供电电压(均值V)')
    supplyvolagemax = t.Column(u'供电电压(峰值V)')

    #温度(均值、最大、最小）
    pontempavg      = t.Column(u'温度(均值)')
    pontempmax      = t.Column(u'温度(峰值)')

    class Meta():
        model = PonPowerPerf
    
