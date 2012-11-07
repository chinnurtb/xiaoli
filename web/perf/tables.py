# coding: utf-8

from tango.ui import tables as t
from .models import *

__all__ = ['CpuMemTable',
           'PingTable',
           
           'IntfUsageTable',
           # 'IntfUsageOnuTable',
           
           'IntfTrafficTable',
           'IntfTrafficOctetsTable',
           
           'PonUsageTable',
           'PonPowerTable']


# ==============================================================================
#  Node
# ==============================================================================
class NodeMixin(object):
    node_alias      = t.Column(u'节点', accessor='node.alias')
    sampletime      = t.Column(u'时间', orderable=True)

    
class PingTable(t.Table, NodeMixin):
    helpdoc = u'PING时延'
    
    rtavg    = t.Column(u'平均时延(ms)', orderable=True)
    rtmax    = t.Column(u'最大时延(ms)', orderable=True)
    rtmin    = t.Column(u'最小时延(ms)', orderable=True)
    lossavg  = t.Column(u'平均丢包率(%)', orderable=True)
    lossmax  = t.Column(u'最大丢包率(%)', orderable=True)
    stdevavg = t.Column(u'平均抖动率', orderable=True)
    stdevmax = t.Column(u'最大抖动率', orderable=True)
    
    class Meta():
        model = PingPerf
    
        
class CpuMemTable(t.Table, NodeMixin):
    helpdoc = u"CPU/内存"

    cpu        = t.Column(u'CPU利用率', orderable=True, subcolumns=[(u'均值', 'cpuavg'), (u'峰值', 'cpumax')])
    mem        = t.Column(u'内存', orderable=True, subcolumns=[(u'均值', 'memavg'), (u'峰值', 'memmax')])
    temp       = t.Column(u'温度', orderable=True, subcolumns=[(u'均值', 'tempavg'), (u'峰值', 'tempmax')])
    powerstate = t.Column(u'电源状态', orderable=True)
    fanstate   = t.Column(u'风扇状态')

    class Meta():
        model = CpuMemPerf

        
class BoardTable(t.Table, NodeMixin):
    helpdoc = u'板卡'
    
    boardtype = t.Column('板卡类型', )
    cpu       = t.Column(u'CPU利用率', subcolumns=[(u'均值', 'cpuavg'), (u'峰值', 'cpumax')])
    mem       = t.Column(u'内存', subcolumns=[(u'均值', 'memavg'), (u'峰值', 'memmax')])
    temp      = t.Column(u'温度', subcolumns=[(u'均值', 'tempavg'), (u'峰值', 'tempmax')])
    
    class Meta():
        model = BoardPerf


# ==============================================================================
#  IntfUsage
# ==============================================================================        
class IntfUsageTable(t.Table, NodeMixin):
    helpdoc = u'端口占用'
    
    intftotal      = t.Column(u'端口总数')
    intfused       = t.Column(u'端口使用数')
    intfusage      = t.Column(u'端口占用率')   
    intfidle       = t.Column(u'端空空闲率') 

    class Meta():
        model = IntfUsagePerf


# ==============================================================================
#  Traffic 
# ==============================================================================
class IntfTrafficTable(t.Table, NodeMixin):
    helpdoc = u'端口流量'

    node_alias      = t.Column(u'节点', accessor='node.alias')
    intf_alias      = t.Column(u'端口', accessor='intf.alias')
    sampletime      = t.Column(u'时间')

    inoctets        = t.Column(u'接收速率', subcolumns=[(u'均值', 'inoctets'), (u'峰值', 'inoctetsmax')])
    inpkts          = t.Column(u'接收包数', subcolumns=[(u'均值', 'inpkts'), (u'峰值', 'inpktsmax')])
    inucastpkts     = t.Column(u'单播包数')

    outoctets       = t.Column(u'发送速率', subcolumns=[(u'均值', 'outoctets'), (u'峰值', 'outoctetsmax')])
    outpkts         = t.Column(u'发送包数', subcolumns=[(u'均值', 'outpkts'), (u'峰值', 'outpktsmax')])
    outucastpkts    = t.Column(u'单播包数')
    
    class Meta():
        model = IntfTrafficPerf

        
class IntfTrafficOctetsTable(t.Table):
    """流量流速"""
    
    octetstotal = t.Column(u'总流量')
    bwusage = t.Column(u'总带宽利用率', subcolumns=[(u'均值', 'bwusage'), (u'峰值', 'bwusagemax')])
    inoctetstotal = t.Column(u'接收流量')
    inbwusage = t.Column(u'接收带宽利用率', subcolumns=[(u'均值', 'inbwusage'), (u'峰值', 'inbwusagemax')])
    outoctetstotal = t.Column(u'发送流量')
    outbwusage = t.Column(u'发送带宽利用率', subcolumns=[(u'均值', 'outbwusage'), (u'峰值', 'outbwusagemax')])

    octets = t.Column(u'总速率', subcolumns=[(u'均值', 'octets'), (u'峰值', 'octetsmax')])
    inoctets = t.Column(u'总发送速率', subcolumns=[(u'均值', 'inoctets'), (u'峰值', 'inoctetsmax')])
    outoctets = t.Column(u'总接收速率', subcolumns=[(u'均值', 'outoctets'), (u'峰值', 'outoctetsmax')])
    
    class Meta():
        model = IntfTrafficPerf

        
# ==============================================================================
#  Pon
# ==============================================================================
class PonUsageTable(t.Table):
    helpdoc = u'PON口占用率'
    
    pontotal = t.Column(u'PON口总数')
    ponused  = t.Column(u'PON口使用数')
    ponusage = t.Column(u'PON口占用率')
    ponfree  = t.Column(u'PON口空闲率')
    
    class Meta():
        model = PonUsagePerf
        
        
class PonPowerTable(t.Table):
    helpdoc         = u'PON口光功率'

    node_alias      = t.Column(u'节点', accessor='node.alias')
    intf_alias      = t.Column(u'PON口', accessor='intf.alias')
    sampletime      = t.Column(u'时间')

    #发送光功率(均值、最大、最小）Output OpticalPower  发送光接口（dbm）
    txpower = t.Column(u'发送光功率(dbm)', orderable=True,
                       subcolumns=[(u'均值', 'txpoweravg'), (u'峰值', 'txpowermax')])
    #接收光功率(均值、最大、最小） Input OpticalPower  接收光功率（dbm）
    rxpower = t.Column(u'接收光功率(dbm)', subcolumns=[(u'均值', 'rxpoweravg'), (u'峰值', 'rxpowermax')])
    
    #偏置电流(均值、最大、最小）Bias Currency  光模块偏置电流(mA)
    biascurrent = t.Column(u'偏置电流(mA)', subcolumns=[(u'均值', 'biascurrentavg'), (u'峰值', 'biascurrentmax')])
    
    #供电电压(均值、最大、最小） Supply Voltage 供电电压(V)
    supplyvolage = t.Column(u'供电电压(V)', subcolumns=[(u'均值', 'supplyvolageavg'), (u'峰值', 'supplyvolagemax')])

    #温度(均值、最大、最小）
    pontemp = t.Column(u'温度', subcolumns=[(u'均值', 'pontempavg'), (u'峰值', 'pontempmax')])

    class Meta():
        model = PonPowerPerf


