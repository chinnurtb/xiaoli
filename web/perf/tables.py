# coding: utf-8

from tango.ui import tables as t
from .models import NodePerf, PortPerf

__all__ = ['CpuMemTable',
           'PingTable',
           'PortUsageTable',
           'NodePerfTable',
           'PortPerfTable',
           'PonPowerTable']

class NodeMixin(t.Table):
    node_alias      = t.Column(verbose_name=u'节点', accessor='node.alias')
    sampletime      = t.Column(verbose_name=u'时间')

    class Meta():
        model = NodePerf

class CpuMemTable(NodeMixin):

    helpdoc = u"CPU内存占用"

    cpu_use    = t.Column(verbose_name=u'CPU利用率', subcolumns=[(u'均值', 'cpuavg'), (u'峰值', 'cpumax')])
    cpuavg     = t.Column(verbose_name=u'CPU利用率(均值)')
    cpumax     = t.Column(verbose_name=u'CPU利用率(峰值)')
    tempavg    = t.Column(verbose_name=u'温度(均值)')
    tempmax    = t.Column(verbose_name=u'温度(峰值)')
    powerstate = t.Column(verbose_name=u'电源状态')
    fanstate   = t.Column(verbose_name=u'风扇状态')

    class Meta():
        model = NodePerf

class PingTable(t.Table):

    node_alias      = t.Column(verbose_name=u'节点', accessor='node.alias')
    sampletime      = t.Column(verbose_name=u'时间')
    pingrta         = t.Column(verbose_name=u'平均时延(ms)', orderable=True)
    pingrtmax       = t.Column(verbose_name=u'最大时延(ms)', orderable=True)
    pingrtmin       = t.Column(verbose_name=u'最小时延(ms)', orderable=True)
    pingloss        = t.Column(verbose_name=u'丢包率(%)', orderable=True)
    pingstdev       = t.Column(verbose_name=u'抖动率', orderable=True)
    
    class Meta():
        model = NodePerf

class PortUsageTable(NodeMixin):
    porttotal      = t.Column(verbose_name=u'端口总数')
    portused       = t.Column(verbose_name=u'端口使用数')
    portusage      = t.Column(verbose_name=u'端口占用率')   
    portidle       = t.Column(verbose_name=u'端空空闲率') 

    class Meta():
        model = NodePerf

class NodePerfTable(t.Table):

    helpdoc = u'节点性能'
    node_alias      = t.Column(verbose_name=u'节点', accessor='node.alias')
    sampletime      = t.Column(verbose_name=u'时间')
    pingrta         = t.Column(verbose_name=u'平均时延', orderable=True)
    pingrtmax       = t.Column(verbose_name=u'最大时延', orderable=True)
    pingrtmin       = t.Column(verbose_name=u'最小时延', orderable=True)
    
    class Meta():
        model = NodePerf
        #group_by = 'node.alias'

class PortPerfTable(t.Table):

    helpdoc         = u'端口性能'

    node_alias      = t.Column(verbose_name=u'节点', accessor='node.alias')
    port_alias      = t.Column(verbose_name=u'端口', accessor='port.alias')
    sampletime      = t.Column(verbose_name=u'时间')

    inoctets        = t.Column(verbose_name=u'接收速率(均值/峰值)')
    #inoctetsmax     = t.Column(verbose_name=u'接收速率(峰值)')

    inpkts          = t.Column(verbose_name=u'接收包数(均值/峰值)')
    #inpktsmax       = t.Column(verbose_name=u'接收包数(峰值)')
    inucastpkts     = t.Column(verbose_name=u'单播包数')

    outoctets       = t.Column(verbose_name=u'发送速率(均值/峰值)')
    #outoctetsmax    = t.Column(verbose_name=u'发送速率(峰值)')
    outpkts         = t.Column(verbose_name=u'发送包数(均值/峰值)')
    #outpktsmax      = t.Column(verbose_name=u'发送包数(峰值)')
    outucastpkts    = t.Column(verbose_name=u'单播包数')
    
    class Meta():
        model = PortPerf

class PonPowerTable(t.Table):
    helpdoc         = u'端口性能'

    node_alias      = t.Column(verbose_name=u'节点', accessor='node.alias')
    port_alias      = t.Column(verbose_name=u'PON口', accessor='port.alias')
    sampletime      = t.Column(verbose_name=u'时间')

    #发送光功率(均值、最大、最小）Output OpticalPower  发送光接口（dbm）
    txpoweravg      = t.Column(verbose_name=u'发送光功率(均值dbm)')
    txpowermax      = t.Column(verbose_name=u'发送光功率(峰值dbm)')

    #接收光功率(均值、最大、最小） Input OpticalPower  接收光功率（dbm）
    rxpoweravg      = t.Column(verbose_name=u'接收光功率(均值dbm)')
    rxpowermax      = t.Column(verbose_name=u'接收光功率(峰值dbm)')
    
    #偏置电流(均值、最大、最小）Bias Currency  光模块偏置电流(mA)
    biascurrentavg = t.Column(verbose_name=u'偏置电流(均值mA)')
    biascurrentmax = t.Column(verbose_name=u'偏置电流(峰值mA)')
    
    #供电电压(均值、最大、最小） Supply Voltage 供电电压(V)
    supplyvolageavg = t.Column(verbose_name=u'供电电压(均值V)')
    supplyvolagemax = t.Column(verbose_name=u'供电电压(峰值V)')

    #温度(均值、最大、最小）
    pontempavg      = t.Column(verbose_name=u'温度(均值)')
    pontempmax      = t.Column(verbose_name=u'温度(峰值)')

    class Meta():
        model = PortPerf
    
