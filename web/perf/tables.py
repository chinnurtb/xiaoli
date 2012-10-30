# coding: utf-8

from tango.ui import tables as t
from .models import NodePerf, PortPerf
from .models import Threshold, Metric

__all__ = ['CpuMemTable',
           'PingTable',
           'PortUsageTable',
           'NodePerfTable',
           'PortPerfTable',
           'ThresholdTable',
           'MetricTable']

class NodeMixin(t.Table):
    node_alias      = t.Column(verbose_name=u'节点', accessor='node.alias')
    sampletime      = t.Column(verbose_name=u'时间')

    class Meta():
        model = NodePerf

class CpuMemTable(NodeMixin):

    helpdoc = '''CPU内存占用'''

    cpuavg      = t.Column(verbose_name=u'CPU利用率(均值)')
    cpumax      = t.Column(verbose_name=u'CPU利用率(最大)')
    tempavg     = t.Column(verbose_name=u'温度(均值)')
    tempmax     = t.Column(verbose_name=u'温度(最大)')
    powerstate  = t.Column(verbose_name=u'电源状态')
    fanstate    = t.Column(verbose_name=u'风扇状态')

    class Meta():
        model = NodePerf

class PingTable(t.Table):

    node_alias      = t.Column(verbose_name=u'节点', accessor='node.alias')
    sampletime      = t.Column(verbose_name=u'时间')
    pingrta         = t.Column(verbose_name=u'平均时延', orderable=True)
    pingrtmax       = t.Column(verbose_name=u'最大时延', orderable=True)
    pingrtmin       = t.Column(verbose_name=u'最小时延', orderable=True)
    pingloss        = t.Column(verbose_name=u'丢包率', orderable=True)
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

    inoctets        = t.Column(verbose_name=u'接收速率(均值)')
    inoctetsmax     = t.Column(verbose_name=u'接收速率(峰值)')
    inpkts          = t.Column(verbose_name=u'接收包数(均值)')
    inpktsmax       = t.Column(verbose_name=u'接收包数(峰值)')
    inucastpkts     = t.Column(verbose_name=u'单播包数')

    outoctets       = t.Column(verbose_name=u'发送速率(均值)')
    outoctetsmax    = t.Column(verbose_name=u'发送速率(峰值)')
    outpkts         = t.Column(verbose_name=u'发送包数(均值)')
    outpktsmax      = t.Column(verbose_name=u'发送包数(峰值)')
    outucastpkts    = t.Column(verbose_name=u'单播包数')
    
    class Meta():
        model = PortPerf

class ThresholdTable(t.Table):
    helpdoc = u'阀值设置'
    edit    = t.Action(name=u'编辑', endpoint='perf.thresholds_edit')
    check   = t.CheckBoxColumn()

    alias         = t.Column(verbose_name=u'阀值')
    summary       = t.Column(verbose_name=u'摘要')
    category_name = t.Column(verbose_name=u'设备', accessor='category.alias')
    
    occur_cond1   = t.Column(verbose_name=u'一级产生条件')
    restore_cond1 = t.Column(verbose_name=u'一级恢复条件')
    severity1     = t.Column(verbose_name=u'一级告警条件')
    
    occur_cond2   = t.Column(verbose_name=u'二级产生条件')
    restore_cond2 = t.Column(verbose_name=u'二级恢复条件')
    severity2     = t.Column(verbose_name=u'二级告警条件')
    enabled       = t.EnumColumn('is_valid', verbose_name=u'是否有效', enums={0: u'否', 1: u'是'})

    class Meta():
        model = Threshold

class MetricTable(t.Table):
    helpdoc = u'指标管理'
    edit    = t.Action(name=u'编辑', endpoint='perf.metrics_edit')
    check   = t.CheckBoxColumn()
    
    grp    = t.Column(verbose_name=u'分组')
    name   = t.Column(verbose_name=u'名称')
    alias  = t.Column(verbose_name=u'显示名')
    calc   = t.Column(verbose_name=u'计算方法')
    unit   = t.Column(verbose_name=u'单位')
    format = t.Column(verbose_name=u'格式')
    descr  = t.Column(verbose_name=u'说明')
    
    class Meta():
        model = Metric
