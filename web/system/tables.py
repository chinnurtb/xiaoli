# coding: utf-8

from tango.ui import tables
from tango.ui.tables.utils import Attrs
from tango.models import Setting, DictCode

from nodes import constants

from .models import  Metric, Threshold

from .models import OperationLog, SecurityLog, SubSystem, TimePeriod

class SettingTable(tables.Table):
    helpdoc = u'查看系统设置参数'
    edit    = tables.Action(name=u'编辑', endpoint='system.settings_edit')
    check   = tables.CheckBoxColumn()

    name  = tables.Column(verbose_name=u'参数名', orderable=True)
    alias = tables.Column(verbose_name=u'参数别名', orderable=True)
    value = tables.Column(verbose_name=u'参数值')
    unit  = tables.Column(verbose_name=u'参数单位')

    class Meta():
        model = Setting


class DictCodeTable(tables.Table):
    helpdoc = u'查看系统字典'
    edit    = tables.Action(name=u'编辑', endpoint='system.dict_codes_edit')
    check   = tables.CheckBoxColumn()
 
    type_label = tables.Column(verbose_name=u'字典类型', orderable=True, accessor='type.type_label')
    code_label = tables.Column(verbose_name=u'字典值', orderable=True)
    is_valid   = tables.EnumColumn('is_valid', verbose_name=u'是否有效', enums={0: u'否', 1: u'是'})

    class Meta():
        model = DictCode

class ThresholdTable(tables.Table):
    helpdoc = u'阀值设置'
    edit    = tables.Action(name=u'编辑', endpoint='system.thresholds_edit')
    check   = tables.CheckBoxColumn()

    alias         = tables.Column(verbose_name=u'阀值')
    summary       = tables.Column(verbose_name=u'摘要')
    category_name = tables.Column(verbose_name=u'设备', accessor='category.alias')
    
    occur_cond1   = tables.Column(verbose_name=u'一级产生条件')
    restore_cond1 = tables.Column(verbose_name=u'一级恢复条件')
    severity1     = tables.Column(verbose_name=u'一级告警条件')
    
    occur_cond2   = tables.Column(verbose_name=u'二级产生条件')
    restore_cond2 = tables.Column(verbose_name=u'二级恢复条件')
    severity2     = tables.Column(verbose_name=u'二级告警条件')
    enabled       = tables.EnumColumn('is_valid', verbose_name=u'是否有效', enums={0: u'否', 1: u'是'})

    class Meta():
        model = Threshold

class MetricTable(tables.Table):
    helpdoc = u'指标管理'
    edit    = tables.Action(name=u'编辑', endpoint='system.metrics_edit')
    delete  = tables.Action(name=u'删除', endpoint='system.metrics_delete', attrs=Attrs(a={"class": "delete-btn"}))
    check   = tables.CheckBoxColumn()
    
    grp    = tables.Column(verbose_name=u'分组')
    name   = tables.LinkColumn(verbose_name=u'名称', endpoint='system.metrics_edit')
    alias  = tables.LinkColumn(verbose_name=u'显示名', endpoint='system.metrics_edit')
    calc   = tables.Column(verbose_name=u'计算方法')
    unit   = tables.Column(verbose_name=u'单位')
    format = tables.Column(verbose_name=u'格式')
    descr  = tables.Column(verbose_name=u'说明')
    
    class Meta():
        model = Metric
        group_by = 'grp'

class TimePeriodTable(tables.Table):
    helpdoc = u'采集规则'
    edit    = tables.Action(name=u'编辑', endpoint='system.timeperiods_edit')
    check   = tables.CheckBoxColumn()

    name       = tables.Column(verbose_name=u'名称')
    alias      = tables.Column(verbose_name=u'显示名')
    _hour       = tables.Column(verbose_name=u'小时')
    _dayofmonth = tables.Column(verbose_name=u'日期')
    _month      = tables.Column(verbose_name=u'月份')
    _dayofweek  = tables.Column(verbose_name=u'星期')
    status     = tables.Column(verbose_name=u'状态')
    created_at = tables.Column(verbose_name=u'创建日期')

    class Meta:
        model = TimePeriod
    
            
class OperationLogTable(tables.Table):
    helpdoc     = u'查看系统操作日志'

    uid            = tables.Column(verbose_name=u'用户名', orderable=True, accessor='user.username',
                                   attrs=tables.Attrs(th={'width': '10%'}))
    terminal_ip    = tables.Column(verbose_name=u'终端IP', orderable=True,
                                   attrs=tables.Attrs(th={'width': '15%'}))
    action         = tables.Column(verbose_name=u'操作', orderable=True,
                                   attrs=tables.Attrs(th={'width': '15%'}))
    oper_obj       = tables.Column(verbose_name=u'操作对象', orderable=True)
    created_at     = tables.DateTimeColumn(verbose_name=u'时间', orderable=True,format='%Y-%m-%d %H:%M:%S')

    class Meta():
        model = OperationLog
        order_by = '-created_at'
        

class SecurityLogTable(tables.Table):
    helpdoc     = u'查看系统安全日志'

    uid            = tables.Column(verbose_name=u'用户名', orderable=True, accessor='user.username')
    terminal_ip    = tables.Column(verbose_name=u'终端IP', orderable=True)
    summary        = tables.Column(verbose_name=u'操作', orderable=True)
    time           = tables.Column(verbose_name=u'时间')

    class Meta():
        model = SecurityLog
        order_by = '-id'
        
class SubSystemTable(tables.Table):
    helpdoc = u'查看所有子系统 (子采集)'

    name       = tables.Column(verbose_name=u'名称', orderable=True)
    alias      = tables.Column(verbose_name=u'显示名')
    host       = tables.Column(verbose_name=u'主机名', orderable=True)
    status     = tables.EnumColumn('state', verbose_name=u'状态', enums=constants.STATUS, orderable=True)
    descr      = tables.Column(verbose_name=u'描述')
    started_at = tables.DateTimeColumn(verbose_name=u'开始采集时间', orderable=True)

    class Meta():
        model = SubSystem
    
