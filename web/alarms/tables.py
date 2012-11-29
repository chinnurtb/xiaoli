# coding: utf-8

import constants

from jinja2 import Markup

from tango.ui import tables as t

from tango.models import Query

from .models import Alarm, AlarmSeverity, History, AlarmClass, AlarmKnowledge
from nodes.tables import redirect_node_show

class SeverityColumn(t.EnumColumn):

    def __init__(self, attrs=None, **extra):
        super(SeverityColumn, self).__init__(u'级别', name='severity', enums=constants.SEVERITIES)
    
    def render(self, value, record, bound_column):
        text = super(SeverityColumn, self).render(value, record, bound_column)
        return Markup('<span class="label severity-%d">%s</span>' % (value, text))

class AlarmAliasColumn(t.Column):
    
    def __init(self, attrs=None,**extra):
        super(AlarmAliasColumn, self).__init__(attrs, extra)

    def render(self, value, record, bound_column):
        return Markup('<a data-toggle="modal" data-remote="/alarms/%d" href="/alarms/%d" data-target="#alarm-show-model">%s</a>' % (record.id, record.id, value))

class AlarmTable(t.Table):
    
    ack         = t.Action(name=u'确认', endpoint='alarms.alarms_ack', modalable=True)
    clear       = t.Action(name=u'清除', endpoint='alarms.alarms_clear', modalable=True)

    check       = t.CheckBoxColumn()

    severity    = SeverityColumn()
    alarm_state = t.EnumColumn(u'状态', name='alarm-state', enums=constants.STATES,  orderable=True)
    alarm_alias = t.LinkColumn(u'名称', endpoint='alarms.alarms_show', orderable=True)
    node_alias  = t.LinkColumn(u'节点', accessor='node.alias', orderable=True)
    node_addr   = t.Column(u'节点地址', accessor='node.addr', orderable=True) 
    summary     = t.Column(u'详细')
    occur_count = t.Column(u'发生次数')
    last_occurrence = t.Column(u'最后发生时间', orderable=True)

    class Meta:
        model = Alarm
        per_page = 30
        order_by = '-last_occurrence'
        url_makers = {
            'node_alias': lambda record: redirect_node_show(record.node),
            'node_addr': lambda record: redirect_node_show(record.node)
        }

class QueryTable(t.Table):
    #edit       = t.Action(name=u'Edit', endpoint='alarms.query_edit')

    check       = t.CheckBoxColumn()
    name        = t.LinkColumn(u'名称', endpoint='alarms.query_edit', orderable=True)
    is_public   = t.Column(u'是否公开', orderable=True)
    created_at  = t.Column(u'创建时间', orderable=True)
    updated_at  = t.Column(u'最后更新时间')

    class Meta:
        model = Query
        per_page = 30
        order_by = '-created_at'

class HistoryTable(t.Table):
    severity    = SeverityColumn()
    alarm_alias = t.LinkColumn(u'名称', endpoint='alarms.history_show', orderable=True)
    node_alias  = t.LinkColumn(u'节点', orderable=True) #accessor='node.alias',
    node_addr   = t.Column(u'节点地址')
    summary     = t.Column(u'详细')
    occur_count = t.Column(u'发生次数')
    last_occurrence = t.Column(u'最后发生时间', orderable=True)
    created_at  = t.Column(u'迁移历史时间')

    class Meta:
        model = History
        per_page = 30
        order_by = '-created_at'
        url_makers = {
            'node_alias': lambda record: redirect_node_show(record.node)
        }

class AlarmClassTable(t.Table):
    name        = t.LinkColumn(u'名称', endpoint='alarms.classes_edit', orderable=True)
    alias       = t.LinkColumn(u'显示名', endpoint='alarms.classes_edit', orderable=True)
    category   = t.Column(u'分类', accessor="category.alias")
    severity    = SeverityColumn()
    probable_cause   = t.Column(u'可能原因')
    specific_problem = t.Column(u'特定原因')
    additional_info   = t.Column(u'附加信息')
    remark           = t.Column(u'备注')

    class Meta:
        model = AlarmClass
        per_page = 50
        order_by = 'id'

class AlarmKnowledgeTable(t.Table):
    
    class_alias     = t.LinkColumn(u'故障名称', endpoint='alarms.knowledges_edit', accessor='alarm_class.alias', orderable=True)
    probable_cause  = t.Column(u'可能原因')
    resolvent       = t.Column(u'解决方法')
    probability     = t.EnumColumn(u'发生概率', 'probability', enums={1: u'极少发生', 2: u'偶尔发生', 3: u'频繁发生'})
    apply_count     = t.Column(u'应用次数')

    class Meta:
        model = AlarmKnowledge
        per_page = 50
        order_by = '-id'

