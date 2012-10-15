# coding: utf-8

import constants

from jinja2 import Markup

from tango.ui import tables as t

from tango.models import Query

from .models import Alarm, AlarmSeverity, History, AlarmClass, AlarmKnowledge

class SeverityColumn(t.EnumColumn):

    def __init__(self, attrs=None, **extra):
        super(SeverityColumn, self).__init__(name='severity', enums=constants.SEVERITIES, verbose_name=u'级别')
    
    def render(self, value, record, bound_column):
        text = super(SeverityColumn, self).render(value, record, bound_column)
        return Markup('<span class="label severity-%d">%s</span>' % (value, text))

class AlarmAliasColumn(t.Column):
    
    def __init(self, attrs=None,**extra):
        super(AlarmAliasColumn, self).__init__(attrs, extra)

    def render(self, value, record, bound_column):
        return Markup('<a data-toggle="modal" data-remote="/alarms/%d" href="/alarms/%d" data-target="#alarm-show-model">%s</a>' % (record.id, record.id, value))

class NodeLinkColumn(t.BaseLinkColumn):
    def render(self, value, record, bound_column):
        return self.render_link("/nodes/%d" % record.node_id, value)

class AlarmTable(t.Table):
    
    ack         = t.Action(name=u'确认', endpoint='alarms.alarm_ack')
    clear       = t.Action(name=u'清除', endpoint='alarms.alarm_clear')

    check       = t.CheckBoxColumn()

    severity    = SeverityColumn()
    alarm_state = t.EnumColumn(verbose_name=u'状态', name='alarm-state', enums=constants.STATES,  orderable=True)
    alarm_alias = t.LinkColumn(verbose_name=u'名称', endpoint='alarms.alarm_show', orderable=True)
    node_alias  = NodeLinkColumn(verbose_name=u'节点', orderable=True) #accessor='node.alias', 
    node_addr   = t.Column(verbose_name=u'节点地址') #, accessor='node.addr'
    summary     = t.Column(verbose_name=u'详细')
    occur_count = t.Column(verbose_name=u'发生次数')
    last_occurrence = t.Column(verbose_name=u'最后发生时间', orderable=True)

    class Meta:
        model = Alarm
        per_page = 30
        order_by = '-last_occurrence'

class QueryTable(t.Table):
    #edit       = t.Action(name=u'Edit', endpoint='alarms.query_edit')

    check       = t.CheckBoxColumn()
    name        = t.LinkColumn(verbose_name=u'名称', endpoint='alarms.query_edit', orderable=True)
    is_public   = t.Column(verbose_name=u'是否公开', orderable=True)
    created_at  = t.Column(verbose_name=u'创建时间', orderable=True)
    updated_at  = t.Column(verbose_name=u'最后更新时间')

    class Meta:
        model = Query
        per_page = 30
        order_by = '-created_at'

class HistoryTable(t.Table):
    severity    = SeverityColumn()
    alarm_alias = t.LinkColumn(verbose_name=u'名称', endpoint='alarms.history_show', orderable=True)
    node_alias  = NodeLinkColumn(verbose_name=u'节点', orderable=True) #accessor='node.alias', 
    node_addr   = t.Column(verbose_name=u'节点地址')
    summary     = t.Column(verbose_name=u'详细')
    occur_count = t.Column(verbose_name=u'发生次数')
    last_occurrence = t.Column(verbose_name=u'最后发生时间', orderable=True)
    created_at  = t.Column(verbose_name=u'迁移历史时间')

    class Meta:
        model = History
        per_page = 30
        order_by = '-created_at'

class AlarmClassTable(t.Table):
    name        = t.LinkColumn(verbose_name=u'分类', endpoint='alarms.class_edit', orderable=True)
    alias       = t.LinkColumn(verbose_name=u'名称', endpoint='alarms.class_edit', orderable=True)
    severity    = SeverityColumn()
    x733_type   = t.Column(verbose_name=u'X733类型')
    probablecause   = t.Column(verbose_name=u'可能原因')
    specific_problem = t.Column(verbose_name=u'特定原因')
    additionalinfo   = t.Column(verbose_name=u'附加信息')
    remark           = t.Column(verbose_name=u'备注')

    class Meta:
        model = AlarmClass
        per_page = 50
        order_by = 'id'

class AlarmKnowledgeTable(t.Table):
    
    class_alias     = t.LinkColumn(verbose_name=u'故障名称', endpoint='alarms.knowledge_edit', accessor='alarm_class.alias', orderable=True)
    probable_cause  = t.Column(verbose_name=u'可能原因')
    resolvent       = t.Column(verbose_name=u'解决方法')
    probability     = t.EnumColumn('probability', verbose_name=u'发生概率', enums={1: u'极少发生', 2: u'偶尔发生', 3: u'频繁发生'})
    apply_count     = t.Column(verbose_name=u'应用次数')

    class Meta:
        model = AlarmKnowledge
        per_page = 50
        order_by = '-id'

