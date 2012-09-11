#!/usr/bin/env python
# -*- coding: utf-8 -*-

from jinja2 import Markup

from tango.ui import tables

from tango.models import Query

from .models import Alarm,History

import constants

class SeverityColumn(tables.EnumColumn):

    def __init__(self, attrs=None, **extra):
        super(SeverityColumn, self).__init__(name='severity', enums=constants.SEVERITIES, verbose_name=u'级别')
    
    def render(self, value, record, bound_column):
        text = super(SeverityColumn, self).render(value, record, bound_column)
        return Markup('<span class="label severity-%d">%s</span>' % (value, text))

class AlarmAliasColumn(tables.Column):
    
    def __init(self, attrs=None,**extra):
        super(AlarmAliasColumn, self).__init__(attrs, extra)

    def render(self, value, record, bound_column):
        return Markup('<a data-toggle="modal" data-remote="/alarms/%d" href="/alarms/%d" data-target="#alarm-show-model">%s</a>' % (record.id, record.id, value))

class NodeLinkColumn(tables.BaseLinkColumn):
    def render(self, value, record, bound_column):
        return self.render_link("/nodes/%d" % record.node_id, value)

class AlarmTable(tables.Table):
    
    ack         = tables.Action(name=u'确认', endpoint='alarms.alarm_ack')
    clear       = tables.Action(name=u'清除', endpoint='alarms.alarm_clear')

    check       = tables.CheckBoxColumn()

    severity    = SeverityColumn()
    alarm_state = tables.EnumColumn(verbose_name=u'状态', name='alarm-state', enums=constants.STATES,  orderable=True)
    alarm_alias = tables.LinkColumn(verbose_name=u'名称', endpoint='alarms.alarm_show', orderable=True)
    node_alias  = NodeLinkColumn(verbose_name=u'节点', orderable=True) #accessor='node.alias', 
    node_addr   = tables.Column(verbose_name=u'节点地址') #, accessor='node.addr'
    summary     = tables.Column(verbose_name=u'详细')
    occur_count = tables.Column(verbose_name=u'发生次数')
    last_occurrence = tables.Column(verbose_name=u'最后发生时间', orderable=True)

    class Meta:
        model = Alarm
        per_page = 30
        order_by = '-last_occurrence'

class QueryTable(tables.Table):
    edit       = tables.Action(name=u'Edit', endpoint='alarms.query_edit')

    check       = tables.CheckBoxColumn()
    name        = tables.Column(verbose_name=u'名称', orderable=True)
    is_public   = tables.Column(verbose_name=u'是否公开', orderable=True)
    created_at  = tables.Column(verbose_name=u'创建时间', orderable=True)
    updated_at  = tables.Column(verbose_name=u'最后更新时间')

    class Meta:
        model = Query
        per_page = 30
        order_by = '-created_at'

class HistoryTable(tables.Table):
    severity    = tables.Column(verbose_name=u'级别', orderable=True)
    alarm_alias = tables.Column(verbose_name=u'名称', orderable=True)
    node_alias  = tables.Column(verbose_name=u'节点', orderable=True)
    node_addr   = tables.Column(verbose_name=u'节点地址')
    summary     = tables.Column(verbose_name=u'详细')
    occur_count = tables.Column(verbose_name=u'发生次数')
    last_occurrence = tables.Column(verbose_name=u'最后发生时间', orderable=True)
    created_at  = tables.Column(verbose_name=u'迁移历史时间')

    class Meta:
        model = History
        per_page = 30
        order_by = '-created_at'
