#!/usr/bin/env python
# -*- coding: utf-8 -*-

from tango.ui import tables

from tango.models import Query

from .models import Alarm,History

class AlarmTable(tables.Table):
    check       = tables.CheckBoxColumn()
    severity    = tables.EnumColumn(verbose_name=u'级别', name='severity', enums={0: u'清除', 1: u'事件', 2: u'警告', 3: u'次要', 4: u'重要', 5: u'紧急'}, orderable=True)
    alarm_state = tables.Column(verbose_name=u'状态', orderable=True)
    alarm_alias = tables.Column(verbose_name=u'名称', orderable=True)
    node_alias  = tables.Column(verbose_name=u'节点', orderable=True) #accessor='node.alias', 
    node_addr   = tables.Column(verbose_name=u'节点地址') #, accessor='node.addr'
    summary     = tables.Column(verbose_name=u'详细')
    occur_count = tables.Column(verbose_name=u'发生次数')
    last_occurrence = tables.Column(verbose_name=u'最后发生时间', orderable=True)

    class Meta:
        model = Alarm
        per_page = 30
        order_by = '-last_occurrence'

class QueryTable(tables.Table):
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
