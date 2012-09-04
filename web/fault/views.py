#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.login import login_required

from tango.ui import menus, Menu

from tango.ui import tables

from tango.ui import Widget, add_widget

from tango.models import Query

from .models import Alarm, AlarmSeverity, History

from sqlalchemy import desc

from .forms import QueryNewForm

faultview = Blueprint("fault", __name__, url_prefix='/fault')

class AlarmTable(tables.Table):
    check       = tables.CheckBoxColumn()
    severity    = tables.Column(verbose_name=u'级别', orderable=True)
    alarm_alias = tables.Column(verbose_name=u'名称', orderable=True)
    node        = tables.Column(verbose_name=u'节点', accessor='node.alias', orderable=True)
    node_addr   = tables.Column(verbose_name=u'节点地址', accessor='node.addr')
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

@faultview.route('/')
@login_required
def index():
    severities = AlarmSeverity.query.order_by(desc(AlarmSeverity.id)).all()
    table = AlarmTable(Alarm.query, request)
    return render_template("/fault/index.html",
        table = table, severities = severities)

@faultview.route('/queries')
@login_required
def alarm_queries():
    q = Query.query.filter_by(tab='alarms')
    t = QueryTable(q, request)
    return render_template("/fault/queries.html", table = t)

@faultview.route('/queries/new')
@login_required
def alarm_query_new():
    f = QueryNewForm()
    return render_template("/fault/query_new.html", form = f)

@faultview.route('/console')
@login_required
def alarm_console():
    return render_template("/fault/console.html")

@faultview.route('/histories')
@login_required
def alarm_histories():
    t = HistoryTable(History.query, request)
    return render_template("/fault/histories.html", table=t)

@faultview.route('/alarms/ack')
@login_required
def alarms_ack():
    #TODO: ack
    return redirect(url_for('index'))

@faultview.route('/alarms/clear')
@login_required
def alarms_clear():
    #TODO: clear
    return redirect(url_for('index'))


menus.append(Menu('fault', u'故障', '/fault'))

add_widget(Widget('event_summary', u'告警统计', url = '/widgets/alarm/summary'))
add_widget(Widget('event_statistics', u'告警概要', content='<div style="height:100px">......</div>'))
add_widget(Widget('dashboard1', 'Dashboard1', ''))
add_widget(Widget('dashboard2', 'Dashboard2', ''))

