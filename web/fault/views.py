#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.login import login_required

from tango.ui import menus, Menu

from tango.ui import tables

from tango.ui import Widget, add_widget

from .models import Alarm

faultview = Blueprint("fault", __name__, url_prefix='/fault')

class AlarmTable(tables.Table):
    check       = tables.CheckBoxColumn()
    severity    = tables.Column(verbose_name=u'级别', orderable=True)
    alarm_alias = tables.Column(verbose_name=u'告警名称', orderable=True)
    node        = tables.Column(verbose_name=u'告警节点', accessor='node.alias', orderable=True)
    summary     = tables.Column(verbose_name=u'告警详细')
    last_occurrence = tables.Column(verbose_name=u'最后发生时间')

    class Meta:
        model = Alarm
        per_page = 30
        order_by = '-last_occurrence'

@faultview.route('/')
@login_required
def index():
    table = AlarmTable(Alarm.query, request)
    return render_template("/fault/index.html", table = table)

@faultview.route('/console')
@login_required
def histories():
    return render_template("/fault/console.html")

@faultview.route('/histories')
@login_required
def histories():
    return render_template("/fault/histories.html")

menus.append(Menu('fault', u'故障', '/fault'))

add_widget(Widget('event_summary', u'告警统计', url = '/widgets/alarm/summary'))
add_widget(Widget('event_statistics', u'告警概要', content='<div style="height:100px">......</div>'))
add_widget(Widget('dashboard1', 'Dashboard1', ''))
add_widget(Widget('dashboard2', 'Dashboard2', ''))

