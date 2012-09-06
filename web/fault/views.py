#!/usr/bin/env python
# -*- coding: utf-8 -*-

from sqlalchemy import desc

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.login import login_required, current_user

from tango.ui import menus, Menu

from tango.ui import tables

from tango.ui import Widget, add_widget

from tango.models import Query, Profile

from .models import Alarm, AlarmSeverity, History

from .tables import AlarmTable, HistoryTable, QueryTable

from .forms import QueryNewForm

faultview = Blueprint("fault", __name__, url_prefix='/fault')

def alarm_filter(request):
    filter = []
    if 'severity' in request.args: 
        id = AlarmSeverity.name2id(request.args['severity']) 
        if id != -1:
            filter.append("severity="+str(id))
    if 'query_id' in request.args:
        if request.args['query_id'] != '-1':
            filter.append("query_id="+request.args['query_id'])
    return ' and '.join(filter)

@faultview.route('/alarms', methods = ['GET'])
@login_required
def alarms():
    severities = AlarmSeverity.query.order_by(desc(AlarmSeverity.id)).all()
    queries = Query.query.filter_by(uid=current_user.id, tab='alarms').all()
    profile = Profile.load(current_user.id, 'table-alarms')
    table = AlarmTable(Alarm.query.filter(alarm_filter(request))).configure(profile)
    return render_template("/fault/index.html", table = table,
        severities = severities, queries = queries)

@faultview.route('/queries')
@login_required
def alarm_queries():
    q = Query.query.filter_by(tab='alarms')
    profile = Profile.load(current_user.id, 'table-alarm-queries')
    t = QueryTable(q).configure(profile)
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
    profile = Profile.load(current_user.id, 'table-histories')
    t = HistoryTable(History.query).configure(Profile)
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


menus.append(Menu('fault', u'故障', '/fault/alarms'))

add_widget(Widget('event_summary', u'告警统计', url = '/widgets/alarm/summary'))
add_widget(Widget('event_statistics', u'告警概要', content='<div style="height:100px">......</div>'))
add_widget(Widget('dashboard1', 'Dashboard1', ''))
add_widget(Widget('dashboard2', 'Dashboard2', ''))

