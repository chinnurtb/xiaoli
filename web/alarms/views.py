#!/usr/bin/env python
# -*- coding: utf-8 -*-

from sqlalchemy import desc

from jinja2 import Markup

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

import constants

alarmview = Blueprint("alarms", __name__)

def alarm_filter(request):
    filter = []
    if 'severity' in request.args:
        id = AlarmSeverity.name2id(request.args['severity']) 
        if id != -1:
            filter.append("severity="+str(id))
    if 'query_id' in request.args:
        if request.args['query_id'] != '':
            filter.append("query_id="+request.args['query_id'])
    return ' and '.join(filter)

@alarmview.route('/alarms', methods = ['GET'])
@login_required
def index():
    print request.args
    severities = AlarmSeverity.query.order_by(desc(AlarmSeverity.id)).all()
    queries = Query.query.filter_by(uid=current_user.id, tab='alarms').all()
    profile = Profile.load(current_user.id, 'table-alarms')
    table = AlarmTable(Alarm.query.filter(alarm_filter(request))).configure(profile)
    return render_template("/alarms/index.html", table = table,
        severities = severities, queries = queries)

@alarmview.route('/alarms/<int:id>')
@login_required
def alarm_show(id):
    alarm = Alarm.query.get_or_404(id)
    return render_template("/alarms/detail.html", alarm=alarm)

@alarmview.route('/alarms/ack/<ids>', )
@login_required
def alarm_ack(ids):
    return redirect(url_for('index'))

@alarmview.route('/alarms/clear/<ids>')
@login_required
def alarm_clear():
    #TODO: clear
    return redirect(url_for('index')) 

@alarmview.route('/alarms/queries')
@login_required
def queries():
    q = Query.query.filter_by(tab='alarms')
    profile = Profile.load(current_user.id, 'table-alarm-queries')
    t = QueryTable(q).configure(profile)
    return render_template("/alarms/queries/index.html", table = t)

@alarmview.route('/alarms/queries/new')
@login_required
def query_new():
    f = QueryNewForm()
    return render_template("/alarms/queries/new.html", form = f)

@alarmview.route('/alarms/console')
@login_required
def alarm_console():
    return render_template("/alarms/console.html")

@alarmview.route('/alarms/histories')
@login_required
def histories():
    profile = Profile.load(current_user.id, 'table-histories')
    t = HistoryTable(History.query).configure(Profile)
    return render_template("/alarms/histories.html", table=t)

@alarmview.app_template_filter("alarm_severity")
def alarm_severity_filter(s):
   return Markup('<span class="label severity-%s">%s</span>' % (s, constants.SEVERITIES[int(s)]))

@alarmview.app_template_filter("alarm_state")
def alarm_state_filter(s):
    return constants.STATES[int(s)] 

menus.append(Menu('alarms', u'故障', '/alarms'))

add_widget(Widget('event_summary', u'告警统计', url = '/widgets/alarm/summary'))
add_widget(Widget('event_statistics', u'告警概要', content='<div style="height:100px">......</div>'))
add_widget(Widget('dashboard1', 'Dashboard1', ''))
add_widget(Widget('dashboard2', 'Dashboard2', ''))

