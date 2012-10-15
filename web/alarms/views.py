#!/usr/bin/env python
# -*- coding: utf-8 -*-

from datetime import datetime

from sqlalchemy import desc, func 

from jinja2 import Markup

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import db

from jinja2 import Markup

from tango import user_profile

from tango.login import login_required, current_user

from tango.ui import menus, Menu

from tango.ui import tables

from tango.ui import Widget, add_widget

from tango.ui.tables import TableConfig

from tango.models import Query, Profile

from .models import Alarm, AlarmSeverity, History, AlarmClass, AlarmKnowledge

from .forms import QueryNewForm, AlarmAckForm, AlarmClearForm, AlarmClassForm, AlarmKnowledgeForm

from .tables import AlarmTable, QueryTable, HistoryTable, AlarmClassTable, AlarmKnowledgeTable

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
    #should use one Query
    severities = AlarmSeverity.query.order_by(desc(AlarmSeverity.id)).all()
    counts = db.session.query(Alarm.severity, func.count(Alarm.id).label('count')).group_by(Alarm.severity).all()
    all_count = 0
    for svt in severities:
        for id, count in counts:
            if svt.id == id:
                all_count += count
                svt.count = count
    queries = Query.query.filter_by(uid=current_user.id, tab='alarms').all()
    profile = user_profile(AlarmTable._meta.profile)
    table = AlarmTable(Alarm.query.filter(alarm_filter(request)))
    TableConfig(request, profile).configure(table)
    return render_template("/alarms/index.html", table = table,
        severities = severities, queries = queries, all_count = all_count)

@alarmview.route('/alarms/<int:id>')
@login_required
def alarm_show(id):
    alarm = Alarm.query.get_or_404(id)
    return render_template("/alarms/detail.html", alarm=alarm)

@alarmview.route('/alarms/ack/<int:id>', methods=['GET', 'POST'])
@login_required
def alarm_ack(id):
    form = AlarmAckForm()
    alarm = Alarm.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        alarm.acked = 1
        alarm.alarm_state = 2
        alarm.acked_time = datetime.now()
        alarm.acked_user = current_user.username
        alarm.acked_note = form.acked_note.data
        db.session.commit()
        return redirect(url_for('.index'))
    else: # request.method == 'GET':
        form.process(obj=alarm)
        return render_template('/alarms/ack.html', alarm=alarm, form=form)

@alarmview.route('/alarms/clear/<int:id>', methods=['GET', 'POST'])
@login_required
def alarm_clear(id=None):
    form = AlarmClearForm()
    alarm = Alarm.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        alarm.cleared = 1
        alarm.severity = 0
        alarm.alarm_state = 3
        alarm.cleared_time = datetime.now()
        alarm.cleared_user = current_user.username
        alarm.cleared_note = form.cleared_note.data
        db.session.commit()
        return redirect(url_for('.index'))
    else:
        form.process(obj=alarm)
        return render_template('/alarms/clear.html', alarm=alarm, form=form)

@alarmview.route('/alarms/queries')
@login_required
def queries():
    profile = user_profile(QueryTable.profile)
    query = Query.query.filter_by(tab='alarms')
    table = QueryTable(query)
    TableConfig(request, profile).configure(table)
    return render_template("/alarms/queries/index.html", table = table)

@alarmview.route('/alarms/queries/new', methods=['GET','POST'])
@login_required
def query_new():
    f = QueryNewForm()
    q = Query(uid=current_user.id, tab='alarms', filters='', created_at=datetime.now(), updated_at=datetime.now())
    if request.method == 'POST':
        q.name = request.form['name']
        q.is_public = True if request.form['is_public'] == 'y' else False
        db.session.add(q)
        db.session.commit()
        return redirect(url_for('.queries'))
    f.process(obj=q)
    return render_template("/alarms/queries/new.html", form=f, query=q)

@alarmview.route('/alarms/queries/edit/<int:id>', methods=['GET','POST'])
@login_required
def query_edit(id):
    q = Query.query.get_or_404(id)
    return render_template("/alarms/queries/edit.html", query=q)

@alarmview.route('/alarms/console')
@login_required
def alarm_console():
    return render_template("/alarms/console.html")

@alarmview.route('/alarms/histories')
@login_required
def histories():
    profile = user_profile(HistoryTable.profile)
    table = HistoryTable(History.query)
    TableConfig(request, profile).configure(table)
    return render_template("/alarms/histories.html", table=table)

@alarmview.route('/alarms/statistics/active')
@login_required
def statistics_active():
    return render_template('/alarms/statistics/active.html')
    
@alarmview.route('/alarms/statistics/history')
@login_required
def statistics_history():
    return render_template('/alarms/statistics/history.html')

#TODO:
@alarmview.route('/alarms/classes')
@login_required
def classes():
    keyword = request.args.get('keyword', '')
    query = AlarmClass.query
    if keyword:
        query = query.filter(db.or_(AlarmClass.name.ilike('%'+keyword+'%'),
                                    AlarmClass.alias.ilike('%'+keyword+'%')))
    table = AlarmClassTable(query)
    profile = user_profile(AlarmClassTable.profile)
    TableConfig(request, profile).configure(table)
    return render_template("/alarms/classes/index.html", table=table, keyword=keyword)

@alarmview.route('/alarms/classes/edit/<int:id>', methods=['GET', 'POST'])
def class_edit(id):
    form = AlarmClassForm()
    alarm_class = AlarmClass.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(alarm_class)
        db.session.add(alarm_class)
        db.session.commit() 
        flash(u'告警类型修改成功')
        return redirect(url_for('alarms.classes'))
    form.process(obj=alarm_class)
    return render_template("/alarms/classes/edit.html", form = form, alarm_class = alarm_class)

@alarmview.route("/alarms/knowledges/")
@login_required
def knowledges():
    profile = user_profile(AlarmKnowledgeTable.profile)
    table = AlarmKnowledgeTable(AlarmKnowledge.query)
    TableConfig(request, profile).configure(table)
    return render_template('/alarms/knowledges/index.html', table=table)

@alarmview.route('/alarms/knowledges/new', methods=['GET', 'POST'])
@login_required
def knowledge_new():
    form = AlarmKnowledgeForm()
    if request.method == 'POST' and form.validate_on_submit():
        record = AlarmKnowledge()
        form.populate_obj(record)
        db.session.add(record)
        db.session.commit()
        flash("Add Alarm Knowledge Successfully!")
        return redirect(url_for('.knowledges'))
    return render_template('/alarms/knowledges/new.html', form=form)

@alarmview.route('/alarms/knowledges/edit/<int:id>', methods=['GET', 'POST'])
@login_required
def knowledge_edit(id):
    form = AlarmKnowledgeForm()
    record = AlarmKnowledge.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(record)
        db.session.add(record)
        db.session.commit()
        flash("Edit Alarm Knowledge Successfully!")
        return redirect(url_for('.knowledges'))
    form.process(obj=record)
    return render_template('/alarms/knowledges/edit.html', form=form, record=record)

@alarmview.route('/alarms/settings', methods=['GET', 'POST'])
def settings():
    q = Setting.query.filter_by(mod='alarms')
    t = SettingTable(q)
    return render_template('/alarms/settings.html', table=t)

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

