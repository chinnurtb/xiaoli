#!/usr/bin/env python
# -*- coding: utf-8 -*-

from datetime import datetime

from sqlalchemy import desc, func 

from jinja2 import Markup

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import db

from jinja2 import Markup

from tango.login import login_required, current_user

from tango.ui import menus, Menu

from tango.ui import tables

from tango.ui import Widget, add_widget

from tango.models import Query, Profile

from .models import Alarm, AlarmSeverity, History, AlarmClass, AlarmKnowledge

from .forms import QueryNewForm, AlarmAckForm, AlarmClearForm, AlarmClassForm, AlarmKnowledgeForm

import constants

alarmview = Blueprint("alarms", __name__)

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
    #edit       = tables.Action(name=u'Edit', endpoint='alarms.query_edit')

    check       = tables.CheckBoxColumn()
    name        = tables.LinkColumn(verbose_name=u'名称', endpoint='alarms.query_edit', orderable=True)
    is_public   = tables.Column(verbose_name=u'是否公开', orderable=True)
    created_at  = tables.Column(verbose_name=u'创建时间', orderable=True)
    updated_at  = tables.Column(verbose_name=u'最后更新时间')

    class Meta:
        model = Query
        per_page = 30
        order_by = '-created_at'

class HistoryTable(tables.Table):
    severity    = SeverityColumn()
    alarm_alias = tables.LinkColumn(verbose_name=u'名称', endpoint='alarms.history_show', orderable=True)
    node_alias  = NodeLinkColumn(verbose_name=u'节点', orderable=True) #accessor='node.alias', 
    node_addr   = tables.Column(verbose_name=u'节点地址')
    summary     = tables.Column(verbose_name=u'详细')
    occur_count = tables.Column(verbose_name=u'发生次数')
    last_occurrence = tables.Column(verbose_name=u'最后发生时间', orderable=True)
    created_at  = tables.Column(verbose_name=u'迁移历史时间')

    class Meta:
        model = History
        per_page = 30
        order_by = '-created_at'

class AlarmClassTable(tables.Table):
    name        = tables.LinkColumn(verbose_name=u'分类', endpoint='alarms.class_edit', orderable=True)
    alias       = tables.LinkColumn(verbose_name=u'名称', endpoint='alarms.class_edit', orderable=True)
    severity    = SeverityColumn()
    x733_type   = tables.Column(verbose_name=u'X733类型')
    probablecause   = tables.Column(verbose_name=u'可能原因')
    specific_problem = tables.Column(verbose_name=u'特定原因')
    additionalinfo   = tables.Column(verbose_name=u'附加信息')
    remark           = tables.Column(verbose_name=u'备注')

    class Meta:
        model = AlarmClass
        per_page = 50
        order_by = 'id'

class AlarmKnowledgeTable(tables.Table):
    
    class_alias     = tables.LinkColumn(verbose_name=u'故障名称', endpoint='alarms.knowledge_edit', accessor='alarm_class.alias', orderable=True)
    probable_cause  = tables.Column(verbose_name=u'可能原因')
    resolvent       = tables.Column(verbose_name=u'解决方法')
    probability     = tables.EnumColumn('probability', verbose_name=u'发生概率', enums={1: u'极少发生', 2: u'偶尔发生', 3: u'频繁发生'})
    apply_count     = tables.Column(verbose_name=u'应用次数')

    class Meta:
        model = AlarmKnowledge
        per_page = 50
        order_by = '-id'

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
    profile = Profile.load(current_user.id, 'table-alarms')
    table = AlarmTable(Alarm.query.filter(alarm_filter(request))).configure(profile)
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
    q = Query.query.filter_by(tab='alarms')
    profile = Profile.load(current_user.id, 'table-alarm-queries')
    t = QueryTable(q).configure(profile)
    return render_template("/alarms/queries/index.html", table = t)

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
    profile = Profile.load(current_user.id, 'table-histories')
    table = HistoryTable(History.query).configure(profile)
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
    profile = Profile.load(current_user.id, 'table-alarm-classes')
    keyword = request.args.get('keyword', '')
    query = AlarmClass.query
    if keyword:
        query = query.filter(db.or_(AlarmClass.name.ilike('%'+keyword+'%'),
                                    AlarmClass.alias.ilike('%'+keyword+'%')))
    table = AlarmClassTable(query).configure(profile)
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
    profile = Profile.load(current_user.id, 'table-alarm-knowledges')
    query = AlarmKnowledge.query
    table = AlarmKnowledgeTable(query).configure(profile)
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
    return render_template('/alarms/settings.html')


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

