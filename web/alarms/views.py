# coding: utf-8

from datetime import datetime

from flask import json

from sqlalchemy import desc, func 

from jinja2 import Markup

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import db, get_profile

from tango.ui.tables import make_table

from tango.login import current_user

from tango.ui import tables, navbar, dashboard, Dashboard 

from tango.models import Query, Profile, Category, Setting

from nodes.models import Node, Vendor, Area
from system.tables import SettingTable
from system.forms import SettingEditForm

from .models import Alarm, AlarmSeverity, History, AlarmClass, AlarmJournal, AlarmKnowledge, query_severities

from .forms import QueryNewForm, AlarmAckForm, AlarmClearForm, AlarmClassForm, AlarmKnowledgeForm, AlarmFilterForm, SearchForm

from .tables import AlarmTable, QueryTable, HistoryTable, AlarmClassTable, AlarmKnowledgeTable

import constants

alarmview = Blueprint("alarms", __name__)

@alarmview.context_processor
def inject_navid():
    return dict(navid = 'alarms')

#===============================================================
#当前告警和历史告警 
#===============================================================
def alarm_filter(cls, query, form):
    """告警过滤"""
    severity = form.alarm_severity.data
    if severity is not None:
        query = query.filter(cls.severity == severity.id)
    alarm_class = form.alarm_class.data
    if alarm_class:
        query = query.filter(cls.class_id == alarm_class.id)
    start_date = form.start_date.data
    if start_date:
        query = query.filter(cls.first_occurrence >= start_date)
    end_date = form.end_date.data
    if end_date:
        query = query.filter(cls.first_occurrence <= end_date)
    keyword = form.keyword.data
    if keyword and keyword != '':
        query = query.filter(db.or_(
                    cls.alarm_alias.ilike('%'+keyword+'%'),
                    cls.node.has(Node.addr.ilike('%'+keyword+'%')),
                    cls.node.has(Node.alias.ilike('%'+keyword+'%'))))
    return query

@alarmview.route('/alarms', methods = ['GET'])
def index():
    filterForm = AlarmFilterForm(formdata=request.args)
    alarm_query = Alarm.query
    if not current_user.is_province_user:
        alarm_query = alarm_query.outerjoin(Node, Node.id == Alarm.node_id)
        alarm_query = alarm_query.outerjoin(Area, Node.area_id==Area.id).filter(current_user.domain.clause_permit)
    query = alarm_filter(Alarm, alarm_query, filterForm)
    severity = request.args.get('severity')
    if severity:
        query = query.filter(Alarm.severity == AlarmSeverity.name2id(severity))
    severities = query_severities()
    total = sum([severity.count for severity in severities])
    table = make_table(query, AlarmTable)
    return render_template("/alarms/index.html",
        table = table, filterForm = filterForm, 
        severities = severities, total = total)

@alarmview.route('/alarms/<int:id>')
def alarms_show(id):
    alarm = Alarm.query.get_or_404(id)
    return render_template("alarms/show.html", alarm=alarm)

@alarmview.route('/alarms/ack/<int:id>', methods=['GET', 'POST'])
def alarms_ack(id):
    form = AlarmAckForm()
    alarm = Alarm.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        alarm.acked = 1
        alarm.alarm_state = 2
        alarm.acked_time = datetime.now()
        alarm.acked_user = current_user.username
        alarm.acked_note = form.acked_note.data
        journal = AlarmJournal(uid=current_user.id,
                               alarm_id=alarm.id,
                               title = u'确认告警',
                               summary=u'确认说明:%s' % alarm.acked_note,
                               created_at=alarm.acked_time)
        db.session.add(journal)
        db.session.commit()
        return redirect(url_for('.index'))
        
    elif form.errors:
        flash(u'确认表单有误', 'error')
        return redirect(url_for('.index'))
        
    # request.method == 'GET':
    form.process(obj=alarm)
    kwargs = {
        'title'  : u'确认: %s / %s' % (alarm.node.alias, alarm.alarm_alias),
        'action' : url_for('alarms.alarms_ack', id=id),
        'form'   : form,
    }
    return render_template('tango/_modal.html', **kwargs)
    

@alarmview.route('/alarms/clear/<int:id>', methods=['GET', 'POST'])
def alarms_clear(id=None):
    form = AlarmClearForm()
    alarm = Alarm.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        alarm.cleared = 1
        alarm.severity = 0
        alarm.alarm_state = 3
        alarm.cleared_time = datetime.now()
        alarm.cleared_user = current_user.username
        alarm.cleared_note = form.cleared_note.data
        journal = AlarmJournal(uid=current_user.id,
                               alarm_id=alarm.id,
                               title = u'人工清除告警',
                               summary=u'清除说明:%s' % alarm.cleared_note,
                               created_at=alarm.cleared_time)
        db.session.add(journal)
        db.session.commit()
        return redirect(url_for('.index'))
        
    elif form.errors:
        flash(u'清除表单有误', 'error')
        return redirect(url_for('.index'))
        
    form.process(obj=alarm)
    kwargs = {
        'title'  : u'清除: %s / %s' % (alarm.node.alias, alarm.alarm_alias),
        'action' : url_for('alarms.alarms_clear', id=id),
        'form'   : form,
    }
    return render_template('tango/_modal.html', **kwargs)

@alarmview.route('/histories')
def histories():
    filterForm = AlarmFilterForm(formdata=request.args)
    history_query = History.query
    if not current_user.is_province_user:
        history_query = history_query.outerjoin(Node, Node.id == History.node_id)
        history_query = history_query.outerjoin(Area, Node.area_id==Area.id).filter(current_user.domain.clause_permit)
    query = alarm_filter(History, history_query, filterForm)
    table = make_table(query, HistoryTable)
    return render_template("/alarms/histories.html",
        table=table, filterForm=filterForm)

#======================================================
#告警控制台
#======================================================
@alarmview.route('/alarms/console/')
def alarms_console():

    #TODO: FIX ME later
    from datetime import datetime, timedelta
    today = datetime.today()
    dates = [today - timedelta(hours=i) for i in range(12)]
    hours = [str(d.hour) for d in reversed(dates)]

    severities = query_severities()

    data = dict([(h, dict([(s.id, 0) for s in severities])) for h in hours])

    alarms = db.session.query('occur_hour', 'severity', 'alarm_count').from_statement(
        "select t2.occur_hour,t2.severity,count(id) alarm_count \
        from (select id,t1.first_occurrence,severity,extract(hour from t1.first_occurrence) occur_hour from alarms t1 \
        where (current_timestamp - t1.first_occurrence) <= '12 hours') t2 \
        group by t2.occur_hour,t2.severity \
        order by t2.occur_hour,t2.severity;"
    ).all()

    for alarm in alarms:
        data[str(int(alarm.occur_hour))][alarm.severity] = alarm.alarm_count

    def series(severity):
        values = [{'series': severity.name, 
                   'x': h+u'点',
                   'y': data[h][severity.id]}
                   for h in hours]
        return {'key': severity.alias,
                'color': severity.color,
                'values': values}

    chartdata = [series(severity) for severity in severities]
    #title = u'最近12小时接收告警'

    alarmconsole.configure(get_profile('alarmconsole'))
    return render_template('alarms/console/index.html',
                           chartid = "alarm_demo_console",
                           chartdata = chartdata,
                           dashboard = alarmconsole)

@alarmview.route('/alarms/console/all')
def console_all():
    return render_console_chart('alarm_console_all', _console_query())

@alarmview.route('/alarms/console/lasthour')
def console_lasthour():
    from datetime import datetime, timedelta
    lastHour = datetime.today() - timedelta(hours = 1)
    filter = Alarm.first_occurrence >= lastHour
    return render_console_chart('alarm_console_lasthour', _console_query(filter))

def _console_query(filter=None):
    q = db.session.query(Alarm.severity, func.count(Alarm.id).label('count'))
    if filter is not None:
        q = q.filter(filter)
    if not current_user.is_province_user:
        q = q.outerjoin(Node, Node.id == Alarm.node_id)
        q = q.outerjoin(Area, Node.area_id==Area.id).filter(current_user.domain.clause_permit)
    return q.group_by(Alarm.severity).order_by(Alarm.severity)

@alarmview.route('/alarms/console/category_status')
def console_category_status():
    query = _console_category_query(100)
    return render_console_chart('console_category_status', query)

@alarmview.route('/alarms/console/category_perf')
def console_category_perf():
    query = _console_category_query(101)
    return render_console_chart('console_category_perf', query)

@alarmview.route('/alarms/console/category_system')
def console_category_system():
    query = _console_category_query(102)
    return render_console_chart('console_category_system', query)

def _console_category_query(cid):
    q = db.session.query(Alarm.severity, func.count(Alarm.id).label('count'))
    q = q.outerjoin(AlarmClass, Alarm.class_id == AlarmClass.id)
    q = q.outerjoin(Category, AlarmClass.category_id == Category.id)
    if not current_user.is_province_user:
        q = q.outerjoin(Node, Node.id == Alarm.node_id)
        q = q.outerjoin(Area, Node.area_id==Area.id).filter(current_user.domain.clause_permit)
    return q.filter(Category.id == cid).group_by(Alarm.severity)

def render_console_chart(id, query):
    severities = query_severities()
    counts = dict(query.all())
    data = [{'label': severity.alias, 
             'color': severity.color,
             'value': counts.get(severity.id, 0)}
              for severity in severities]
    chartdata = [{'values': data}]
    return render_template('alarms/console/_chart.html',
                           chartid = id, chartdata = chartdata)

@alarmview.route('/alarms/stats/active')
def stats_active():
    activestats.configure(get_profile('activestats'))
    return render_template('alarms/stats/active.html',
                           dashboard = activestats)
    
@alarmview.route('/alarms/stats/history')
def stats_history():
    return render_template('alarms/stats/history.html')

@alarmview.route('/alarms/classes')
def classes():
    query = AlarmClass.query
    form = SearchForm(formdata=request.args)
    keyword = form.keyword.data
    if keyword and keyword != '':
        query = query.filter(db.or_(AlarmClass.name.ilike('%'+keyword+'%'),
                                    AlarmClass.alias.ilike('%'+keyword+'%')))
    table = make_table(query, AlarmClassTable)
    return render_template("/alarms/classes/index.html",
                            table=table, filterForm=form)

@alarmview.route('/alarms/classes/edit/<int:id>', methods=['GET', 'POST'])
def classes_edit(id):
    form = AlarmClassForm()
    alarm_class = AlarmClass.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(alarm_class)
        db.session.add(alarm_class)
        db.session.commit() 
        flash(u'告警类型修改成功')
        return redirect(url_for('alarms.classes'))
    form.process(obj=alarm_class)
    return render_template('alarms/classes/edit.html', form = form, alarm_class = alarm_class)

@alarmview.route("/alarms/knowledges/")
def knowledges():
    query = AlarmKnowledge.query
    form = SearchForm(formdata=request.args)
    keyword = form.keyword.data
    if keyword and keyword != '':
        query = query.filter(AlarmKnowledge.alarm_class.has(
            AlarmClass.alias.ilike('%'+keyword+'%')))
    table = make_table(query, AlarmKnowledgeTable)
    return render_template('/alarms/knowledges/index.html',
                            table=table, filterForm=form)

@alarmview.route('/alarms/knowledges/new', methods=['GET', 'POST'])
def knowledges_new():
    form = AlarmKnowledgeForm()
    if request.method == 'POST' and form.validate_on_submit():
        record = AlarmKnowledge()
        form.populate_obj(record)
        db.session.add(record)
        db.session.commit()
        flash("Add Alarm Knowledge Successfully!")
        return redirect(url_for('.knowledges'))
    return render_template('alarms/knowledges/new.html', form=form)

@alarmview.route('/alarms/knowledges/edit/<int:id>', methods=['GET', 'POST'])
def knowledges_edit(id):
    form = AlarmKnowledgeForm()
    record = AlarmKnowledge.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(record)
        db.session.add(record)
        db.session.commit()
        flash("Edit Alarm Knowledge Successfully!")
        return redirect(url_for('.knowledges'))
    form.process(obj=record)
    return render_template('alarms/knowledges/edit.html', form=form, record=record)

@alarmview.route('/alarms/settings', methods=['GET', 'POST'])
def settings():
    table = make_table(Setting.query.filter(Setting.mod == 'alarms'), SettingTable)
    return render_template('/alarms/settings/index.html', table=table)

@alarmview.route('/alarms/setting/edit/<int:id>', methods=('GET', 'POST'))
def settings_edit(id):
    form = SettingEditForm()
    setting = Setting.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        old_value = setting.value
        setting.value = form.value.data
        db.session.commit()
        flash(u'%s 被修改: %s --> %s' % (setting.name, str(old_value), str(form.value.data)), 'success')
        return redirect('/alarms/settings/')
    form.process(obj=setting)
    return render_template('/alarms/settings/edit.html', form=form, setting=setting)

@alarmview.app_template_filter("alarm_severity")
def alarm_severity_filter(s):
   return Markup('<span class="label severity-%s">%s</span>' % (s, constants.SEVERITIES[int(s)]))

@alarmview.app_template_filter("alarm_state")
def alarm_state_filter(s):
    return constants.STATES[int(s)] 

@alarmview.app_template_filter("alarm_severity_alias")
def alarm_severity_alias(s):
    return constants.SEVERITIES[int(s)]

#==============================================
#Statistics 
#==============================================
@alarmview.route('/alarms/stats/by_last10')
def stats_by_last10():
    alarms = Alarm.query.filter(Alarm.severity > 0).order_by(Alarm.last_occurrence.desc())
    if not current_user.is_province_user:
        alarms = alarms.outerjoin(Node, Node.id == Alarm.node_id)
        alarms = alarms.outerjoin(Area, Node.area_id==Area.id).filter(current_user.domain.clause_permit)
    return render_template('alarms/stats/by_last10.html', alarms=alarms.limit(10))

@alarmview.route('/alarms/stats/by_severity')
def stats_by_severity():
    data = [{'label': s.alias, 'color': s.color, 'value': s.count}
                for s in query_severities()]
    chartdata = [{'values': data}]
    return render_template('alarms/stats/by_severity.html',
                            chartdata=chartdata)

@alarmview.route('/alarms/stats/by_category')
def stats_by_category():

    severities = AlarmSeverity.query.order_by(AlarmSeverity.id).all()

    q = db.session.query(func.count(Alarm.id), Alarm.severity, Category.id, Category.alias)
    q = q.outerjoin(AlarmClass, Alarm.class_id == AlarmClass.id)
    q = q.outerjoin(Category, AlarmClass.category_id == Category.id)
    q = q.group_by(Alarm.severity, Category.id, Category.alias).order_by(Category.id)
    if not current_user.is_province_user:
        q = q.outerjoin(Node, Node.id == Alarm.node_id)
        q = q.outerjoin(Area, Node.area_id==Area.id).filter(current_user.domain.clause_permit)

    #(id, alias): (clear, indeterminate, warning, minor, major, critical)
    rows = {}
    for count, severity, cat_id, cat_alias in q.all():
        row = rows.get((cat_id, cat_alias), [0,0,0,0,0,0,0])
        row[severity] = count
        row[6] += count #total
        rows[(cat_id, cat_alias)] = row
    print rows
    return render_template('alarms/stats/by_category.html',
                            severities = severities, data=rows)

@alarmview.route('/alarms/stats/by_class')
def stats_by_class():
    q = db.session.query(func.count(Alarm.id), AlarmClass.id, AlarmClass.alias)
    q = q.outerjoin(AlarmClass, Alarm.class_id == AlarmClass.id)
    q = q.group_by(AlarmClass.id, AlarmClass.alias)
    if not current_user.is_province_user:
        q = q.outerjoin(Node, Node.id == Alarm.node_id)
        q = q.outerjoin(Area, Node.area_id==Area.id).filter(current_user.domain.clause_permit)
    return render_template('alarms/stats/by_class.html', data=q.all())

@alarmview.route('/alarms/stats/by_node_category')
def stats_by_node_category():
    q = db.session.query(func.count(Alarm.id), Category.id, Category.alias)
    q = q.outerjoin(Node, Alarm.node_id == Node.id)
    q = q.outerjoin(Category, Node.category_id == Category.id)
    q = q.group_by(Category.id, Category.alias).order_by(Category.id)
    if not current_user.is_province_user:
        q = q.outerjoin(Area, Node.area_id==Area.id).filter(current_user.domain.clause_permit)
    return render_template('alarms/stats/by_node_category.html', data=q.all())

@alarmview.route('/alarms/stats/by_node_vendor')
def stats_by_node_vendor():
    q = db.session.query(func.count(Alarm.id), Vendor.id, Vendor.alias)
    q = q.outerjoin(Node, Alarm.node_id == Node.id)
    q = q.outerjoin(Vendor, Node.vendor_id == Vendor.id)
    q = q.group_by(Vendor.id, Vendor.alias).order_by(Vendor.id)
    if not current_user.is_province_user:
        q = q.outerjoin(Area, Node.area_id==Area.id).filter(current_user.domain.clause_permit)
    return render_template('alarms/stats/by_node_vendor.html', data=q.all())


@alarmview.route('/nvd3/')
def nvd3_demo():
    data = [{'label': u'严重', 'value': 10},
            {'label': u'清除', 'value': 20}]
    return render_template('alarms/nvd3_demo.html', data=data)
    
navbar.add('alarms', u'故障', 'warning-sign', '/alarms/console/')

dashboard.add_widget('alarms_stats_by_last10', u'最近10条告警', url='/alarms/stats/by_last10')
dashboard.add_widget('alarms_stats_by_severity', u'告警概况', url='/alarms/stats/by_severity')
dashboard.add_widget('alarms_stats_by_category', u'告警分类', url='/alarms/stats/by_category')

dashboard.add_widget('alarms_stats_by_class', u'告警类型', url='/alarms/stats/by_class')
dashboard.add_widget('alarms_stats_by_node_category', u'设备告警', url='/alarms/stats/by_node_category')
dashboard.add_widget('alarms_stats_by_node_vendor', u'厂商告警', url='/alarms/stats/by_node_vendor')

activestats = Dashboard('activestats')
activestats.add_widget('alarms_stats_by_severity', u'告警概况', url='/alarms/stats/by_severity')
activestats.add_widget('alarms_stats_by_category', u'告警分类', url='/alarms/stats/by_category')

activestats.add_widget('alarms_stats_by_class', u'告警类型', url='/alarms/stats/by_class', column='side')
activestats.add_widget('alarms_stats_by_node_category', u'设备告警', url='/alarms/stats/by_node_category', column='side')
activestats.add_widget('alarms_stats_by_node_vendor', u'厂商告警', url='/alarms/stats/by_node_vendor', column='side')

alarmconsole = Dashboard('alarmconsole')
alarmconsole.add_widget('alarms_console_all', u'全部告警', url='/alarms/console/all')
alarmconsole.add_widget('alarms_console_lasthour', u'最近1小时告警', url='/alarms/console/lasthour', column='side')
alarmconsole.add_widget('alarms_console_status', u'状态告警', url='/alarms/console/category_status')
alarmconsole.add_widget('alarms_console_perf', u'性能告警', url='/alarms/console/category_perf', column='side')
alarmconsole.add_widget('alarms_console_system', u'网管自身告警', url='/alarms/console/category_system')
