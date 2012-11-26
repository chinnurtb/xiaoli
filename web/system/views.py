# coding: utf-8

from flask import (Blueprint, request, url_for, redirect,
                   render_template, flash, json)

from tango import db, cache
from tango.ui.tables import make_table
from tango.models import Setting, DictCode, Category
from tango.ui import navbar

from nodes.models import NodeHost
from nodes.tables import NodeHostTable
from users.models import User

from alarms.models import AlarmSeverity

from .models import Threshold, Metric

from .forms import ThresholdEditForm, ThresholdNewForm, MetricNewEditForm

from .models import OperationLog, SecurityLog, SubSystem, TimePeriod

from .tables import MetricTable, ThresholdTable
from .tables import (SettingTable, OperationLogTable, SecurityLogTable,
                     DictCodeTable, SubSystemTable, TimePeriodTable)
from .forms import (SettingEditForm, SearchForm, OplogFilterForm, DictCodeFilterForm,
                    DictCodeNewEditForm, NodeHostEditForm, TimePeriodNewEditForm)

sysview = Blueprint('system', __name__)

@sysview.context_processor
def inject_navid():
    return dict(navid = 'system')

# ==============================================================================
#  系统设置
# ==============================================================================
@sysview.route('/system/')
@sysview.route('/system/settings/')
def settings():
    table = make_table(Setting.query, SettingTable)
    return render_template('/system/settings/index.html', table=table)
    
    
@sysview.route('/system/setting/edit/<int:id>', methods=('GET', 'POST'))
def settings_edit(id):
    form = SettingEditForm()
    setting = Setting.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        old_value = setting.value
        setting.value = form.value.data
        db.session.commit()
        cache.delete(setting.mod+'.'+setting.name)
        flash(u'%s 被修改:(%s)--> %s' % (setting.name, old_value, form.value.data), 'success')
        return redirect('/system/settings/')
    form.process(obj=setting)
    return render_template('/system/settings/edit.html', form=form, setting=setting)


# ==============================================================================
#  字典管理
# ==============================================================================    
@sysview.route('/dict-codes/')
def dict_codes():
    form = DictCodeFilterForm(formdata=request.args)
    query = DictCode.query
    if form.type.data:
        query = query.filter_by(type_id=form.type.data.id)
    if form.is_valid.data:
        query = query.filter_by(is_valid=form.is_valid.data)
        
    table = make_table(query, DictCodeTable)
    return render_template('/system/dict-codes/index.html', table=table, form=form)
    

@sysview.route('/dict-codes/new', methods=('GET', 'POST'))
def dict_codes_new():
    form = DictCodeNewEditForm()
    if form.is_submitted and form.validate_on_submit():
        dict_code = DictCode()
        form.populate_obj(dict_code)
        db.session.add(dict_code)
        db.session.commit()
        flash(u'字典(%s)添加成功' % dict_code.code_label, 'success')
        return redirect('/dict-codes/')
        
    return render_template('/system/dict-codes/new_edit.html', form=form,
                           action='/dict-codes/new', title=u'添加字典')


@sysview.route('/dict-codes/edit/<int:id>', methods=('GET', 'POST'))
def dict_codes_edit(id):
    dict_code = DictCode.query.get_or_404(id)
    form = DictCodeNewEditForm()
    
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(dict_code)
        db.session.commit()
        flash(u'字典(%s)修改成功' % dict_code.code_label, 'success')
        return redirect('/dict-codes/')
        
    form.process(obj=dict_code)
    return render_template('/system/dict-codes/new_edit.html', form=form,
                           action=url_for('system.dict_codes_edit', id=id), title=u'修改字典')
    

# ==============================================================================
#  阀值管理
# ==============================================================================
@sysview.route('/')
@sysview.route('/thresholds/')
def thresholds():
    query = Threshold.query
    form = SearchForm(formdata=request.args)
    keyword = form.keyword.data
    if keyword and keyword != '':
        ikeyword = '%' + keyword + '%'
        query = query.filter(db.or_(Threshold.name.ilike(ikeyword),
                                    Threshold.alias.ilike(ikeyword),
                                    Threshold.category.has(Category.alias.ilike(ikeyword)),
                                    Threshold.summary.ilike(ikeyword)))
    table = make_table(query, ThresholdTable)
    return render_template("system/thresholds/index.html",
                            filterForm = form, table=table)

    
@sysview.route('/thresholds/new', methods=['GET', 'POST'])
def thresholds_new():
    form = ThresholdNewForm()
    if form.is_submitted and form.validate_on_submit():
        threshold = Threshold()
        form.populate_obj(threshold)
        db.session.add(threshold)
        db.session.commit()
        
        flash(u'阀值(%s)添加成功' % threshold.name, 'success')
        return redirect(url_for('system.thresholds'))
        
    return render_template("system/thresholds/new.html", form=form, )
    
    
@sysview.route('/thresholds/edit/<int:id>', methods=['GET', 'POST'])
def thresholds_edit(id):
    form = ThresholdEditForm()
    threshold = Threshold.query.get_or_404(id)
    
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(threshold)
        db.session.commit()
        flash(u'阀值(%s)修改成功' % threshold.name, 'success')
        return redirect(url_for('system.thresholds'))
        
    form.process(obj=threshold)
    return render_template("system/thresholds/edit.html", form=form, id=id)

# ==============================================================================
#  指标管理
# ==============================================================================    
@sysview.route('/metrics/')
def metrics():
    query = Metric.query
    form = SearchForm(formdata=request.args)
    keyword = form.keyword.data
    if keyword and keyword != '':
        ikeyword = '%' + keyword + '%'
        query = query.filter(db.or_(Metric.name.ilike(ikeyword),
                                    Metric.grp.ilike(ikeyword),
                                    Metric.alias.ilike(ikeyword)))
    table = make_table(query, MetricTable)
    return render_template('system/metrics/index.html',
                            filterForm=form, table=table)
    
@sysview.route('/metrics/new', methods=['GET', 'POST'])
def metrics_new():
    form = MetricNewEditForm()
    if form.is_submitted and form.validate_on_submit():
        metric = Metric()
        form.populate_obj(metric)
        db.session.add(metric)
        db.session.commit()
        flash(u'指标 (%s) 添加成功!' % metric.alias, 'success')
        return redirect(url_for('system.metrics'))
        
    return render_template('system/metrics/new-edit.html', form=form,
                           action=url_for('system.metrics_new'), title=u'添加指标')

    
@sysview.route('/metrics/edit/<int:id>', methods=['GET', 'POST'])
def metrics_edit(id):
    form = MetricNewEditForm()
    metric = Metric.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(metric)
        db.session.commit()
        flash(u'指标 (%s) 修改成功' % metric.alias, 'success')
        return redirect(url_for('system.metrics'))
        
    form.process(obj=metric)
    return render_template('system/metrics/new-edit.html', form=form,
                           action=url_for('system.metrics_edit', id=id), title=u'修改指标')


@sysview.route('/metric/delete/<int:id>', methods=['GET', 'POST'])
def metrics_delete(id):
    metric = Metric.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(metric)
        db.session.commit()
        flash(u'指标 (%s) 删除成功!' % metric.alias, 'success')
        return redirect(url_for('system.metrics'))
        
    kwargs = {
        'title' : u'删除指标',
        'action': url_for('system.metrics_delete', id=id),
        'fields': [(u'名称', metric.name), (u'显示名', metric.alias)],
        'type'  : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)
    
# ==============================================================================
#  采集规则管理
# ==============================================================================    
@sysview.route('/timeperiods/')
def timeperiods():
    query = TimePeriod.query
    form = SearchForm(formdata=request.args)
    keyword = form.keyword.data
    if keyword and keyword != '':
        ikeyword = '%' + keyword + '%'
        query = query.filter(db.or_(TimePeriod.name.ilike(ikeyword),
                                    TimePeriod.alias.ilike(ikeyword)))
    table = make_table(query, TimePeriodTable)
    return render_template('/system/timeperiods/index.html',
                            filterForm = form, table=table)


@sysview.route('/timeperiods/new', methods=['GET', 'POST'])
def timeperiods_new():
    form = TimePeriodNewEditForm()
    if form.is_submitted and form.validate_on_submit():
        timeperiod = TimePeriod()
        form.populate_obj(timeperiod)
        db.session.add(timeperiod)
        db.session.commit()
        flash(u'规则添加成功!', 'success')
        return redirect(url_for('system.timeperiods'))
        
    return render_template('/system/timeperiods/new-edit.html', form=form,
                           action=url_for('system.timeperiods_new'), title=u'添加规则')

    
@sysview.route('/timeperiods/edit/<int:id>', methods=['GET', 'POST'])
def timeperiods_edit(id):
    form = TimePeriodNewEditForm()
    timeperiod = TimePeriod.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(timeperiod)
        db.session.commit()
        flash(u'修改成功!', 'success')
        return redirect(url_for('system.timeperiods'))
        
    form.process(obj=timeperiod)
    return render_template('/system/timeperiods/new-edit.html', form=form,
                           action=url_for('system.timeperiods_edit', id=id), title=u'修改规则')


# ==============================================================================
#  日志管理
# ==============================================================================
@sysview.route('/oplogs/')
def oplogs():
    query = OperationLog.query
    filterForm = OplogFilterForm(formdata=request.args)
    keyword = filterForm.keyword.data
    if keyword and keyword != '':
        keyword = keyword.strip()
        query = query.filter(db.or_(
                        OperationLog.terminal_ip.ilike('%'+keyword+'%'),
                        OperationLog.summary.ilike('%'+keyword+'%')))
    user = filterForm.uid.data
    if user :
        query = query.filter(OperationLog.uid == user.id)
    ip = filterForm.ip.data
    if ip:
        query = query.filter(OperationLog.terminal_ip == ip)
    start_date = filterForm.start_date.data
    if start_date:
        query = query.filter(OperationLog.created_at >= start_date)
    end_date = filterForm.end_date.data
    if end_date:
        query = query.filter(OperationLog.created_at <= end_date)

    table = make_table(query, OperationLogTable)
    return render_template('/system/oplogs.html', 
        table=table, filterForm=filterForm)
    
@sysview.route('/seclogs/')
def seclogs():
    query = SecurityLog.query
    form = SearchForm(formdata=request.args)
    keyword = form.keyword.data
    if keyword and keyword != '':
        keyword = keyword.strip()
        query = query.filter(db.or_(
            SecurityLog.terminal_ip.ilike('%'+keyword+'%'),
            SecurityLog.user.has(User.username.ilike('%'+keyword+'%'))))
    table = make_table(query, SecurityLogTable)
    return render_template('/system/seclogs.html',
                            filterForm=form, table=table)

# ==============================================================================
#  网管系统
# ==============================================================================    
@sysview.route('/hosts/')
def hosts():
    table = make_table(NodeHost.query, NodeHostTable)
    return render_template("/system/hosts/index.html", table=table)
    

@sysview.route('/hosts/edit/<int:id>', methods=['GET', 'POST'])
def hosts_edit(id):
    host = NodeHost.query.get_or_404(id)
    form = NodeHostEditForm()
    
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(host)
        db.session.commit()
        flash(u'%s 修改成功' % host.name, 'success')
        return redirect('/hosts/')
        
    form.process(obj=host)
    return render_template("/system/hosts/edit.html", form=form, host=host)

    
@sysview.route('/subsystems/', methods=['GET'])
def subsystems():
    table = make_table(SubSystem.query, SubSystemTable)
    return render_template('/system/subsystems.html', table=table)


navbar.add('system', u'系统', 'wrench', '/system')

