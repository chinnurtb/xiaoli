# coding: utf-8

from flask import (Blueprint, request, url_for, redirect,
                   render_template, flash)

from tango import db
from tango.base import make_table
from tango.models import Setting, DictCode
from tango.ui import navbar

from nodes.models import NodeHost
from nodes.tables import NodeHostTable
from users.models import User

from .models import OperationLog, SecurityLog, SubSystem, TimePeriod
from .tables import (SettingTable, OperationLogTable, SecurityLogTable,
                     DictCodeTable, SubSystemTable, TimePeriodTable)
from .forms import (SettingEditForm, SearchForm, OplogFilterForm, DictCodeFilterForm,
                    DictCodeNewEditForm, NodeHostEditForm, TimePeriodNewEditForm)

sysview = Blueprint('system', __name__)

@sysview.context_processor
def inject_navid():
    return dict(navid = 'system')

@sysview.route('/system/')
@sysview.route('/system/settings/')
def settings():
    #TODO: SettingTable()
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
        flash(u'%s 被修改: %s --> %s' % (setting.name, str(old_value), str(form.value.data)), 'success')
        return redirect('/system/settings/')
    form.process(obj=setting)
    return render_template('/system/settings/edit.html', form=form, setting=setting)

    
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
        flash(u'%s 添加成功' % dict_code.code_label, 'success')
        return redirect('/dict-codes/')

    return render_template('/system/dict-codes/new_edit.html', form=form,
                           action='/dict-codes/new')


@sysview.route('/dict-codes/edit/<int:id>', methods=('GET', 'POST'))
def dict_codes_edit(id):
    dict_code = DictCode.query.get_or_404(id)
    form = DictCodeNewEditForm()
    
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(dict_code)
        db.session.commit()
        flash(u'%s 修改成功' % dict_code.code_label, 'success')
        return redirect('/dict-codes/')
        
    form.process(obj=dict_code)
    return render_template('/system/dict-codes/new_edit.html', form=form,
                           action=url_for('system.dict_codes_edit', id=id))

    
@sysview.route('/timeperiods/')
def timeperiods():
    table = make_table(TimePeriod.query, TimePeriodTable)
    return render_template('/system/timeperiods/index.html', table=table)


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
        flash(u'编辑成功!', 'success')
        return redirect(url_for('system.timeperiods'))
        
    form.process(obj=timeperiod)
    return render_template('/system/timeperiods/new-edit.html', form=form,
                           action=url_for('system.timeperiods_edit', id=id), title=u'编辑规则')


##  Hosts
@sysview.route('/hosts/')
def hosts():
    table = make_table(NodeHost.query, NodeHostTable)
    return render_template("/system/hosts/index.html", table=table)
    

@sysview.route('/hosts/edit/<int:id>', methods=['GET', 'POST'])
def hosts_edit(id):
    host = NodeHost.query.get_or_404(id)
    form = NodeHostEditForm()
    
    if form.is_submitted and form.validate_on_submit():
        alias = form.alias.data
        remark = form.remark.data
        host.alias = alias
        host.remark = remark
        db.session.commit()
        flash(u'%s 修改成功' % host.name, 'success')
        return redirect('/hosts/')
        
    form.process(obj=host)
    return render_template("/system/hosts/edit.html", form=form, host=host)

    
@sysview.route('/subsystems/', methods=['GET'])
def subsystems():
    table = make_table(SubSystem.query, SubSystemTable)
    return render_template('/system/subsystems.html', table=table)
    
@sysview.route('/oplogs/')
def oplogs():
    filterForm = OplogFilterForm(formdata=request.args)
    query = OperationLog.query
    
    user = filterForm.uid.data
    if user :
        query = query.filter(OperationLog.uid == user.id)
    start_date = filterForm.start_date.data
    if start_date:
        query = query.filter(OperationLog.created_at >= start_date)
    end_date = filterForm.end_date.data
    if end_date:
        query = query.filter(OperationLog.created_at <= end_date)
    keyword = filterForm.keyword.data
    if keyword and keyword != '':
        keyword = keyword.strip()
        query = query.filter(OperationLog.summary.ilike('%'+keyword+'%'))

    table = make_table(query, OperationLogTable)
    return render_template('/system/oplogs.html', 
        table=table, filterForm=filterForm)

    
@sysview.route('/seclogs/')
def seclogs():
    searchForm = SearchForm(formdata=request.args)
    keyword = searchForm.keyword.data
    query = SecurityLog.query
    if keyword and keyword != '':
        keyword = keyword.strip()
        query = query.filter(db.or_(
            SecurityLog.terminal_ip.ilike('%'+keyword+'%'),
            SecurityLog.user.has(User.username.ilike('%'+keyword+'%'))))
        
    table = make_table(query, SecurityLogTable)
    return render_template('/system/seclogs.html', 
        table=table, searchForm = searchForm)

navbar.add('system', u'系统', '/system')

