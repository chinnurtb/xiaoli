#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import (Blueprint, request, url_for, redirect,
                   render_template, flash)

from tango import db, user_profile
from tango.base import make_table
from tango.models import Setting, DictCode
from tango.ui import menus, Menu
from tango.ui.tables import TableConfig

from nodes.models import NodeHost
from users.models import User

from .models import OperationLog, SecurityLog, SubSystem
from .tables import (SettingTable, OperationLogTable, SecurityLogTable,
                     DictCodeTable, NodeHostTable, SubSystemTable)
from .forms import (SettingEditForm, SearchForm, OplogFilterForm,
                    DictCodeNewEditForm, NodeHostEditForm)

sysview = Blueprint('system', __name__)

@sysview.route('/system/')
@sysview.route('/system/settings/')
def settings():
    #TODO: SettingTable()
    query = Setting.query
    table = SettingTable(query)
    profile = user_profile(SettingTable._meta.profile)
    TableConfig(request, profile).configure(table)
    
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
    return render_template('/system/settings/edit.html', form=form,
                           action=url_for('syste.settings_edit', id=id))

    
@sysview.route('/dict_codes')
def dict_codes():
    profile = user_profile(DictCodeTable._meta.profile)
    table = DictCodeTable(DictCode.query)
    TableConfig(request, profile).configure(table)
    return render_template('/system/dict_codes/index.html', table=table)
    

@sysview.route('/dict_codes/new', methods=('GET', 'POST'))
def dict_codes_new():
    form = DictCodeNewEditForm()
    if form.is_submitted and form.validate_on_submit():
        dict_code = DictCode()
        form.populate_obj(dict_code)
        db.session.add(dict_code)
        db.session.commit()
        flash(u'%s 添加成功' % dict_code.code_label, 'success')
        return redirect('/dict_codes')

    return render_template('/system/dict_codes/new_edit.html', form=form,
                           action='/dict_codes/new')


@sysview.route('/dict_code/edit/<int:id>', methods=('GET', 'POST'))
def dict_codes_edit(id):
    dict_code = DictCode.query.get_or_404(id)
    form = DictCodeNewEditForm()
    
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(dict_code)
        db.session.commit()
        flash(u'%s 修改成功' % dict_code.code_label, 'success')
        return redirect('/dict_codes')
        
    form.process(obj=dict_code)
    return render_template('/system/dict_codes/new_edit.html', form=form,
                           action=url_for('system.dict_codes_edit', id=id))

    
@sysview.route('/timeperiods')
def timeperiods():
    # :HOLD
    return render_template('/system/timeperiods/index.html')

@sysview.route('/timeperiods/new')
def timeperiods_new():
    # :HOLD
    return render_template('/system/timeperiods/new.html')


##  Hosts
@sysview.route('/hosts/')
def hosts():
    profile = user_profile(NodeHostTable._meta.profile)
    table = NodeHostTable(NodeHost.query)
    TableConfig(request, profile).configure(table)
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
    return render_template("/system/hosts/edit.html", form=form,
                           action=url_for('system.hosts_edit', id=id))

    
@sysview.route('/subsystems', methods=['GET'])
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
        
    table = OperationLogTable(query)
    profile = user_profile(OperationLogTable._meta.profile)
    TableConfig(request, profile).configure(table)
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
    table = SecurityLogTable(query)
    profile = user_profile(SecurityLogTable._meta.profile)
    TableConfig(request, profile).configure(table)
    return render_template('/system/seclogs.html', 
        table=table, searchForm = searchForm)

menus.append(Menu('system', u'系统', '/system'))

