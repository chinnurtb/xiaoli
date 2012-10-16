#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import db, user_profile
from tango.login import login_required
from tango.ui import menus, Menu
from tango.models import Setting, Profile
from .models import OperationLog, SecurityLog
from .tables import SettingTable, OperationLogTable, SecurityLogTable
from tango.ui.tables import TableConfig

from users.models import User
from .forms import SettingEditForm, SearchForm, OplogFilterForm

sysview = Blueprint('system', __name__)

@sysview.route('/system/')
@sysview.route('/system/settings/')
@login_required
def settings():
    #TODO: SettingTable()
    query = Setting.query
    table = SettingTable(query)
    profile = user_profile(SettingTable._meta.profile)
    TableConfig(request, profile).configure(table)
    
    return render_template('/system/settings.html', table=table)

    
@sysview.route('/system/setting/edit/<int:id>', methods=('GET', 'POST'))
def setting_edit(id):
    form = SettingEditForm()
    setting = Setting.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        old_value = setting.value
        setting.value = form.value.data
        db.session.commit()
        flash(u'%s 被修改: %s --> %s' % (setting.name, str(old_value), str(form.value.data)), 'success')
        return redirect('/system/settings/')
    form.process(obj=setting)
    return render_template('/system/setting_edit.html', form=form, id=id)

    
@sysview.route('/dic_codes')
@login_required
def dic_codes():
    #TODO:
    #profile = user_profile(DicCodeTable._meta.profile)
    #table = DicCodeTable(DicCode.query)
    #TableConfig.configure(request, profile).configure(table)
    return render_template('/system/dic_codes.html')

@sysview.route('/dic_codes/new')
@login_required
def dic_codes_new():
    #TODO:
    #form = DicCodeNewForm(formdata=request.args)
    return render_template('/system/dic_codes_new.html')

@sysview.route('/timeperiods')
@login_required
def timeperiods():
    return render_template('/system/timeperiods.html')

@sysview.route('/timeperiods/new')
@login_required
def timeperiods_new():
    return render_template('/system/timeperiods_new.html')

@sysview.route('/hosts', methods=['GET'])
@login_required
def hosts():
    return render_template("/system/hosts.html")

@sysview.route('/subsystems', methods=['GET'])
@login_required
def subsystems():
    return render_template('/system/subsystems.html')
    
@sysview.route('/oplogs/')
@login_required
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
@login_required
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

