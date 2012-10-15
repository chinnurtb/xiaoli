#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import user_profile
from tango.login import login_required
from tango.ui import menus, Menu
from tango.models import Profile
from .models import OperationLog, SecurityLog
from .tables import OperationLogTable, SecurityLogTable
from tango.ui.tables import TableConfig

from .forms import SearchForm

sysview = Blueprint('system', __name__)

@sysview.route('/system/')
@sysview.route('/system/settings')
@login_required
def settings():
    #TODO: SettingTable()
    return render_template('/system/settings.html')

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
    searchForm = SearchForm(formdata=request.args)
    keyword = searchForm.keyword.data
    query = OperationLog.query
    print keyword
    if keyword and keyword != '':
        query = query.filter(OperationLog.summary.ilike('%'+keyword+'%'))
    table = OperationLogTable(query)
    profile = user_profile(OperationLogTable._meta.profile)
    TableConfig(request, profile).configure(table)
    return render_template('/system/oplogs.html', 
        table=table, searchForm=searchForm)

@sysview.route('/seclogs/')
@login_required
def seclogs():
    searchForm = SearchForm(formdata=request.args)
    keyword = searchForm.keyword.data
    query = SecurityLog.query
    print keyword
    if keyword and keyword != '':
        query = query.filter(SecurityLog.summary.ilike('%'+keyword+'%'))
    table = SecurityLogTable(query)
    profile = user_profile(SecurityLogTable._meta.profile)
    TableConfig(request, profile).configure(table)
    return render_template('/system/seclogs.html', 
        table=table, searchForm = searchForm)

menus.append(Menu('system', u'系统', '/system'))

