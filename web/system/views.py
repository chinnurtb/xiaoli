#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.login import login_required,current_user
from tango.ui import menus, Menu
from tango.models import Profile
from .models import OperationLog
from .tables import OperationLogTable

sysview = Blueprint('system', __name__)

@sysview.route('/system')
@sysview.route('/oplogs/')
def oplogs():
    profile = Profile.load(current_user.id, OperationLogTable._meta.profile_grp)
    order_by = request.args.get('order_by', None)
    page = int(request.args.get('page',1))
    query = OperationLog.query
    table = OperationLogTable(query).configure(profile, page=page, order_by=order_by)
    return render_template('/system/oplogs.html', table=table)

@sysview.route('/seclogs/')
def seclogs():
    return render_template('/system/seclogs.html')

menus.append(Menu('system', u'系统', '/system'))

