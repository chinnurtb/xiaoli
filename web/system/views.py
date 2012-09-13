#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.login import login_required

from tango.ui import menus, Menu

sysview = Blueprint('system', __name__)

@sysview.route('/system')
def index():
    return render_template("system/index.html")

@sysview.route('/seclogs/')
def seclogs():
    return render_template('/system/seclogs.html')

@sysview.route('/oplogs/')
def oplogs():
    return render_template('/system/oplogs.html')

menus.append(Menu('system', u'系统', '/system'))

