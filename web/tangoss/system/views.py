#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.login import login_required

from tango import menus, Menu

sysview = Blueprint('system', __name__)

@sysview.route('/system')
def index():
    return render_template("system/index.html")

menus.append(Menu('system', u'系统', '/system'))
