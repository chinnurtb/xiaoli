#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.login import login_required

from tango.ui import menus, Menu

from tango.ui import Widget, add_widget

from .models import Alarm

faultview = Blueprint("fault", __name__, url_prefix='/fault')

@faultview.route('/')
@login_required
def index():
    return render_template("/fault/index.html")

menus.append(Menu('fault', u'故障', '/fault'))

add_widget(Widget('event_summary', u'告警统计', url = '/widgets/alarm/summary'))
add_widget(Widget('event_statistics', u'告警概要', content='<div style="height:100px">......</div>'))
add_widget(Widget('dashboard1', 'Dashboard1', ''))
add_widget(Widget('dashboard2', 'Dashboard2', ''))

