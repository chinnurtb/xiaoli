#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.ui import menus, Menu

from tango.ui import add_widget, Widget

reportview = Blueprint('report', __name__, url_prefix='/report')

@reportview.route('/')
def index():
    #TODO:
    return render_template("report/index.html")

@reportview.route('/nodes')
def nodes_report():
    #TODO:
    return render_template("report/index.html")

@reportview.route('/perf')
def perf_report():
    #TODO:
    return render_template('report/index.html')

@reportview.route('/alarms')
def alarms_report():
    #TODO:
    return render_template('report/index.html')

menus.append(Menu('report', u'报表', '/report'))

#col3
add_widget(Widget('dashboard6', 'Dashboard6', content='<div style="height:100px">Dashboard6</div>', column = 'column3'))
add_widget(Widget('dashboard7', 'Dashboard7', content='<div style="height:100px">Dashboard7</div>', column = 'column3'))
add_widget(Widget('dashboard8', 'Dashboard8', content='<div style="height:100px">Dashboard8</div>', column = 'column3'))

