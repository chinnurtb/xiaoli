#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.ui import navbar

reportview = Blueprint('report', __name__, url_prefix='/report')

@reportview.context_processor
def inject_navid():
    return dict(navid = 'report')

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

navbar.add('report', u'报表', '/report')

