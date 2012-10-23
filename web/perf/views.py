#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.ui import menus, Menu

perfview = Blueprint('perf', __name__, url_prefix="/perf")

@perfview.route('/')
def index():
    return render_template("perf/index.html")

@perfview.route('/metrics/'
def metrics():
    return render_template('perf/metrics/index.html')

@perfview.route('/thresholds/')
def thresholds():
    return render_template("perf/thresholds/index.html")

menus.append(Menu('perf', u'性能', '/perf'))
