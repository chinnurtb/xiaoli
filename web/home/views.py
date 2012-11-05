#!/usr/bin/env python  
# coding: utf-8

from flask import Blueprint, request, render_template

from tango import user_profile

from tango.ui import navbar, dashboard

homeview = Blueprint('home', __name__)

@homeview.context_processor
def inject_navid():
    return dict(navid = 'home')

@homeview.route('/')
def index():
    dashboard.configure(user_profile('dashboard'))
    return render_template('/index.html', dashboard = dashboard)

@homeview.route('/timeline')
def timeline():
    return render_template('/timeline.html', events=[])

navbar.add('home', u'首页', 'home', '/')

