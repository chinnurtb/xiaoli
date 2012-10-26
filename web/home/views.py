#!/usr/bin/env python  
# -*- coding: utf-8 -*-

import re

from tango import db

from flask import Blueprint, request, render_template

from tango import user_profile

from tango.ui import navbar, dashboard

from tango.login import login_required, current_user

from tango.models import Profile

homeview = Blueprint('home', __name__)

@homeview.context_processor
def inject_navid():
    return dict(navid = 'home')

def nested_dict(name, form):
    dict = {}
    pattern = "^%s\[(.+)\]$" % name
    for key in form.keys():
        m = re.match(pattern, key)
        if m:
            dict[m.group(1)] = form[key]
    return dict

@homeview.route('/')
def index():
    dashboard.configure(user_profile('dashboard'))
    return render_template('/index.html', dashboard = dashboard)

@homeview.route('/dashboard/settings', methods = ['POST'])
def setting():
    form = request.form
    uid = current_user.id
    if form['action'] == 'meta-box-order' and form['page'] == 'dashboard':
        order = nested_dict('order', form)
        layout = form['page_columns']
        Profile(uid, 'dashboard', 'dashboard.box.order', str(order)).update()
        Profile(uid, 'dashboard', 'dashboard.screen.layout', layout).update()
        db.session.commit()
    elif form['action'] == 'closed-postboxes' and form['page'] == 'dashboard':
        Profile(uid, 'dashboard', 'dashboard.closedbox', form['closed']).update()
        Profile(uid, 'dashboard', 'dashboard.metaboxhidden', form['hidden']).update()
        db.session.commit()
    elif form['action'] == 'update-welcome-panel':
        Profile(uid, 'dashboard', 'dashboard.welcome.panel', form['visible']).update()
        db.session.commit()

    return '0'

@homeview.route('/timeline')
def timeline():
    return render_template('/timeline.html', events=[])

navbar.add('home', u'首页', '/')

