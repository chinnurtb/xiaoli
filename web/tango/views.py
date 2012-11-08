#!/usr/bin/env python

# coding: utf-8

import re

from tango import db, update_profile

from flask import Blueprint, request, make_response, render_template

from users.models import User

from tango.ui import tables

from tango.login import current_user

from tango.models import Profile

tangoview = Blueprint('tango', __name__)

# ==============================================================================
#  Charts
# ==============================================================================    
from .tdata import *
from tango.ui.charts.nvd3charts import *
from tango.ui.charts.highcharts import *

def nested_dict(name, form):
    dict = {}
    pattern = "^%s\[(.+)\]$" % name
    for key in form.keys():
        m = re.match(pattern, key)
        if m:
            dict[m.group(1)] = form[key]
    return dict


pwd = '.'    
@tangoview.route('/shell', methods=['GET', 'POST'])
def shell():
    if current_user.username != 'root':
        return "Hey, It's Dangerous!"
    def utf8(s):
        return unicode(s, encoding='utf-8')
    if request.method == 'POST':
        command = request.form.get('command', 'None')
        from commands import getstatusoutput
        global pwd
        status, output = getstatusoutput('cd %s;' % pwd +  command + ';pwd')
        pwd = output.split('\n')[-1]
        output = '\n'.join(output.split('\n')[:-1])
        output = u'$ %s\n========================================\n%s' % (command, utf8(output))
        if status == 0:
            return output
        else:
            return '[Bad Command: %s]' % command
    return render_template('shell.html')

    
@tangoview.route('/dashboard/settings', methods = ['POST'])
def dashboard_setting():
    form = request.form
    uid = current_user.id
    if form['action'] == 'meta-box-order': # and form['page'] == 'dashboard':
        order = nested_dict('order', form)
        layout = form['page_columns']
        update_profile(form['page'], 'box.order', str(order))
        update_profile(form['page'], 'screen.layout', layout)
        db.session.commit()
    elif form['action'] == 'closed-postboxes': # and form['page'] == 'dashboard':
        update_profile(form['page'], 'closedbox', form['closed'])
        update_profile(form['page'], 'metaboxhidden', form['hidden'])
        db.session.commit()
    elif form['action'] == 'update-welcome-panel':
        update_profile('dashboard', 'welcome.panel', form['visible'])
        db.session.commit()
    elif form['action'] == 'meta-page-refresh':
        update_profile(form['page'], 'page.refresh', form['refresh'])
        db.session.commit()

    return '0'

