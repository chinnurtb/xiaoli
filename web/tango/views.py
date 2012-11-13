#!/usr/bin/env python
#coding=utf-8

import re, os

from tango import db, update_profile

from flask import Blueprint, request, make_response, render_template, send_file

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


MODULE_TEXT_DICT = {
    'tango'  : u'公共',
    'home'   : u'首页',
    'topo'   : u'拓扑管理',
    'nodes'  : u'节点管理',
    'alarms' : u'故障管理',
    'perf'   : u'性能管理',
    'report' : u'报表管理',
    'users'  : u'用户管理',
    'system' : u'系统管理',
    'admin'  : u'后台管理',
}
    
OPERATIONS = {
    'new'             : u'新建',
    'edit'            : u'编辑',
    'delete'          : u'删除',
    ('delete', 'all') : u'批量删除',
}

def rebuild_permissions():
    from flask import current_app
    from users.models import Permission
    
    perm_records = []
    for rule in current_app.url_map.iter_rules():
        endpoint = rule.endpoint
        if endpoint in current_app.config['SAFE_ENDPOINTS']:
            print 'Safe>> ', endpoint
            continue
        if endpoint.find('.') == -1:
            raise ValueError('UnExcepted endpoint: %s' % endpoint)
            
        if Permission.query.filter_by(endpoint=endpoint).first():
            print 'Exist>> ', endpoint
            continue
            
        name = ''
        module = endpoint.split('.')[0]
        module_text = MODULE_TEXT_DICT[module]
        pieces = endpoint.split('_')
        operation = ''
        if len(pieces) > 1:
            if pieces[-1] in OPERATIONS:
                operation = OPERATIONS[pieces[-1]]
            elif tuple(pieces[-2:]) in OPERATIONS:
                operation = OPERATIONS[tuple(pieces[-2:])]
                
        p = Permission()
        p.name = name
        p.module_text = module_text
        p.operation = operation
        p.module = module
        p.endpoint = endpoint
        db.session.add(p)
        print 'Added>> ', endpoint, ' | ', operation
    db.session.commit()
        

@tangoview.route('/shell', methods=['GET', 'POST'])
def shell():
    rebuild_permissions()
    if current_user.username != 'root':
        return "Hey, It's Dangerous!"
    def utf8(s):
        return unicode(s, encoding='utf-8')
    if request.method == 'POST':
        command = request.form.get('command', 'None')
        from commands import getstatusoutput
        status, output = getstatusoutput(command)
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

@tangoview.route('/download', methods = ['get'])
def download():
    file = request.args.get("file")
    if file:
        root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..')
        file = os.path.join(root_path, *file.split("/"))
        return send_file(file,as_attachment=True)
