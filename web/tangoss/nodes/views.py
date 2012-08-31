#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import db

from tango.ui import menus, Menu

from tango.ui import add_widget, Widget

from tango.login import current_user, login_required

from .models import Node

from tango.ui import tables

nodeview = Blueprint('nodes', __name__)

class NodeTable(tables.Table):
    check       = tables.CheckBoxColumn()
    status      = tables.Column(verbose_name=u'状态')
    alias       = tables.Column(verbose_name=u'名称', orderable=True)
    addr        = tables.Column(verbose_name=u'地址')

    class Meta():
        model = Node 
        per_page = 30
        order_by = '-alias'

@nodeview.route('/nodes')
@login_required
def nodes():
    table = NodeTable(Node.query, request)
    return render_template('nodes/index.html', table = table)

menus.append(Menu('nodes', u'资源', '/nodes'))

#col2
add_widget(Widget('dashboard3', 'Dashboard3', content='<div style="height:100px">Dashboard3</div>', column = 'side'))
add_widget(Widget('dashboard4', 'Dashboard4', content='<div style="height:100px">Dashboard4</div>', column = 'side'))
add_widget(Widget('dashboard5', 'Dashboard5', content='<div style="height:100px">Dashboard5</div>', column = 'side'))


