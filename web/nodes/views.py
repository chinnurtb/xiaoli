#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import db
from tango.ui import menus, Menu
from tango.ui import add_widget, Widget
from tango.login import current_user, login_required
from tango.models import Profile

from .models import Node, Board, Port
from .forms import NodeNewForm
from .tables import NodeTable,PortTable,BoardTable

nodeview = Blueprint('nodes', __name__)

@nodeview.route('/nodes')
@login_required
def nodes():
    profile = Profile.load(current_user.id, 'table-nodes')
    table = NodeTable(Node.query).configure(profile)
    return render_template('nodes/index.html', table = table)

@nodeview.route('/nodes/new')
@login_required
def node_new():
    form = NodeNewForm()
    return render_template('nodes/new.html', form = form)

@nodeview.route("/boards")
@login_required
def boards():
    profile = Profile.load(current_user.id, 'table-boards')
    table = BoardTable(Board.query).configure(profile)
    return render_template('boards/index.html', table = table)

@nodeview.route("/ports")
@login_required
def ports():
    profile = Profile.load(current_user.id, 'table-ports')
    table = PortTable(Port.query).configure(profile)
    return render_template('ports/index.html', table = table)

menus.append(Menu('nodes', u'资源', '/nodes'))

#col2
add_widget(Widget('dashboard3', 'Dashboard3', content='<div style="height:100px">Dashboard3</div>', column = 'side'))
add_widget(Widget('dashboard4', 'Dashboard4', content='<div style="height:100px">Dashboard4</div>', column = 'side'))
add_widget(Widget('dashboard5', 'Dashboard5', content='<div style="height:100px">Dashboard5</div>', column = 'side'))

