#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import menus, Menu

from tango.login import current_user, login_required

nodeview = Blueprint('nodes', __name__)

@nodeview.route('/nodes')
@login_required
def nodes():
    nodes = get_nodes(current_user)
    return render_template('nodes/index.html', nodes = nodes)

def get_nodes(user):
    return user.nodes

menus.append(Menu('nodes', u'资源', '/nodes'))
