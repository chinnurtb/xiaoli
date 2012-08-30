#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from flask import Blueprint, render_template

from tango import menus, Menu

from tango.login import login_required, current_user

from tangoss.fault import Event

homeview = Blueprint('home', __name__)

@homeview.route('/dashboard')
@login_required
def dashboard():
    #current_user.profiles
    return render_template('/dashboard.html')

@homeview.route('/timeline')
@login_required
def timeline():
    events = get_events(current_user)
    return render_template('/timeline.html', events=events)

def get_events(user):
    nids = [node.id for node in user.nodes]
    q = Event.query.filter(Event.node_id.in_(nids))
    return q.order_by("raised_at desc").limit(50).all()

menus.append(Menu('dashboard', u'首页', '/dashboard'))

