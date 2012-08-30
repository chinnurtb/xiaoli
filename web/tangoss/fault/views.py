#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.login import login_required

from tango import menus, Menu

from .models import Event

faultview = Blueprint("fault", __name__, url_prefix='/fault')

@faultview.route('/')
@login_required
def index():
    return render_template("/fault/index.html")

menus.append(Menu('fault', u'故障', '/fault'))

