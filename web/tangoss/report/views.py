#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import menus, Menu

reportview = Blueprint('report', __name__, url_prefix='/report')

@reportview.route('/')
def index():
    return render_template("report/index.html")

menus.append(Menu('report', u'报表', '/report'))


