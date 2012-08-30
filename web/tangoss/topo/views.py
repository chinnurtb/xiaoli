#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import menus, Menu

topoview = Blueprint('topo', __name__, url_prefix='/topo')

@topoview.route('/')
def index():
    return render_template("topo/index.html", menuid = 'topo')

menus.append(Menu('topo', u'拓扑', 'topo'))
