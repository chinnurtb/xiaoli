#!/usr/bin/env python  
# -*- coding: utf-8 -*-
"""
   XiaoLi
   ~~~~~~~ 

   Integrated Access Network Monitoring.
   :copyright: (c) 2012 by Ery Lee(ery.lee@gmail.com)
"""

from flask import Flask, session, url_for, redirect, \
    render_template, g

from tango.ui import menus

from tango import db, login_mgr

from tango.login import login_required, current_user

from users.models import User

app = Flask(__name__)

app.config.from_pyfile('webapp.cfg')

db.init_app(app)

login_mgr.login_view = "/login"

login_mgr.login_message = u"Please log in to access this page."

login_mgr.refresh_view = "/reauth"

@login_mgr.user_loader
def load_user(id):
    return User.query.get(int(id))

login_mgr.init_app(app)

from dashboard.views import homeview
from topo.views import topoview
from nodes.views import nodeview
from fault.views import faultview
from perf.views import perfview
from report.views import reportview
from users.views import userview
from system.views import sysview

blueprints = [homeview,
              #topoview,
              nodeview,
              faultview,
              #perfview,
              #reportview,
              userview,
              sysview]
 
for bp in blueprints: 
    app.register_blueprint(bp)

@app.route('/')
@login_required
def index():
    return render_template('index.html')

#FIXME
@app.before_request
def before_request():
    if current_user: 
        g.menus = menus

@app.errorhandler(404)
def page_not_found(e):
    return render_template('404.html'), 404

@app.errorhandler(500)
def internal_error(e):
    return render_template('500.html'), 500

if __name__ == "__main__":
    app.run(host='0.0.0.0', debug=True)

