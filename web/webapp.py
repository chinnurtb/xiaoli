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

from tango import db, menus, login_mgr

from tango.login import current_user

from tangoss.users.models import User

app = Flask(__name__)

app.config.from_pyfile('webapp.cfg')

db.init_app(app)

login_mgr.login_view = "tangoss.users.views.login"

login_mgr.login_message = u"Please log in to access this page."

login_mgr.refresh_view = "tangoss.users.views.reauth"

@login_mgr.user_loader
def load_user(id):
    return User.query.get(int(id))

login_mgr.init_app(app)

from tangoss.home.views import homeview
from tangoss.topo.views import topoview
from tangoss.nodes.views import nodeview
from tangoss.fault.views import faultview
from tangoss.perf.views import perfview
from tangoss.report.views import reportview
from tangoss.users.views import userview
from tangoss.system.views import sysview

blueprints = [homeview,
              topoview,
              nodeview,
              faultview,
              perfview,
              reportview,
              userview,
              sysview]
 
for bp in blueprints: 
    app.register_blueprint(bp)

@app.route('/')
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

