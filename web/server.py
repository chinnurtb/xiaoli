#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from werkzeug.wsgi import DispatcherMiddleware

from werkzeug.serving import run_simple

from webapp import app

application = DispatcherMiddleware(app)

if __name__ == "__main__":
    run_simple('0.0.0.0', 8000, application, 
                use_reloader=True, use_debugger=True)     
