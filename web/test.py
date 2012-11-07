#!/usr/bin/env python
#coding=utf-8

import os
import unittest
import tempfile
import flask
from flask import request

from webapp import app
from users.models import User

class XiaoliTestCase(unittest.TestCase):
    def setUp(self):
        self.db_fd, app.config['DATABASE'] = tempfile.mkstemp()
        app.config['TESTING'] = True
        self.app = app.test_client()

    def login(self, username, password):
        return self.app.post('/login', data=dict(
            username=username,
            password=password
        ), follow_redirects=True)        

    def tearDown(self):
        os.close(self.db_fd)
        os.unlink(app.config['DATABASE'])

    def test_stuff(self):
        self.login('root', 'public')
        rv = self.app.get('/')
        rv.data
        with app.test_client():
            print User.query.all() # !!!!! ***** See ***** !!!!! 

        
if __name__ == '__main__':
    unittest.main()
