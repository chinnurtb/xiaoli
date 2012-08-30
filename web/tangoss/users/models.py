#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango import db

from hashlib import md5

from tango.models import Profile

from tango.login import UserMixin

from datetime import datetime

class User(db.Model, UserMixin):
    __tablename__ = 'users'
    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(80), unique=True)
    email = db.Column(db.String(120), unique=True)
    password = db.Column(db.String(60))
    signup_on = db.Column(db.DateTime)
    #profiles = db.relation('Profile', backref='user', lazy='dynamic')

    def __init__(self, username, email, password):
        self.username = username
        self.email = email
        self.password = password
        self.signup_on = datetime.now()

    def gravatar_url(self, size=80):
        """Return the gravatar image for the given email address."""
        return 'http://www.gravatar.com/avatar/%s?d=identicon&s=%d' % \
            (md5(self.email.strip().lower().encode('utf-8')).hexdigest(), size)

    def __repr__(self):
        return '<User %r>' % self.username

    def check_passwd(self, passwd):
        if not self.password:
            return False
        return self.password == md5(passwd).hexdigest()

    @classmethod
    def authenticate(clazz, login, passwd):
        user = clazz.query.filter(db.or_(User.username == login, 
                                    User.email == login)).first()
        if user:
            authenticated = user.check_passwd(passwd)
        else:
            authenticated = False

        return user, authenticated


class Role(db.Model):
    __tablename__ = 'roles'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(255), unique=True)
    description = db.Column(db.String(255))
    created_at = db.Column(db.DateTime)
    #users = db.relation('User', backref='role', lazy='dynamic')

    def __init__(self, name, description = ''):
        self.name = name
        self.description = description
        self.created_at = datetime.now()

    def __repr__(self):
        return '<Role%r>' % self.name
