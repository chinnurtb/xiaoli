#!/usr/bin/env python
# -*- coding: utf-8 -*-

from tango import db

from tangoss.fault import Event

class Node(db.Model):
    __tablename__ = 'nodes'
    id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('users.id'))
    name = db.Column(db.String(60), unique = True)
    label = db.Column(db.String(40))
    apikey = db.Column(db.String(20))
    presence = db.Column(db.Integer)
    show = db.Column(db.String(40))
    created_at = db.Column(db.DateTime)
    events = db.relationship('Event', backref='node')

    def __repr__(self):
        return '<Node %r>' % self.name
