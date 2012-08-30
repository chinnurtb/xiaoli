#!/usr/bin/env python
# -*- coding: utf-8 -*-

from tango import db

from tangoss.fault import Event

class Node(db.Model):
    __tablename__ = 'nodes'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(60), unique = True)
    alias = db.Column(db.String(40))
    created_at = db.Column(db.DateTime)
    events = db.relationship('Event', backref='node')

    def __repr__(self):
        return '<Node %r>' % self.name
