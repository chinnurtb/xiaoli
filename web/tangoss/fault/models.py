#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango import db

class Event(db.Model):
    __tablename__ = 'events'
    id = db.Column(db.Integer, primary_key=True)
    node_id = db.Column(db.Integer, db.ForeignKey('nodes.id'))
    severity = db.Column(db.Integer)
    title = db.Column(db.String(100))
    body = db.Column(db.String(200))
    sensor = db.Column(db.String(60))
    raised_at = db.Column(db.DateTime)

    def __repr__(self):
        return '<Event %r>' % self.title
