#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango import db

class Alarm(db.Model):
    __tablename__ = 'alarms'
    id = db.Column(db.Integer, primary_key=True)
    node_id = db.Column(db.Integer, db.ForeignKey('nodes.id'))
    severity = db.Column(db.Integer)
    summary = db.Column(db.String(255))

    def __repr__(self):
        return '<Alarm%r>' % self.alarm_alias
