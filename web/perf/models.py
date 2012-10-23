#!/usr/bin/env python  
#coding=utf-8

from tango import db

class Metric(db.Model):
    __tablename__ = 'metrics'
    id       = db.Column(db.Integer, primary_key=True)

class Threshold(db.Model):
    __tablename__ = 'thresholds'
    id       = db.Column(db.Integer, primary_key=True)


class Miboid(db.Model):
    __tablename__ = 'miboids'

    id       = db.Column(db.Integer, primary_key=True)
    mib      = db.Column(db.String(100))
    grp      = db.Column(db.String(40))
    name     = db.Column(db.String(100))
    oid      = db.Column(db.String(100))
    is_valid = db.Column(db.Integer)
    alias    = db.Column(db.String(100))
    remark   = db.Column(db.String(100))










