#!/usr/bin/env python  
#coding=utf-8

from tango import db

class Metric(db.Model):
    __tablename__ = 'metrics'
    id     = db.Column(db.Integer, primary_key=True)
    grp    = db.Column(db.String(60))
    name   = db.Column(db.String(60))
    alias  = db.Column(db.String(100))
    calc   = db.Column(db.String(60))
    unit   = db.Column(db.String(200))
    format = db.Column(db.String(200))
    descr  = db.Column(db.String(200))
    

class Threshold(db.Model):
    __tablename__ = 'thresholds'
    
    id             = db.Column(db.Integer, primary_key=True)
    category_id    = db.Column(db.Integer, db.ForeignKey('categories.id'))
    metric_id      = db.Column(db.Integer, db.ForeignKey('metrics.id'))
    name           = db.Column(db.String(60))
    alias          = db.Column(db.String(100))
    enabled        = db.Column(db.Integer, default=1)
    alarm_class_id = db.Column(db.Integer, db.ForeignKey('alarm_classes.id'))
    occur_count    = db.Column(db.Integer, default=1)
    summary        = db.Column(db.String(200))
    
    occur_cond1    = db.Column(db.String(60))
    restore_cond1  = db.Column(db.String(60))
    severity1      = db.Column(db.Integer, default=4)
    
    occur_cond2    = db.Column(db.String(60))
    restore_cond2  = db.Column(db.String(60))
    severity2      = db.Column(db.Integer, default=2)

    category    = db.relationship('Category')
    metric      = db.relationship('Metric')
    alarm_class = db.relationship('AlarmClass')

    
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










