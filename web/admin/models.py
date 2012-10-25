# coding: utf-8

from tango import db

class Module(db.Model):
    """采集模块表"""

    __tablename__ = 'modules'
    id          = db.Column(db.Integer, primary_key=True)
    name        = db.Column(db.String(20))
    alias       = db.Column(db.String(40))
    period      = db.Column(db.Integer)
    retries     = db.Column(db.Integer)
    timeout     = db.Column(db.Integer)
    remark      = db.Column(db.String(100))

class Monitor(db.Model):
    
    """监控器表"""
    
    __tablename__ = 'monitors'

    id          = db.Column(db.Integer, primary_key=True)
    category    = db.Column(db.String(20))
    vendor      = db.Column(db.String(20))
    sysoid      = db.Column(db.String(100))
    match       = db.Column(db.String(100))
    modid       = db.Column(db.Integer, db.ForeignKey('modules.id'))
    mib         = db.Column(db.String(20))
    remark      = db.Column(db.String(100))
    
    module      = db.relation('Module')

    
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


