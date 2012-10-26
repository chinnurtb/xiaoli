#!/usr/bin/env python
# -*- coding: utf-8 -*-
from tango import db
from datetime import datetime
from sqlalchemy.ext.hybrid import hybrid_property

class TimePeriod(db.Model):
    """采集规则"""
    __tablename__ = 'timeperiods'
    
    id          = db.Column(db.Integer, primary_key=True)
    name        = db.Column(db.String(100))
    alias       = db.Column(db.String(100))
    minute      = db.Column(db.String(100))
    _hour       = db.Column('hour', db.String(60), default='*')
    _dayofmonth = db.Column('dayofmonth', db.String(100), default='*')
    _month      = db.Column('month', db.String(40), default='*')
    _dayofweek  = db.Column('dayofweek', db.String(40), default='*')
    start_at    = db.Column(db.DateTime)
    end_at      = db.Column(db.DateTime)
    state_in    = db.Column(db.Integer)
    state_out   = db.Column(db.Integer)
    status      = db.Column(db.Integer)
    remark      = db.Column(db.String(200))
    created_at  = db.Column(db.DateTime)
    updated_at  = db.Column(db.DateTime)

    
    @property
    def hour(self):
        return self._hour.split(',')

    @hour.setter
    def hour(self, lst):
        self._hour = ','.join(lst)

    @property
    def dayofmonth(self):
        return self._dayofmonth.split(',')

    @dayofmonth.setter
    def dayofmonth(self, lst):
        self._dayofmonth = ','.join(lst)

    @property
    def month(self):
        return self._month.split(',')

    @month.setter
    def month(self, lst):
        self._month = ','.join(lst)

    @property
    def dayofweek(self):
        return self._dayofweek.split(',')

    @dayofweek.setter
    def dayofweek(self, lst):
        self._dayofweek = ','.join(lst)
        
        
class OperationLog(db.Model):
    __tablename__ = 'oplogs'
    id          = db.Column(db.Integer, primary_key=True)
    session     = db.Column(db.String(50))
    uid         = db.Column(db.Integer, db.ForeignKey("users.id"))
    action      = db.Column(db.String(200))
    success     = db.Column(db.Integer)
    summary     = db.Column(db.String(200))
    module      = db.Column(db.String(200))
    terminal_ip = db.Column(db.String(20))
    created_at  = db.Column(db.DateTime, default=datetime.now)
    updated_at  = db.Column(db.DateTime, default=datetime.now)

    user = db.relationship("User")

    @hybrid_property
    def oper_obj(self):
        return self.module + self.summary

class SecurityLog(db.Model):
    __tablename__ = 'seclogs'
    id          = db.Column(db.Integer, primary_key=True)
    session     = db.Column(db.String(50))
    uid         = db.Column(db.Integer, db.ForeignKey("users.id"))
    success     = db.Column(db.Integer)
    summary     = db.Column(db.String(200))
    terminal_ip = db.Column(db.String(20))
    login_at  = db.Column(db.DateTime)
    logout_at  = db.Column(db.DateTime)

    user = db.relationship("User")

    @hybrid_property
    def time(self):
        if u'登录' in self.summary:
            return self.login_at
        else:
            return self.logout_at


class SubSystem(db.Model):
    """ 子采集 """

    __tablename__ = 'subsystems'
    
    id         = db.Column(db.Integer, primary_key=True)
    rdn        = db.Column(db.String(100))
    name       = db.Column(db.String(100))
    alias      = db.Column(db.String(100))
    host       = db.Column(db.String(100))
    status     = db.Column(db.Integer)
    descr      = db.Column(db.String(200))
    started_at = db.Column(db.DateTime) # 启动采集的时间
    updated_at = db.Column(db.DateTime, onupdate=datetime.now)
 
