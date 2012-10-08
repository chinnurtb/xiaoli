#!/usr/bin/env python
# -*- coding: utf-8 -*-
from tango import db
from datetime import datetime
from sqlalchemy.ext.hybrid import hybrid_property

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

    user = db.relationship("User", backref="oplogs")

    @hybrid_property
    def oper_obj(self):
        return self.module + self.summary