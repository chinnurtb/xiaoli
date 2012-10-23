# coding: utf-8

from tango import db

class Metric(db.Model):
    __tablename__ = 'metrics'

class Threshold(db.Model):
    __tablename__ = 'thresholds'

