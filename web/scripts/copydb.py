#!/usr/bin/env python
# -*- coding: utf-8 -*-

# import areas data from OLD mysql database to NEW postgresql database

from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from webapp import db

from nodes.models import Area

t_app = Flask(__name__)
t_app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://root:public@192.168.100.71/tl1_snmp'
old_db = SQLAlchemy()
old_db.init_app(t_app)
old_db.app = t_app

class OldArea(old_db.Model):
    __tablename__ = 'areas'
    
    id            = old_db.Column(old_db.Integer, primary_key=True)
    cityid        = old_db.Column(old_db.Integer)
    parent_id     = old_db.Column(old_db.Integer)
    area_name     = old_db.Column(old_db.String(255))
    area_level    = old_db.Column(old_db.Integer)
    area_dn       = old_db.Column(old_db.String(255))
    entrance_type = old_db.Column(old_db.Integer)
    longitude     = old_db.Column(old_db.Float)
    latitude      = old_db.Column(old_db.Float)

count = 0    
def save(old, parent_id, level):
    area = Area(
        parent_id     = parent_id,
        name          = old.area_name,
        rdn           = 'In fact.....None.',
        area_type     = level,
        entrance_type = old.entrance_type,
        longitude     = old.longitude,
        latitude      = old.latitude
    )
    db.session.add(area)
    global count
    count += 1
    return area.id
    

def dir_save(cur_id, par_id, level):
    old_area = OldArea.query.get(cur_id)
    save(old_area, par_id, level)
    children = OldArea.query.filter(OldArea.parent_id==cur_id)
    for child in children:
        dir_save(child.id, cur_id, level+1)
    global count
    if count % 50 == 0:
        db.session.commit()
        print '50 items saved'

dir_save(1, 1, 0)
db.session.commit()
