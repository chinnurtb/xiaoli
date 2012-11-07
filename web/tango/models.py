# coding: utf-8
from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()

import ast
from datetime import datetime

class Setting(db.Model):
    __tablename__ = 'settings'
    id          = db.Column(db.Integer, primary_key=True)
    mod         = db.Column(db.String())
    name        = db.Column(db.String(100))
    alias       = db.Column(db.String(100))
    value       = db.Column(db.Text())
    unit        = db.Column(db.String(20)) # 值的单位
    created_at  = db.Column(db.DateTime, default=datetime.now)
    updated_at  = db.Column(db.DateTime, default=datetime.now)

    def __init__(self, name, value):
        self.name = name
        self.value = value

    def __unicode__(self):
        return u'<参数设置 %s>' % self.alias

class DictCode(db.Model):
    __tablename__ = 'dict_codes'
    
    id         = db.Column(db.Integer, primary_key=True)
    cityid     = db.Column(db.Integer)
    type_id    = db.Column(db.Integer, db.ForeignKey('dict_types.id'))
    parent_id  = db.Column(db.Integer, db.ForeignKey('dict_codes.id'))
    code_name  = db.Column(db.String(100)) # 字典编码
    code_label = db.Column(db.String(100)) # 字典值
    is_valid   = db.Column(db.Boolean)
    remark     = db.Column(db.String(100))

    type = db.relationship('DictType')
    parent = db.relationship('DictCode')

    def __unicode__(self):
        return u'<字典 %s>' % self.code_label
    
class DictType(db.Model):
    __tablename__ = 'dict_types'

    id         = db.Column(db.Integer, primary_key=True)
    cityid     = db.Column(db.Integer)
    type_group = db.Column(db.String(100))
    type_name  = db.Column(db.String(100))
    type_label = db.Column(db.String(100))
    editable   = db.Column(db.Boolean)

    def __unicode__(self):
        return u'<字典类型 %s>' % self.type_name

class Profile(db.Model):
    __tablename__ = 'profiles'
    
    id = db.Column(db.Integer, primary_key=True)
    uid = db.Column(db.Integer, db.ForeignKey('users.id'))
    grp = db.Column(db.String(40))
    key = db.Column(db.String(100))
    value = db.Column(db.Text())
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now, onupdate=datetime.now)

    def __unicode__(self):
        return u'<用户配置>'

    def __init__(self, uid, grp, key, value):
        self.uid = uid
        self.grp = grp
        self.key = key
        self.value = value
        self.created_at = datetime.now()
        self.updated_at = datetime.now()

    @staticmethod
    def load(uid, grp):
        profile = {}
        profiles = Profile.query.filter_by(uid = uid, grp = grp).all()
        for p in profiles:
            profile[p.key] = p.value
        return profile

    @staticmethod
    def find(key, profiles):
        for p in profiles:
            if key == p.key:
                return p
        return None

    def update(self):
        profile = Profile.query.filter_by(uid=self.uid, grp=self.grp, key=self.key).first()
        if profile:
            profile.value = self.value
        else:
            db.session.add(self)

class QueryColumn(db.Model):
    __tablename__ = 'query_columns'

    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(60))
    orderable = db.Column(db.Boolean)
    groupable = db.Column(db.Boolean)


class Query(db.Model):

    __tablename__ = 'queries'

    id = db.Column(db.Integer, primary_key=True)
    uid = db.Column(db.Integer, db.ForeignKey('users.id'))
    tab = db.Column(db.String(100))
    mod = db.Column(db.String(100))
    name = db.Column(db.String(200))
    filters = db.Column(db.String)
    is_public = db.Column(db.Boolean)
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now) 


            
class QueryFilter(db.Model):
    __tablename__ = 'query_filters'

    id        = db.Column(db.Integer, primary_key=True)
    name      = db.Column(db.String(64))
    table     = db.Column(db.String(64))
    user_id   = db.Column(db.Integer, db.ForeignKey('users.id'))
    is_public = db.Column(db.Boolean, default=False)
    kv_list   = db.Column(db.String(2048))

    created_at = db.Column(db.DateTime, default=datetime.now)

    def get_kv_list(self):
        return ast.literal_eval(self.kv_list)


class Category(db.Model):

    """全局分类表"""

    __tablename__ = 'categories'

    id          = db.Column(db.Integer, primary_key=True)
    obj         = db.Column(db.String(100)) # 分组
    name        = db.Column(db.String(100))
    alias       = db.Column(db.String(100))
    is_valid    = db.Column(db.Boolean, default=True)

    def __unicode__(self):
        return u'<全局分类 %s>' % self.alias