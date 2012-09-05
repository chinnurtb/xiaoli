#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango import db

from hashlib import md5

from tango.login import UserMixin

from datetime import datetime

class User(db.Model, UserMixin):

    """用户表"""

    __tablename__ = 'users'

    id         = db.Column(db.Integer, primary_key=True)
    username   = db.Column(db.String(40), unique=True)
    name       = db.Column(db.String(40))
    email      = db.Column(db.String(60), unique=True)
    password   = db.Column(db.String(60))
    signup_on  = db.Column(db.DateTime)
    role_id    = db.Column(db.Integer)#, db.ForeignKey('roles.id'))
    domain_id  = db.Column(db.Integer)#, db.ForeignKey('domains.id'))
    group_id   = db.Column(db.Integer)#, db.ForeignKey('user_groups.id'))
    department = db.Column(db.String(100))
    telephone  = db.Column(db.String(20))
    mobile     = db.Column(db.String(20))
    memo       = db.Column(db.String(1024))
    status     = db.Column(db.String(40))
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now)
    expired_at = db.Column(db.DateTime, default=datetime.now)

    # role   = db.relation('Role')
    # domain = db.relation('Domain')
    # group  = db.relation('UserGroup')


    def __init__(self):
        pass

    def gravatar_url(self, size=80):
        """Return the gravatar image for the given email address."""
        return 'http://www.gravatar.com/avatar/%s?d=identicon&s=%d' % \
            (md5(self.email.strip().lower().encode('utf-8')).hexdigest(), size)

    def __repr__(self):
        return '<User %r>' % self.username

    def check_passwd(self, passwd):
        if not self.password:
            return False
        return self.password == md5(passwd).hexdigest()

    @classmethod
    def authenticate(clazz, login, passwd):
        user = clazz.query.filter(db.or_(User.username == login, 
                                    User.email == login)).first()
        if user:
            authenticated = user.check_passwd(passwd)
        else:
            authenticated = False

        return user, authenticated


class UserGroup(db.Model):
    """用户组"""
    __tablename__ = 'user_groups'
    id         = db.Column(db.Integer, primary_key=True)
    name       = db.Column(db.String(100))
    role_id    = db.Column(db.Integer, db.ForeignKey('roles.id'))
    domain_id  = db.Column(db.Integer, db.ForeignKey('domains.id'))
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now)

    role   = db.relation('Role')
    domain = db.relation('Domain')
    
        
class Domain(db.Model):

    """管理域"""

    __tablename__ = 'domains'
    
    id            = db.Column(db.Integer, primary_key=True)
    name          = db.Column(db.String(50), nullable=False, unique=True)  
    city_list     = db.Column(db.String(100))
    town_list	  = db.Column(db.String(200))
    branch_list	  = db.Column(db.String(300))
    entrance_list = db.Column(db.String(300))
    created_at	  = db.Column(db.DateTime, default=datetime.now)
    updated_at	  = db.Column(db.DateTime, default=datetime.now)
    description   = db.Column(db.String(100))

roles_permissions = db.Table('roles_permissions', db.Model.metadata,
                             db.Column('role_id', db.Integer,
                                       db.ForeignKey('roles.id', ondelete='CASCADE')),
                             db.Column('permission_id', db.Integer,
                                       db.ForeignKey('permissions.id', ondelete='CASCADE')))

class Role(db.Model):

    """角色表"""

    __tablename__ = 'roles'

    id          = db.Column(db.Integer, primary_key=True)
    name        = db.Column(db.String(50), unique=True)
    description = db.Column(db.String(100))
    created_at  = db.Column(db.DateTime, default=datetime.now)
    updated_at  = db.Column(db.DateTime, default=datetime.now)

    permissions = db.relation('Permission', backref="roles", secondary=roles_permissions)

    def __repr__(self):
        return '<Role%r>' % self.name

class Permission(db.Model):
    __tablename__ = 'permissions'
    
    id                 = db.Column(db.Integer, primary_key=True)
    name               = db.Column(db.String(255), nullable=False)  
    text               = db.Column(db.String(255), nullable=False)  
    module_name        = db.Column(db.String(255))
    controller_name    = db.Column(db.String(255))
    action_name        = db.Column(db.String(255))
    method             = db.Column(db.String(255), nullable=False, default='GET')
    format             = db.Column(db.String(255))
    description        = db.Column(db.String(255))
    module_text        = db.Column(db.String(255))
    default_permission = db.Column(db.Integer(1), default=0)
    order_seq          = db.Column(db.Integer)
    is_valid           = db.Column(db.Integer(1), default=1)
    created_at         = db.Column(db.DateTime, default=datetime.now)
    updated_at         = db.Column(db.DateTime, default=datetime.now)


