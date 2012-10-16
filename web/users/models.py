#!/usr/bin/env python  
# -*- coding: utf-8 -*-


from hashlib import md5

from tango import db
from tango.login import UserMixin
from tango.base import AutoIncrSortedDict
from nodes.models import Area, AREA_CITY, AREA_TOWN, AREA_BRANCH, AREA_ENTRANCE

from datetime import datetime

class User(db.Model, UserMixin):

    """用户表"""

    __tablename__ = 'users'

    id         = db.Column(db.Integer, primary_key=True)
    username   = db.Column(db.String(40), unique=True)
    name       = db.Column(db.String(40))
    email      = db.Column(db.String(60), unique=True)
    _password   = db.Column('password', db.String(60))
    signup_on  = db.Column(db.DateTime)
    role_id    = db.Column(db.Integer, db.ForeignKey('roles.id'))
    domain_id  = db.Column(db.Integer, db.ForeignKey('domains.id'))
    group_id   = db.Column(db.Integer, db.ForeignKey('user_groups.id'))
    department = db.Column(db.String(100))
    telephone  = db.Column(db.String(20))
    mobile     = db.Column(db.String(20))
    memo       = db.Column(db.String(1024))
    status     = db.Column(db.String(40))
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now)
    expired_at = db.Column(db.DateTime, default=datetime.now)

    role   = db.relation('Role')
    domain = db.relation('Domain')
    # group  = db.relation('UserGroup')


    def __init__(self):
        pass

    @property
    def password(self):
        return self._password

    @password.setter
    def password(self, value):
        self._password = User.create_passwd(value)
        
    def gravatar_url(self, size=80):
        """Return the gravatar image for the given email address."""
        return 'http://www.gravatar.com/avatar/%s?d=identicon&s=%d' % \
            (md5(self.email.strip().lower().encode('utf-8')).hexdigest(), size)

    def __repr__(self):
        return '<User %r>' % self.username

    def __unicode__(self):
        return self.username

    @staticmethod
    def create_passwd(raw):
        return md5(raw).hexdigest()        
        
    def check_passwd(self, passwd):
        if not self.password:
            return False
        # return self.password == passwd
        return self.password == User.create_passwd(passwd)

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
    description   = db.Column(db.String(255))

    def dump_areas(self, domain_areas):
        domain_areas = [int(area_id) for area_id in domain_areas.split(',') if area_id]
        areas = [Area.query.get(id) for id in domain_areas]
        city_list = []
        town_list = []
        branch_list = []
        entrance_list = []
        area_list = { AREA_CITY: city_list,
                      AREA_TOWN: town_list,
                      AREA_BRANCH: branch_list,
                      AREA_ENTRANCE: entrance_list}
        for area in areas:
            if area.area_type in (AREA_CITY, AREA_TOWN, AREA_BRANCH, AREA_ENTRANCE):
                area_list[area.area_type].append(str(area.id))
        if city_list: self.city_list = (',').join(city_list)
        if town_list: self.town_list = (',').join(town_list)
        if branch_list: self.branch_list = (',').join(branch_list)
        if entrance_list: self.entrance_list = (',').join(entrance_list)

    def load_areas(self):
        domain_areas = []
        if self.city_list: domain_areas.extend(self.city_list.split(','))
        if self.town_list: domain_areas.extend(self.town_list.split(','))
        if self.branch_list: domain_areas.extend(self.branch_list.split(','))
        if self.entrance_list: domain_areas.extend(self.entrance_list.split(','))
        return domain_areas

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
    module             = db.Column(db.String(100)) # 模块的名字, 例如(users, nodes)
    module_text        = db.Column(db.String(255)) # 模块的中文名字, 如(拓扑, 资源, 用户, 系统)
    endpoint           = db.Column(db.String(100), nullable=False) # 一个flask的 endpoint 用来作为url_for的参数
    
    operation          = db.Column(db.String(255))
    default_permission = db.Column(db.Integer(1), default=0)
    created_at         = db.Column(db.DateTime, default=datetime.now)
    updated_at         = db.Column(db.DateTime, default=datetime.now)
    
    # text               = db.Column(db.String(255), nullable=False)  
    # controller_name    = db.Column(db.String(255))
    # action_name        = db.Column(db.String(255))
    # method             = db.Column(db.String(255), nullable=False, default='GET')
    # format             = db.Column(db.String(255))
    # module_text        = db.Column(db.String(255))
    # order_seq          = db.Column(db.Integer)
    # is_valid           = db.Column(db.Integer(1), default=1)

    def __repr__(self):
        return '<Permission %r>' % (self.endpoint + self.operation,)
    
    @staticmethod
    def make_tree(role_perms=None):
        all_perms = Permission.query.all()
        perm_tree = AutoIncrSortedDict()
        for p in all_perms:
            module_checked = ''
            name_checked = ''
            operation_checked = ''
            if role_perms is not None:
                for rp in role_perms:
                    if p.module == rp.module:
                        module_checked = 'checked'
                    if p.name == rp.name:
                        name_checked = 'checked'
                    if p.id == rp.id:
                        operation_checked = 'checked'
            module_key = (p.module_text, module_checked)
            name_key = (p.name, name_checked)
            operation_key = (p.operation, operation_checked)
            
            perm_tree[module_key][name_key][operation_key] = p.id
            
        return perm_tree
