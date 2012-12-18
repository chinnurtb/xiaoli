#!/usr/bin/env python  
# -*- coding: utf-8 -*-


from hashlib import md5

from tango import db
from tango.login import UserMixin
from tango.base import AutoIncrSortedDict
from nodes.models import Area, AREA_PROVINCE, AREA_CITY, AREA_TOWN, AREA_BRANCH, AREA_ENTRANCE

from datetime import datetime


class User(db.Model, UserMixin):

    """用户表"""

    __tablename__ = 'users'

    id         = db.Column(db.Integer, primary_key=True)
    username   = db.Column(db.String(40), unique=True)
    name       = db.Column(db.String(40))
    email      = db.Column(db.String(60), unique=True)
    _password  = db.Column('password', db.String(60))
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

    role   = db.relation('Role', order_by='Role.name')
    domain = db.relation('Domain')
    # group  = db.relation('UserGroup')

    def __unicode__(self):
        return u'<用户 %s>' % self.username

    def __init__(self):
        pass

    @property
    def password(self):
        return self._password

    @password.setter
    def password(self, value):
        self._password = User.create_passwd(value)

    @property
    def is_province_user(self):
        if self.domain.province_list:
            return True
        else:
            return False
        
    def gravatar_url(self, size=80):
        """Return the gravatar image for the given email address."""
        return 'http://www.gravatar.com/avatar/%s?d=identicon&s=%d' % \
            (md5(self.email.strip().lower().encode('utf-8')).hexdigest(), size)

    def __repr__(self):
        return '<User %r>' % self.username

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

    def __unicode__(self):
        return u'<用户组 %s>' % self.name
        
class Domain(db.Model):

    """管理域"""

    __tablename__ = 'domains'
    
    id            = db.Column(db.Integer, primary_key=True)
    name          = db.Column(db.String(50), nullable=False, unique=True)  
    province_list = db.Column(db.String(100))
    city_list     = db.Column(db.String(100))
    town_list	  = db.Column(db.String(200))
    branch_list	  = db.Column(db.String(300))
    entrance_list = db.Column(db.String(300))
    created_at	  = db.Column(db.DateTime, default=datetime.now)
    updated_at	  = db.Column(db.DateTime, default=datetime.now)
    description   = db.Column(db.String(255))

    def __unicode__(self):
        return u'<管理域 %s>' % self.name

    def dump_areas(self, domain_areas):
        domain_areas = [int(area_id) for area_id in domain_areas.split(',') if area_id]
        areas = [Area.query.get(id) for id in domain_areas]
        province_list, city_list, town_list, branch_list, entrance_list = [], [], [], [], []
        
        area_types = (AREA_PROVINCE, AREA_CITY, AREA_TOWN, AREA_BRANCH, AREA_ENTRANCE)
        lists      = (province_list, city_list, town_list, branch_list, entrance_list)
        area_list = dict(zip(area_types, lists))
        
        for area in areas:
            if not area: continue # If the area has been deleted!
            if area.area_type in area_types:
                area_list[area.area_type].append(str(area.id))

        attr_names = ('province_list', 'city_list', 'town_list',
                      'branch_list', 'entrance_list')
        for attr_name, lst in zip(attr_names, lists):
            setattr(self, attr_name, ','.join(lst))

            
    def load_areas(self):
        domain_areas = []
        for lst in (self.province_list, self.city_list, self.town_list,
                    self.branch_list, self.entrance_list):
            if lst: domain_areas.extend(lst.split(','))
        return domain_areas

    def area_ids(self, area_type=0):
        ids = []
        if self.province_list:
            ids.extend([int(i) for i in self.province_list.split(',')])
        if self.city_list:
            areas = Area.query.filter(Area.cityid.in_(self.city_list.split(',')))
            if area_type: areas = areas.filter(Area.area_type==area_type)
            ids.extend([area.id for area in areas])
        if self.town_list:
            areas = Area.query.filter(Area.town.in_(self.town_list.split(',')))
            if area_type: areas = areas.filter(Area.area_type==area_type)
            ids.extend([area.id for area in areas])
        if self.branch_list:
            areas = Area.query.filter(Area.branch.in_(self.branch_list.split(',')))
            if area_type: areas = areas.filter(Area.area_type==area_type)
            ids.extend([area.id for area in areas])
        if self.entrance_list:
            areas = Area.query.filter(Area.entrance.in_(self.entrance_list.split(',')))
            if area_type: areas = areas.filter(Area.area_type==area_type)
            ids.extend([area.id for area in areas])
        return set(ids)

    @property
    def clause_permit(self):
        from sqlalchemy import or_
        clause = []
        if self.city_list: clause.append(Area.cityid.in_(self.city_list.split(',')))
        if self.town_list: clause.append(Area.town.in_(self.town_list.split(',')))
        if self.branch_list: clause.append(Area.branch.in_(self.branch_list.split(',')))
        if self.entrance_list: clause.append(Area.entrance.in_(self.entrance_list.split(',')))
        if len(clause) == 0:
            return None
        elif len(clause) == 1:
            return clause[0]
        else:
            return or_(*clause)

    def import_permit(self, area_type):
        area_list = []
        if self.city_list:
            areas = Area.query.filter(Area.cityid.in_(self.city_list.split(',')))
            if area_type: areas = areas.filter(Area.area_type==area_type)
            area_list.extend([(area.alias,area.id) for area in areas])
        if self.town_list:
            areas = Area.query.filter(Area.town.in_(self.town_list.split(',')))
            if area_type: areas = areas.filter(Area.area_type==area_type)
            area_list.extend([(area.alias,area.id) for area in areas])
        if self.branch_list:
            areas = Area.query.filter(Area.branch.in_(self.branch_list.split(',')))
            if area_type: areas = areas.filter(Area.area_type==area_type)
            area_list.extend([(area.alias,area.id) for area in areas])
        if self.entrance_list:
            areas = Area.query.filter(Area.entrance.in_(self.entrance_list.split(',')))
            if area_type: areas = areas.filter(Area.area_type==area_type)
            area_list.extend([(area.alias,area.id) for area in areas])
        return dict(area_list)

    @property
    def import_clause_permit(self):
        clause = []
        if self.city_list: clause.append('areas.cityid in ('+self.city_list+')')
        if self.town_list: clause.append('areas.town in ('+self.town_list+')')
        if self.branch_list: clause.append('areas.branch in ('+self.branch_list+')')
        if self.entrance_list: clause.append('areas.entrance in ('+self.entrance_list+')')
        if len(clause) == 0:
            return ''
        elif len(clause) == 1:
            return clause[0]
        else:
            return '('+' or '.join(clause)+')'

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

    def __unicode__(self):
        return u'<角色 %s>' % self.name

        
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

    def __repr__(self):
        return '<Permission %r>' % (self.endpoint + self.operation,)

    def __unicode__(self):
        return u'<权限 %s>' % self.name
    
    @staticmethod
    def make_tree(role_perms=None):
        # Prepare
        all_perms = Permission.query.all()
        perm_tree = AutoIncrSortedDict()
        role_ids = []
        role_names = []
        role_modules = []
        if role_perms:
            role_ids = [p.id for p in role_perms]
            role_names = [p.name for p in role_perms]
            role_modules = [p.module for p in role_perms]

        OPERATIONS = {
            u'查看' : 0,
            u'编辑' : 1,
            u'新建' : 2,
            u'删除' : 3,
            u'批量删除': 4,
        }
        def cmp_operation(x, y):
            if x not in OPERATIONS.keys() and y not in OPERATIONS.keys():
                return 0
            elif x not in OPERATIONS.keys():
                return 1
            elif y not in OPERATIONS.keys():
                return -1
            else:
                return OPERATIONS[x] - OPERATIONS[y]
                
        for p in all_perms:
            module_checked = ''
            name_checked = ''
            operation_checked = ''

            if role_perms:
                if p.id in role_ids:
                    operation_checked = 'checked'
                if p.name in role_names:
                    name_checked = 'checked'
                if p.module in role_modules:
                    module_checked = 'checked'
            module_key = (p.module_text, module_checked)
            name_key = (p.name, name_checked)
            operation_key = p.operation
            
            perm_tree[module_key][name_key][operation_key] = (p.id, operation_checked)

        perm_tree['cmpfunc'] = cmp_operation
        return perm_tree
