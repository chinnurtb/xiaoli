#!/usr/bin/env python
# -*- coding: utf-8 -*-
from sqlalchemy.orm import object_session
from sqlalchemy.ext.hybrid import hybrid_property
from sqlalchemy import select, func, and_
from tango import db

#0: 省
AREA_PROVINCE=0
#1: 市 
AREA_CITY=1
#2: 县
AREA_TOWN=2
#3: 分局
AREA_BRANCH=3
#4: 接入点
AREA_ENTRANCE=4

class Area(db.Model):
    """
    Area Table
    """
    __tablename__ = 'areas'
    id             = db.Column(db.Integer, primary_key=True)
    parent_id      = db.Column(db.Integer, db.ForeignKey('areas.id'))
    rdn            = db.Column(db.String(100))
    name           = db.Column(db.String(50))
    alias          = db.Column(db.String(100))
    area_type      = db.Column(db.Integer)
    longitude      = db.Column(db.Float)
    latitude       = db.Column(db.Float)
    address        = db.Column(db.String(100))
    order_seq      = db.Column(db.Integer)
    managed_status = db.Column(db.Integer)
    entrance_type  = db.Column(db.Integer)
    sub_type       = db.Column(db.Integer)
    entrance_level = db.Column(db.Integer)
    check_state    = db.Column(db.Integer)
    entrance       = db.Column(db.Integer)
    entrance_name  = db.Column(db.String(50))
    branch         = db.Column(db.Integer)
    branch_name    = db.Column(db.String(50))
    town           = db.Column(db.Integer)
    town_name      = db.Column(db.String(50))
    cityid         = db.Column(db.Integer)
    city_name      = db.Column(db.String(50))
    remark         = db.Column(db.String(50)) 
    created_at     = db.Column(db.DateTime) 
    updated_at     = db.Column(db.DateTime)

    children = db.relation('Area')

    @hybrid_property
    def full_name(self):
        if self.entrance_name is not None:
            return self.city_name+self.town_name+self.branch_name+self.entrance_name
        elif self.branch_name is not None:
            return self.city_name+self.town_name+self.branch_name
        elif self.town_name is not None:
            return self.city_name+self.town_name
        elif self.city_name is not None:
            return self.city_name
        else:
            return self.name


class Manager(db.Model):
    """EMS"""
    __tablename__ = 'managers'
    id = db.Column(db.Integer, primary_key=True)
    cityid = db.Column(db.Integer)
    rdn = db.Column(db.String(100))
    name = db.Column(db.String(40))
    alias = db.Column(db.String(100))
    addr  = db.Column(db.String(100))
    status = db.Column(db.Integer)
    created_at = db.Column(db.DateTime)
    updated_at = db.Column(db.DateTime)

class Vendor(db.Model):
    """Device Vendor"""
    __tablename__ = 'vendors'
    id = db.Column(db.Integer, primary_key=True)
    cityid = db.Column(db.Integer)
    type_id = db.Column(db.Integer)
    name = db.Column(db.String(100))   
    alias = db.Column(db.String(100))
    url = db.Column(db.String(100))
    is_valid = db.Column(db.Integer)

    @property
    def node_count(self):
        return object_session(self).\
        scalar(
            select([func.count(Node.id)]).\
            where(Node.vendor_id==self.id)
        )
    @property
    def node_status0_count(self):
        return object_session(self).\
        scalar(
            select([func.count(Node.id)]).\
            where(and_(Node.vendor_id==self.id, Node.status == 0))
        )
    @property
    def node_status1_count(self):
        return object_session(self).\
        scalar(
            select([func.count(Node.id)]).\
            where(and_(Node.vendor_id==self.id, Node.status == 1))
        )

class TimePeriod(db.Model):
    """采集规则"""
    __tablename__ = 'timeperiods'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100))
    alias = db.Column(db.String(100))
    rule_format = db.Column(db.String(100))
    match_status = db.Column(db.Integer)
    other_status = db.Column(db.Integer)
    start_time = db.Column(db.DateTime)
    end_time = db.Column(db.DateTime)
    curr_status = db.Column(db.Integer)

class Model(db.Model):
    """设备型号"""
    __tablename__ = 'models'
    id = db.Column(db.Integer, primary_key=True)
    cityid = db.Column(db.Integer)
    type_id = db.Column(db.Integer)
    object = db.Column(db.String(100))
    name = db.Column(db.String(100))
    alias = db.Column(db.String(100))
    sysoid = db.Column(db.String(100))
    vendor_id = db.Column(db.Integer, db.ForeignKey("vendors.id"))
    fttx = db.Column(db.Integer)
    control_slot_num = db.Column(db.Integer)
    business_slot_num = db.Column(db.Integer)
    is_valid = db.Column(db.Integer)
    remark = db.Column(db.String(100))

NODE_STATUS_ONLINE=1

NODE_STATUS_OFFLINE=0

class Node(db.Model):
    __tablename__ = 'nodes'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(40))
    alias = db.Column(db.String(200))
    addr = db.Column(db.String(20))
    mask = db.Column(db.String(60))
    mac = db.Column(db.String(20))
    #-- 0:不可用 1:可用
    status = db.Column(db.Integer)
    #-- 0:未管理 1:已管理
    managed_state = db.Column(db.Integer)
    #-- 0:Production 1:Pre-Production 2:Test 3:Maintenance 4:Decommissioned
    prod_state = db.Column(db.Integer)
    remark = db.Column(db.String(200))
    #-- 1:olt 2:onu 3:dslam 4:eoc 5:switch
    category = db.Column(db.Integer)
    #-- 1:PON 2:WLAN 3:DATA 4:SERVER 5:CPE 6:ACCESS
    business = db.Column(db.Integer)
    area_id = db.Column(db.Integer, db.ForeignKey('areas.id'))
    timeperiod_id = db.Column(db.Integer, db.ForeignKey('timeperiods.id'))
    vendor_id = db.Column(db.Integer, db.ForeignKey("vendors.id"))
    model_id = db.Column(db.Integer, db.ForeignKey("models.id"))
    group_id = db.Column(db.Integer)
    summary = db.Column(db.String(255))
    location = db.Column(db.String(200))
    owner = db.Column(db.String(40))
    snmp_port = db.Column(db.Integer)
    snmp_ver = db.Column(db.String(20))
    snmp_comm = db.Column(db.String(40))
    snmp_wcomm = db.Column(db.String(40))
    sysoid = db.Column(db.String(100))
    sysname = db.Column(db.String(40))
    sysdescr = db.Column(db.String(200))
    sysuptime = db.Column(db.DateTime)
    oid_idx = db.Column(db.String(100))
    sysmodel = db.Column(db.String(100))
    os_version = db.Column(db.String(40))
    controller_id = db.Column(db.Integer)
    agent = db.Column(db.String(100))
    manager = db.Column(db.String(100))
    maintainer_id = db.Column(db.Integer, db.ForeignKey("maintains.id"))
    extra = db.Column(db.String(255))
    last_check = db.Column(db.DateTime)
    next_check = db.Column(db.DateTime)
    duration = db.Column(db.Integer)
    updated_at = db.Column(db.DateTime)

    area   = db.relation('Area')
    vendor = db.relation('Vendor')
    model  = db.relation('Model')

    __table_args__ = {'implicit_returning':False}

    def __repr__(self):
        return '<Node %r>' % self.name

class Board(db.Model):
    """板卡"""
    __tablename__ = 'boards'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(60), unique = True)
    alias = db.Column(db.String(40))
    created_at = db.Column(db.DateTime)

class Port(db.Model):
    """端口"""
    __tablename__ = 'ports'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(60), unique = True)
    alias = db.Column(db.String(40))
    created_at = db.Column(db.DateTime)

class Server(db.Model):
    """Servers of the system"""
    __tablename__ = 'servers'
    id = db.Column(db.Integer, primary_key=True)
    dn = db.Column(db.String(200))
    jid = db.Column(db.String(200))
    name = db.Column(db.String(100))
    os_type = db.Column(db.String(100))
    addrs = db.Column(db.String(200))  
    cpu_info = db.Column(db.String(200))
    mem_info = db.Column(db.String(200))
    swap_info = db.Column(db.String(200))
    disk_info = db.Column(db.String(200))
    worker_num = db.Column(db.Integer)
    presence = db.Column(db.Integer)
    descr = db.Column(db.String(200))
    created_at = db.Column(db.DateTime)
    updated_at = db.Column(db.DateTime)

class Maintains(db.Model):
    """维护人信息"""
    __tablename__ = 'maintains'
    id          = db.Column(db.Integer, primary_key=True)
    cityid      = db.Column(db.Integer)
    name        = db.Column(db.String(40)) 
    alias       = db.Column(db.String(100))
    department  = db.Column(db.String(40))   
    phone       = db.Column(db.String(40))
    mobile      = db.Column(db.String(40))
    email       = db.Column(db.String(50))
    post_addr   = db.Column(db.String(50))   
    post_code   = db.Column(db.String(50))
    admin       = db.Column(db.String(50))
    remark      = db.Column(db.String(100))  
    created_at  = db.Column(db.DateTime)
    updated_at  = db.Column(db.DateTime) 

