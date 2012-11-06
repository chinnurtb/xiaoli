# coding: utf-8

from tango import db

class Module(db.Model):
    """采集模块表"""

    __tablename__ = 'modules'
    id          = db.Column(db.Integer, primary_key=True)
    name        = db.Column(db.String(20))
    alias       = db.Column(db.String(40))
    period      = db.Column(db.Integer) # 采集周期, 单位: minute
    retries     = db.Column(db.Integer) # 重试次数, 单位: 次
    timeout     = db.Column(db.Integer) # 超时, 单位: second
    remark      = db.Column(db.String(100))

    
class Monitor(db.Model):
    """监控器表"""
    
    __tablename__ = 'monitors'

    id          = db.Column(db.Integer, primary_key=True)
    category    = db.Column(db.String(20)) # 分类, 不关联
    vendor      = db.Column(db.String(20)) # 厂商,
    sysoid      = db.Column(db.String(100)) # 不关联
    match       = db.Column(db.String(100)) # 匹配规则
    modid       = db.Column(db.Integer, db.ForeignKey('modules.id'))
    mib         = db.Column(db.String(20))
    remark      = db.Column(db.String(100))
    
    module      = db.relation('Module')

    
class Miboid(db.Model):
    """Mib 文件"""
    __tablename__ = 'miboids'

    id       = db.Column(db.Integer, primary_key=True)
    mib      = db.Column(db.String(100))
    grp      = db.Column(db.String(40))
    name     = db.Column(db.String(100))
    alias    = db.Column(db.String(100))
    oid      = db.Column(db.String(100))
    is_valid = db.Column(db.Integer)
    remark   = db.Column(db.String(100))

