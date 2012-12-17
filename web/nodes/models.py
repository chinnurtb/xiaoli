# coding: utf-8

from sqlalchemy.orm import object_session,backref
from sqlalchemy.ext.hybrid import hybrid_property
from sqlalchemy.ext.declarative import declared_attr
from sqlalchemy import select, func, and_
from tango import db
from datetime import datetime, timedelta
import time
import errdb

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

AREA_TYPE_DICT = {1:"city_name",2:"town_name",3:"branch_name",4:"entrance_name"}

class Area(db.Model):
    """
    Area Table
    """
    __tablename__ = 'areas'
    id             = db.Column(db.Integer, primary_key=True)
    parent_id      = db.Column(db.Integer, db.ForeignKey('areas.id'))
    dn            = db.Column(db.String(100))
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
    updated_at     = db.Column(db.DateTime, default=datetime.now)

    children = db.relation('Area',backref=backref("parent", remote_side=id),)

    @hybrid_property
    def full_name(self):
        name_list = [self.city_name, self.town_name, self.branch_name, self.entrance_name]
        fname = ' / '.join([name for name in name_list if name])
        return self.name if fname == '' else fname

    def __unicode__(self):
        return u'<区域 %r>' % self.alias


class Manager(db.Model):
    """EMS"""
    __tablename__ = 'managers'
    id         = db.Column(db.Integer, primary_key=True)
    cityid     = db.Column(db.Integer)
    dn        = db.Column(db.String(100))
    name       = db.Column(db.String(40))
    alias      = db.Column(db.String(100))
    addr       = db.Column(db.String(100))
    status     = db.Column(db.Integer)
    created_at = db.Column(db.DateTime)
    updated_at = db.Column(db.DateTime)

class Vendor(db.Model):
    """Device Vendor"""
    __tablename__ = 'vendors'
    id       = db.Column(db.Integer, primary_key=True)
    cityid   = db.Column(db.Integer)
    type_id  = db.Column(db.Integer) # 貌似没用?
    name     = db.Column(db.String(100))   
    alias    = db.Column(db.String(100))
    url      = db.Column(db.String(100)) # 厂商主页
    is_valid = db.Column(db.Integer)

    models = db.relationship("Model", backref="vendor")

    @property
    def node_count(self):
        return object_session(self).\
        scalar(
            select([func.count(Node.id)]).\
            where(Node.vendor_id==self.id)
        )
    @property
    def node_status1_count(self):
        return object_session(self).\
        scalar(
            select([func.count(Node.id)]).\
            where(and_(Node.vendor_id==self.id, Node.status == 1))
        )
    @property
    def node_status2_count(self):
        return object_session(self).\
        scalar(
            select([func.count(Node.id)]).\
            where(and_(Node.vendor_id==self.id, Node.status == 2))
        )
    @property
    def node_status3_count(self):
        return object_session(self).\
        scalar(
            select([func.count(Node.id)]).\
            where(and_(Node.vendor_id==self.id, Node.status == 3))
        )
    @property
    def node_status4_count(self):
        return object_session(self).\
        scalar(
            select([func.count(Node.id)]).\
            where(and_(Node.vendor_id==self.id, Node.status == 4))
        )

    def __unicode__(self):
        return u'<厂商 %s>' % self.alias

class Model(db.Model):
    """设备型号"""
    __tablename__     = 'models'
    id                = db.Column(db.Integer, primary_key=True)
    cityid            = db.Column(db.Integer)
    category_id       = db.Column(db.Integer, db.ForeignKey("categories.id"))
    object            = db.Column(db.String(100)) # 不要
    name              = db.Column(db.String(100))
    alias             = db.Column(db.String(100))
    sysoid            = db.Column(db.String(100))
    vendor_id         = db.Column(db.Integer, db.ForeignKey("vendors.id"))
    fttx              = db.Column(db.Integer) # 不要
    control_slot_num  = db.Column(db.Integer) # 不要
    business_slot_num = db.Column(db.Integer) # 不要
    is_valid          = db.Column(db.Integer)
    remark            = db.Column(db.String(100))

    category          = db.relation('Category')

    def __unicode__(self):
        return u'<型号 %s>' % self.alias

NODE_STATUS_DICT = {0: u'未知',1: u'正常', 2: u'宕机', 3: u'不可达'}
NODE_STATUS_COLOR = {0: '#808080',1: 'lime', 2: 'red', 3: '#F6983E'}
SNMP_VER_DICT = {"v1":'v1',"v2c":'v2c'}

ping_status_dict = {0: u'未知', 1: u'正常', 2: u'故障'}
snmp_status_dict = {0: u'未知', 1: u'正常', 2: u'故障'}

class NodeMixin(object):
    id            = db.Column(db.Integer, primary_key=True)
    dn            = db.Column(db.String(100))
    name          = db.Column(db.String(40))
    alias         = db.Column(db.String(200))
    addr          = db.Column(db.String(20))
    mask          = db.Column(db.String(60))
    mac           = db.Column(db.String(20))
    #-- 0:不可用 1:可用
    status        = db.Column(db.Integer)
    #-- 0:未管理 1:已管理
    managed_state = db.Column(db.Integer)
    #-- 0:Production 1:Pre-Production 2:Test 3:Maintenance 4:Decommissioned
    prod_state    = db.Column(db.Integer)
    remark        = db.Column(db.String(200))
    #-- 1:PON 2:WLAN 3:DATA 4:SERVER 5:CPE 6:ACCESS
    business      = db.Column(db.Integer)
    group_id      = db.Column(db.Integer)
    summary       = db.Column(db.String(255))
    location      = db.Column(db.String(200))
    owner         = db.Column(db.String(40))
    snmp_port     = db.Column(db.Integer)
    snmp_ver      = db.Column(db.String(20))
    snmp_comm     = db.Column(db.String(40))
    snmp_wcomm    = db.Column(db.String(40))
    sysoid        = db.Column(db.String(100))
    sysname       = db.Column(db.String(40))
    sysdescr      = db.Column(db.String(200))
    sysuptime     = db.Column(db.DateTime)
    #oid_idx       = db.Column(db.String(100))  # 节点的oid索引
    sysmodel      = db.Column(db.String(100))
    os_version    = db.Column(db.String(40))
    agent         = db.Column(db.String(100))
    manager       = db.Column(db.String(100))
    extra         = db.Column(db.String(255))
    last_check    = db.Column(db.DateTime)
    next_check    = db.Column(db.DateTime)
    duration      = db.Column(db.Integer)
    created_at    = db.Column(db.DateTime, default=datetime.now)
    updated_at    = db.Column(db.DateTime, default=datetime.now)
    snmp_status   = db.Column(db.Integer)       # snmp状态
    snmp_summary  = db.Column(db.String(400))   # snmp状态详细
    ping_status   = db.Column(db.Integer)       # ping状态
    ping_summary  = db.Column(db.String(400))   # ping状态详细

    #-- 20:olt 21:onu 30:dslam 50:eoc 2:switch 90:host
    @declared_attr
    def category_id(cls):
        return db.Column(db.Integer, db.ForeignKey('categories.id'))

    @declared_attr
    def category(cls):
        return db.relationship("Category")

    @declared_attr
    def maintainer_id(cls):
        return db.Column(db.Integer, db.ForeignKey('maintains.id'))

    @declared_attr
    def maintain(cls):
        return db.relationship("Maintain")

    @declared_attr
    def area_id(cls):
        return db.Column(db.Integer, db.ForeignKey('areas.id'))

    @declared_attr
    def area(cls):
        return db.relationship("Area")

    @declared_attr
    def timeperiod_id(cls):
        return db.Column(db.Integer, db.ForeignKey('timeperiods.id'))

    @declared_attr
    def timeperiod(cls):
        return db.relationship("TimePeriod")

    @declared_attr
    def vendor_id(cls):
        return db.Column(db.Integer, db.ForeignKey('vendors.id'))

    @declared_attr
    def vendor(cls):
        return db.relationship("Vendor")

    @declared_attr
    def model_id(cls):
        return db.Column(db.Integer, db.ForeignKey('models.id'))

    @declared_attr
    def model(cls):
        return db.relationship("Model")

    @hybrid_property
    def cityid(self):
        return self.area.cityid

    @hybrid_property
    def town(self):
        return self.area.town

    @hybrid_property
    def branch(self):
        return self.area.branch

    @hybrid_property
    def entrance(self):
        return self.area.entrance

    @hybrid_property
    def status_name(self):
        return NODE_STATUS_DICT.get(self.status,"")

    @hybrid_property
    def ping_status_name(self):
        return ping_status_dict.get(self.ping_status,"")

    @hybrid_property
    def snmp_status_name(self):
        return snmp_status_dict.get(self.snmp_status,"")

    @property
    def boards(self):
        return Board.query.filter(Board.node_id==self.id)


class Node(NodeMixin,db.Model):
    """ Node """
    __tablename__ = 'nodes'
    __table_args__ = {'implicit_returning':False}

    def __unicode__(self):
        return u'<节点 %s>' % self.alias

    @staticmethod
    def export_columns():
        return ['status','name','alias','addr','category.alias','vendor.alias','model.alias','area.full_name','last_check','summary']

class NodeSwitch(NodeMixin, db.Model):
    """ Switchs """
    __tablename__ = 'node_switchs'
    ports   = db.relationship("PortSwitch", backref="switch")

    def __unicode__(self):
        return u'<交换机 %s>' % self.alias

    @staticmethod
    def export_columns():
        return ['status','name','alias','addr','area.full_name','vendor.alias','model.alias','mask','snmp_comm','snmp_wcomm','last_check','location','remark']

class NodeRouter(NodeMixin, db.Model):
    """ Routers """
    __tablename__ = 'node_routers'

    def __unicode__(self):
        return u'<路由器 %s>' % self.alias

    @staticmethod
    def export_columns():
        return ['status','name','alias','addr','area.full_name','vendor.alias','model.alias','mask','snmp_comm','snmp_wcomm','last_check','location','remark']

class NodeHost(NodeMixin, db.Model):
    """ Hosts """
    __tablename__ = 'node_hosts'
    os_type    = db.Column(db.String(100))
    ifaces     = db.Column(db.String(200)) # 
    cpu_info   = db.Column(db.String(200))
    mem_info   = db.Column(db.String(200))
    swap_info  = db.Column(db.String(200))
    disk_info  = db.Column(db.String(200))
    worker_num = db.Column(db.Integer) # 采集进程数

    def __unicode__(self):
        return u'<服务器 %s>' % self.alias

class NodeOlt(NodeMixin,db.Model):
    """ OLT """
    __tablename__ = 'node_olts'

    onus    = db.relationship("NodeOnu", backref="olt")
    ports   = db.relationship("PortOlt", backref="olt")

    def __unicode__(self):
        return u'<OLT %s>' % self.alias

    @property
    def onu_count_plan(self):
        return object_session(self).\
        scalar(
            select([func.count(NodeOnu.id)]).\
            where(and_(NodeOnu.ctrl_id==self.id, NodeOnu.area_id != None))
        )
    @property
    def onu_count_unplan(self):
        return object_session(self).\
        scalar(
            select([func.count(NodeOnu.id)]).\
            where(and_(NodeOnu.ctrl_id==self.id, NodeOnu.area_id == None))
        )

    @staticmethod
    def export_columns():
        return ['status','category.alias','name','alias','addr','area.full_name','vendor.alias','model.alias','mask','snmp_comm','snmp_wcomm','snmp_ver','last_check','location','remark']

    def get_traffic(self):
        data_ifInOctets,data_ifOutOctets = [] ,[]
        try:
            client = errdb.Client(host=db.app.config.get("ERRDB_HOST"), port=db.app.config.get("ERRDB_PORT"))
            start_time = time.mktime((datetime.now()-timedelta(days=2)).timetuple())
            data = client.fetch(self.dn+":ping", ['rtmax','rtmax'], start_time, time.time())
            data_ifInOctets = [{'x':data_dict.keys()[0], 'y': data_dict.values()[0][0]} for data_dict in data ]
            data_ifOutOctets = [{'x':data_dict.keys()[0], 'y': data_dict.values()[0][1]} for data_dict in data ]
        except :
            pass
        return data_ifInOctets, data_ifOutOctets

    def get_ping_delay(self):
        data_ping = []
        try:
            client = errdb.Client(host=db.app.config.get("ERRDB_HOST"), port=db.app.config.get("ERRDB_PORT"))
            start_time = time.mktime((datetime.now()-timedelta(days=2)).timetuple())
            data = client.fetch(self.dn+":ping", ['rtmax'], start_time, time.time())
            data_ping = [{'x':data_dict.keys()[0], 'y': data_dict.values()[0][0]} for data_dict in data ]
        except :
            pass
        return data_ping

    def get_errdb_data(self, key, dn=None):
        if not dn: dn = self.dn + ":ping"
        try:
            client = errdb.Client(host=db.app.config.get("ERRDB_HOST"), port=db.app.config.get("ERRDB_PORT"))
            start_time = time.mktime((datetime.now()-timedelta(days=2)).timetuple())
            data = client.fetch(dn, key, start_time, time.time())
            result = data[-1].values()
            if len(result) == 0: return None
            if len(result) == 1:return result[0]
            return result
        except :
            pass

class NodeOnu(NodeMixin,db.Model):
    """ ONU """
    __tablename__ = 'node_onus'

    ctrl_id = db.Column(db.Integer, db.ForeignKey('node_olts.id'))
    eocs    = db.relationship("NodeEoc", backref="onu")
    ports   = db.relationship("PortOnu", backref="onu")

    def __unicode__(self):
        return u'<ONU %s>' % self.alias

    @staticmethod
    def export_columns():
        return ['status','name','alias','addr','area.full_name','olt.name','olt.addr','vendor.alias','model.alias','mask','snmp_comm','snmp_wcomm','last_check','location','remark']

class NodeEoc(NodeMixin, db.Model):
    """ Eocs """
    __tablename__ = 'node_eocs'

    esn             = db.Column(db.String(50))  # ESN
    contact_tel     = db.Column(db.String(50))  # 联系电话
    install_time    = db.Column(db.DateTime)    # 安装时间

    ctrl_id = db.Column(db.Integer, db.ForeignKey('node_onus.id'))
    cpes = db.relationship("NodeCpe", backref="eoc")
    ports   = db.relationship("PortEoc", backref="eoc")

    @property
    def cpe_count_plan(self):
        return object_session(self).\
        scalar(
            select([func.count(NodeCpe.id)]).\
            where(and_(NodeCpe.ctrl_id==self.id, NodeCpe.area_id != None))
        )
    @property
    def cpe_count_unplan(self):
        return object_session(self).\
        scalar(
            select([func.count(NodeCpe.id)]).\
            where(and_(NodeCpe.ctrl_id==self.id, NodeCpe.area_id == None))
        )

    def __unicode__(self):
        return u'<EOC %s>' % self.alias

    @staticmethod
    def export_columns():
        return ['status','category.alias','name','alias','addr','area.full_name','vendor.alias','model.alias','mask','snmp_comm','snmp_wcomm','snmp_ver','last_check','location','remark']

class NodeCpe(NodeMixin, db.Model):
    """ Cpes """
    __tablename__ = 'node_cpes'

    esn             = db.Column(db.String(50))  # ESN
    contact_tel     = db.Column(db.String(50))  # 联系电话
    install_time    = db.Column(db.DateTime)    # 安装时间
    card_id         = db.Column(db.String(50))  # 身份证号

    ctrl_id = db.Column(db.Integer, db.ForeignKey('node_eocs.id'))

    def __unicode__(self):
        return u'<CPE %s>' % self.alias

    @staticmethod
    def export_columns():
        return ['status','name','alias','addr','area.full_name','eoc.name','eoc.addr','vendor.alias','model.alias','mask','snmp_comm','snmp_wcomm','last_check','location','remark']


class Server(db.Model):
    """Servers of the system"""
    __tablename__ = 'servers'
    id         = db.Column(db.Integer, primary_key=True)
    dn         = db.Column(db.String(200))
    jid        = db.Column(db.String(200))
    name       = db.Column(db.String(100))
    os_type    = db.Column(db.String(100))
    addrs      = db.Column(db.String(200))  
    cpu_info   = db.Column(db.String(200))
    mem_info   = db.Column(db.String(200))
    swap_info  = db.Column(db.String(200))
    disk_info  = db.Column(db.String(200))
    worker_num = db.Column(db.Integer)
    presence   = db.Column(db.Integer)
    descr      = db.Column(db.String(200))
    created_at = db.Column(db.DateTime)
    updated_at = db.Column(db.DateTime)

    def __unicode__(self):
        return u'<服务器 %s>'% self.name
    
class Maintain(db.Model):
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

    def __unicode__(self):
        return u'<维护人 %s>'% self.alias

class SysOid(db.Model):

    """设备系统OID"""

    __tablename__ = 'sysoids'
    
    id          = db.Column(db.Integer, primary_key=True)
    sysoid      = db.Column(db.String(100))
    model_id    = db.Column(db.Integer, db.ForeignKey('models.id'))
    disco       = db.Column(db.String(20)) # 发现模块
    mib         = db.Column(db.String(20)) # mib文件, 从 miboids.mib 中选
    remark      = db.Column(db.String(100))
    
    model       = db.relation('Model')

    def __unicode__(self):
        return u'<SysOid %s>'% self.sysoid

class Board(db.Model):
    """板卡"""
    __tablename__ = 'boards'
    node_id     = db.Column(db.Integer)
    id          = db.Column(db.Integer, primary_key=True)
    name        = db.Column(db.String(60), unique = True)
    alias       = db.Column(db.String(40))
    vendor_id   = db.Column(db.Integer) # 厂商
    shelf_no    = db.Column(db.Integer) # 机架号
    slot_no     = db.Column(db.Integer) # 槽位号
    board_no    = db.Column(db.Integer) # 板卡号
    board_type  = db.Column(db.Integer) # 板卡类型
    board_status= db.Column(db.Integer) # 板卡状态
    software_vsn= db.Column(db.String(200)) #软件版本
    hardversion= db.Column(db.String(200)) #硬件版本
    fpga_vsn    = db.Column(db.String(100)) #FPGA版本
    cpld_vsn    = db.Column(db.String(100)) #CPLD版本
    max_ports   = db.Column(db.Integer) # 端口数量
    adminstatus= db.Column(db.Integer) # 管理状态
    operstatus = db.Column(db.Integer) # 运行状态
    standbystatus  = db.Column(db.Integer) #主备状态
    lockstatus = db.Column(db.Integer) # 锁状态
    cpuload    = db.Column(db.Integer) # CPU负载
    memusage   = db.Column(db.Integer) # 内存占用
    temperature = db.Column(db.Integer) # 温度
    serial_no   = db.Column(db.String(100)) # 序列号
    uptime      = db.Column(db.Integer) # 运行时间
    remark      = db.Column(db.String(100))
    last_update = db.Column(db.DateTime)
    created_at  = db.Column(db.DateTime)
    updated_at  = db.Column(db.DateTime)

    category_id = db.Column(db.Integer, db.ForeignKey('categories.id'))
    category    = db.relationship("Category")

    @property
    def port_count(self):
        return object_session(self).\
        scalar(
            select([func.count(Port.id)]).\
            where(Port.board_id==self.id)
        )

class PortMixin(object):
    id          = db.Column(db.Integer, primary_key=True)
    name        = db.Column(db.String(60), unique = True)
    alias       = db.Column(db.String(40))
    board_id    = db.Column(db.Integer)
    # 业务端口类型，   1.FE  2.GE  3.PON  4.POTS  5.DSL  6.E1  7.SDH  8.RF  9.VI  10.AGGREGATION
    biztype    = db.Column(db.Integer)
    ifindex     = db.Column(db.Integer) # 端口索引
    ifdescr     = db.Column(db.String(100)) # 端口名称
    iftype      = db.Column(db.Integer) # 端口物理类型
    ifphysaddr  = db.Column(db.String(50))  # 端口物理地址
    ifadminstatus   = db.Column(db.Integer) # 管理状态
    ifoperstatus    = db.Column(db.Integer) # 运行状态
    ifspeed     = db.Column(db.Integer) # 端口速率
    ifmtu       = db.Column(db.Integer) # 端口MTU
    iflastchange    = db.Column(db.DateTime)    # 端口最后变化时间
    uplink_port     = db.Column(db.Integer) # 上联端口
    slot_no     = db.Column(db.Integer) # 槽位号
    port_no     = db.Column(db.Integer) # 端口号
    downassuredbw   = db.Column(db.Integer) # 下行限速
    downmaximumbw   = db.Column(db.Integer) # 下行带宽
    upassuredbw = db.Column(db.Integer) # 上行限速
    upmaximumbw = db.Column(db.Integer) # 上行带宽
    temperature = db.Column(db.Integer) # 温度
    received_power  = db.Column(db.Integer) # 接受功率
    led_power   = db.Column(db.Integer) # 发光功率
    e_current   = db.Column(db.Integer) # 电流
    voltage     = db.Column(db.Integer) # 电压
    telno       = db.Column(db.String(50))
    duplex      = db.Column(db.Integer) # 双工状态
    tid         = db.Column(db.String(50))  # 协议终端标识
    physaddress = db.Column(db.String(50))
    mgid        = db.Column(db.String(20))
    created_at  = db.Column(db.DateTime)
    updated_at  = db.Column(db.DateTime)

    @declared_attr
    def category_id(cls):
        return db.Column(db.Integer, db.ForeignKey('categories.id'))

    @declared_attr
    def category(cls):
        return db.relationship("Category")

    @declared_attr
    def vendor_id(cls):
        return db.Column(db.Integer, db.ForeignKey('vendors.id'))

    @declared_attr
    def vendor(cls):
        return db.relationship("Vendor")

class Port(PortMixin,db.Model):
    """ Port """
    __tablename__ = 'ports'
    __table_args__ = {'implicit_returning':False}
    node_id      = db.Column(db.Integer, db.ForeignKey('nodes.id'))

class PortOlt(PortMixin,db.Model):
    """ OLT Port """
    __tablename__ = 'port_olts'
    node_id      = db.Column(db.Integer, db.ForeignKey('node_olts.id'))

class PortOnu(PortMixin,db.Model):
    """ ONU Port """
    __tablename__ = 'port_onus'
    node_id      = db.Column(db.Integer, db.ForeignKey('node_onus.id'))

class PortEoc(PortMixin,db.Model):
    """ EOC Port """
    __tablename__ = 'port_eocs'
    node_id      = db.Column(db.Integer, db.ForeignKey('node_eocs.id'))

class PortSwitch(PortMixin,db.Model):
    """ 交换机 Port """
    __tablename__ = 'port_switchs'
    node_id      = db.Column(db.Integer, db.ForeignKey('node_switchs.id'))
