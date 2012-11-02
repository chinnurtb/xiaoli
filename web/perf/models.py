# coding=utf-8

from tango import db

from sqlalchemy.ext.declarative import declared_attr 

__all__ = ['PingPerf',
           'CpuMemPerf',
           'BoardPerf',
           'IntfTrafficPerf',
           'IntfUsagePerf',
           'PonPowerPerf',
           'PonTrafficPerf',
           'PonUsagePerf']

class PerfMixin(object):

    id = db.Column(db.Integer, primary_key=True)

    @declared_attr 
    def nodeid(cls):
        return db.Column(db.Integer, db.ForeignKey("nodes.id"))

    @declared_attr
    def node(cls):
        return db.relation('Node')

    sampleyear      = db.Column(db.Integer)
    samplemonth     = db.Column(db.Integer)
    sampleday       = db.Column(db.Integer)
    sampleweekday   = db.Column(db.Integer)
    samplehour      = db.Column(db.Integer)
    sampletime      = db.Column(db.DateTime)

class PingPerf(db.Model, PerfMixin):

    '''节点PING指标'''
    
    __tablename__ = 'perf_ping'

    rtavg   = db.Column(db.Float)
    rtmax   = db.Column(db.Float)
    rtmin   = db.Column(db.Float)
    lossavg = db.Column(db.Float)
    lossmax = db.Column(db.Float)
    stdevavg = db.Column(db.Float)
    stdevmax = db.Column(db.Float)

    
class CpuMemPerf(db.Model, PerfMixin):
    '''节点CPU内存指标'''
    __tablename__ = 'perf_cpumem'
    
    cpuavg      = db.Column(db.Float)
    cpumax      = db.Column(db.Float) 
    memavg      = db.Column(db.Float) 
    memmax      = db.Column(db.Float) 
    tempavg     = db.Column(db.Float) 
    tempmax     = db.Column(db.Float) 
    powerstate  = db.Column(db.Integer)
    fanstate    = db.Column(db.Integer)

    
class BoardPerf(db.Model, PerfMixin):
    '''版卡指标'''
    __tablename__ = 'perf_board'

    boardidx    = db.Column(db.Integer)
    boardtype   = db.Column(db.Integer)
    
    cpuavg      = db.Column(db.Float)
    cpumax      = db.Column(db.Float) 
    memavg      = db.Column(db.Float) 
    memmax      = db.Column(db.Float) 
    tempavg     = db.Column(db.Float) 
    tempmax     = db.Column(db.Float) 

    
class IntfUsagePerf(db.Model, PerfMixin):
    '''接口占用指标'''
    __tablename__ = 'perf_intfusage'

    intftype    = db.Column(db.Integer)
    
    intftotal   = db.Column(db.Integer)
    intfused    = db.Column(db.Integer)
    intfusage   = db.Column(db.Float)
    

class IntfTrafficPerf(db.Model, PerfMixin):
    '''接口流量流速指标'''
    __tablename__ = 'perf_intftraffic'

    intfidx     = db.Column(db.Integer)
    intftype    = db.Column(db.Integer)
    
    ifspeed     = db.Column(db.Integer) # 带宽
    
    # 接收
    inoctets        = db.Column(db.Float)   # 均值速率
    inoctetsmax     = db.Column(db.Float)   # 峰值速率
    inpkts          = db.Column(db.Integer) # 报文均值
    inpktsmax       = db.Column(db.Integer) # 报文峰值
    inucastpkts     = db.Column(db.Integer) # 单播报文
    inmulticastpkts = db.Column(db.Float)   # 多播报文
    inbroadcastpkts = db.Column(db.Float)   # 广播报文
    innucastpkts    = db.Column(db.Float)   # 非单播报文
    indiscards      = db.Column(db.Float)   # 丢包
    inerrors        = db.Column(db.Float)   # 错误包
    inunknownprotos = db.Column(db.Float)   # 未知协议包
    
    # 发送
    outoctets       = db.Column(db.Float) 
    outoctetsmax    = db.Column(db.Float)
    outpkts         = db.Column(db.Float) 
    outpktsmax      = db.Column(db.Float) 
    outucastpkts    = db.Column(db.Float) 
    outmulticastpkts= db.Column(db.Float)
    outbroadcastpkts= db.Column(db.Float) 
    outnucastpkts   = db.Column(db.Float) 
    outdiscards     = db.Column(db.Float) 
    outerrors       = db.Column(db.Float)

    #====================================
    # 定义计算指标
    #====================================
    # <流量>
    @property #接收流量
    def inoctetstotal(self):
        return self.inoctets * 3600 / 8
        
    @property #发送流量
    def outoctetstotal(self):
        return self.outoctets * 3600 / 8
        
    @property #总流量
    def octetstotal(self):
        return self.inoctetstotal + self.outoctetstotal

    # <速率>
    @property #总峰值速率
    def octets(self):
        return self.inoctets + self.outoctets
        
    @property #总均值速率
    def octetsmax(self):
        return self.inoctetsmax + self.outoctetsmax

        
    # <带宽利用率>
    @property #接收带宽利用率
    def inbwusage(self):
        return (self.inoctets / self.ifspeed) * 100
        
    @property #发送带宽利用率
    def outbwusage(self):
        return (self.outoctets / self.ifspeed) * 100
        
    @property # 总带宽利用率
    def bwusage(self):
        return self.inbwusage + self.outbwusage
        
    @property #接收带宽利用率峰值
    def inbwusagemax(self):
        return (self.inoctetsmax / self.ifspeed) * 100

    @property #发送带宽利用率峰值
    def outbwusagemax(self):
        return (self.outoctetsmax / self.ifspeed) * 100

    @property # 总带宽利用率峰值
    def bwusagemax(self):
        return self.inbwusagemax + self.outbwusagemax
    
        
class PonTrafficPerf(db.Model, PerfMixin):
    """PON口流量"""
    __tablename__ = 'perf_pontraffic'

    ponidx      = db.Column(db.Integer)
    
    rx64bytes   = db.Column(db.Float) 
    tx64bytes   = db.Column(db.Float) 
    rx128bytes  = db.Column(db.Float)
    tx128bytes  = db.Column(db.Float)
    rx256bytes  = db.Column(db.Float)
    tx256bytes  = db.Column(db.Float)
    rx512bytes  = db.Column(db.Float)
    tx512bytes  = db.Column(db.Float)
    rx1024bytes = db.Column(db.Float)
    tx1024bytes = db.Column(db.Float)
    rx1518bytes = db.Column(db.Float)
    tx1518bytes = db.Column(db.Float)
    rxover1518bytes = db.Column(db.Float) 
    txover1518bytes = db.Column(db.Float) 


class PonUsagePerf(db.Model, PerfMixin):
    """PON口占用率"""
    __tablename__ = 'perf_ponusage'

    pontotal    = db.Column(db.Integer)
    ponused     = db.Column(db.Integer)
    ponusage    = db.Column(db.Float)

    @property
    def ponfree(self):
        return 1.0 - self.ponusage

    
class PonPowerPerf(db.Model, PerfMixin):
    """ PON口光功率 """
    __tablename__ = 'perf_ponpower'
    
    ponidx      = db.Column(db.Integer)
    
    rxpoweravg  = db.Column(db.Float) 
    rxpowermax  = db.Column(db.Float) 
    rxpowermin  = db.Column(db.Float) 
    txpoweravg  = db.Column(db.Float) 
    txpowermax  = db.Column(db.Float) 
    txpowermin  = db.Column(db.Float)
    
    currentavg  = db.Column(db.Float) 
    currentmax  = db.Column(db.Float) 
    currentmin  = db.Column(db.Float)
    
    voltageavg  = db.Column(db.Float) 
    voltagemax  = db.Column(db.Float) 
    voltagemin  = db.Column(db.Float) 

    tempavg     = db.Column(db.Float) 
    tempmax     = db.Column(db.Float) 
    tempmin     = db.Column(db.Float) 

