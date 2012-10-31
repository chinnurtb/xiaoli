# coding=utf-8

from tango import db

class NodePerf(db.Model):

    '''节点性能'''
    
    __tablename__ = 'perf_node'

    id          = db.Column(db.Integer, primary_key=True)
    nodeid      = db.Column(db.Integer, db.ForeignKey("nodes.id"))
    sampletime  = db.Column(db.DateTime)
    pingrta     = db.Column(db.Float)
    pingrtmax   = db.Column(db.Float)
    pingrtmin   = db.Column(db.Float)
    pingloss    = db.Column(db.Integer)

    node        = db.relation('Node')

class BoardPerf(db.Model):
    '''版卡性能'''
    
    __tablename__ = 'perf_board'

    id          = db.Column(db.Integer, primary_key=True)
    nodeid      = db.Column(db.Integer, db.ForeignKey("nodes.id"))
    sampletime  = db.Column(db.DateTime)
    cpuavg      = db.Column(db.Integer)
    cpumax      = db.Column(db.Integer) 
    memavg      = db.Column(db.Integer) 
    memmax      = db.Column(db.Integer) 
    tempavg     = db.Column(db.Integer) 
    tempmax     = db.Column(db.Integer) 

    node        = db.relation('Node')
    

class PortPerf(db.Model):

    '''端口性能'''

    __tablename__ = 'perf_port'

    id          = db.Column(db.Integer, primary_key=True)
    nodeid      = db.Column(db.Integer, db.ForeignKey("nodes.id"))
    portid      = db.Column(db.Integer, db.ForeignKey("ports.id"))
    sampletime  = db.Column(db.DateTime)

    inoctets    = db.Column(db.Float)
    inoctetsmax = db.Column(db.Float)
    inpkts      = db.Column(db.Integer)
    inpktsmax   = db.Column(db.Integer)
    inucastpkts = db.Column(db.Integer)

    inmulticastpkts = db.Column(db.Integer)
    inbroadcastpkts = db.Column(db.Integer)
    innucastpkts    = db.Column(db.Integer)
    indiscards      = db.Column(db.Integer)
    inerrors        = db.Column(db.Integer)
    inunknownprotos = db.Column(db.Integer)

    outoctets       = db.Column(db.Float)
    outoctetsmax    = db.Column(db.Float)
    outpkts         = db.Column(db.Integer) 
    outpktsmax      = db.Column(db.Integer) 
    outucastpkts    = db.Column(db.Integer) 
    outmulticastpkts= db.Column(db.Integer)
    outbroadcastpkts= db.Column(db.Integer) 
    outnucastpkts   = db.Column(db.Integer) 
    outdiscards     = db.Column(db.Integer) 
    outerrors       = db.Column(db.Integer)

    node            = db.relation('Node')
    port            = db.relation('Port')


    #TODO: 定义计算函数
    #接收流量
    #接收带宽利用率
    #接收带宽利用率峰值
    #发送流量
    #发送带宽利用率
    #发送带宽利用率峰值
