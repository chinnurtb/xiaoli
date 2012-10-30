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

class Metric(db.Model):
    ''' 指标管理 '''
    
    __tablename__ = 'metrics'
    id     = db.Column(db.Integer, primary_key=True)
    grp    = db.Column(db.String(60))
    name   = db.Column(db.String(60))
    alias  = db.Column(db.String(100))
    calc   = db.Column(db.String(60))
    unit   = db.Column(db.String(200))
    format = db.Column(db.String(200))
    descr  = db.Column(db.String(200))

class Threshold(db.Model):
    __tablename__ = 'thresholds'
    
    id             = db.Column(db.Integer, primary_key=True)
    category_id    = db.Column(db.Integer, db.ForeignKey('categories.id'))
    metric_id      = db.Column(db.Integer, db.ForeignKey('metrics.id'))
    name           = db.Column(db.String(60)) # 后台生成
    alias          = db.Column(db.String(100))
    enabled        = db.Column(db.Integer, default=1)
    alarm_class_id = db.Column(db.Integer, db.ForeignKey('alarm_classes.id'))
    occur_count    = db.Column(db.Integer, default=1)
    summary        = db.Column(db.String(200))
    
    occur_cond1    = db.Column(db.String(60))
    restore_cond1  = db.Column(db.String(60))
    severity1      = db.Column(db.Integer, default=4)
    
    occur_cond2    = db.Column(db.String(60))
    restore_cond2  = db.Column(db.String(60))
    severity2      = db.Column(db.Integer, default=2)

    category    = db.relationship('Category')
    metric      = db.relationship('Metric')
    alarm_class = db.relationship('AlarmClass')


