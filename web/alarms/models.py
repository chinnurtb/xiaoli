# coding: utf-8

from tango import db

from tango.models import Category

from sqlalchemy import select, func

from sqlalchemy.orm import column_property, object_session

class Alarm(db.Model):
    
    """告警表"""

    __tablename__ = 'alarms'

    id                      = db.Column(db.Integer, primary_key=True)
    alarm_key               = db.Column(db.String(200))
    class_id                = db.Column(db.Integer, db.ForeignKey('alarm_classes.id'))
    alarm_name              = db.Column(db.String(60))
    alarm_alias             = db.Column(db.String(200))
    alarm_state             = db.Column(db.Integer)
    manager                 = db.Column(db.String(60))   
    agent                   = db.Column(db.String(60))  
    node_id                 = db.Column(db.Integer, db.ForeignKey('nodes.id')) 
    #node_class              = db.Column(db.Integer) 
    #node_alias              = db.Column(db.String(200))  
    #node_addr               = db.Column(db.String(100))  
    #node_managed_state      = db.Column(db.Integer) 
    #node_prod_state         = db.Column(db.Integer) 
    source                  = db.Column(db.String(60))
    source_class            = db.Column(db.String(60))  
    severity                = db.Column(db.Integer) 
    summary                 = db.Column(db.String(200))
    state_change            = db.Column(db.DateTime)
    first_occurrence        = db.Column(db.DateTime)
    last_occurrence         = db.Column(db.DateTime)
    occur_count             = db.Column(db.Integer) 
    priority                = db.Column(db.Integer)
    graded                  = db.Column(db.Integer) 
    location                = db.Column(db.String(100))  
    service                 = db.Column(db.String(100))  
    customer                = db.Column(db.String(100))  
    sequence_no             = db.Column(db.Integer) 
    x733_type               = db.Column(db.Integer) 
    probable_cause          = db.Column(db.String(200))  
    specific_problem        = db.Column(db.String(200)) 
    additional_information  = db.Column(db.String(200))
    proposed_repaire_action = db.Column(db.String(200))  
    acked                   = db.Column(db.Integer) 
    acked_user              = db.Column(db.String(60))   
    acked_time              = db.Column(db.DateTime) 
    acked_note              = db.Column(db.String(60))   
    cleared                 = db.Column(db.Integer) 
    cleared_user            = db.Column(db.String(60))   
    cleared_time            = db.Column(db.DateTime) 
    cleared_note            = db.Column(db.String(100)) 
    order_state             = db.Column(db.Integer) 
    root_cause              = db.Column(db.Integer) 
    cause_type              = db.Column(db.Integer) 
    extended_attrs          = db.Column(db.Text)
    created_at              = db.Column(db.DateTime) 
    updated_at              = db.Column(db.DateTime) 

    alarm_class             = db.relation('AlarmClass')
    node                    = db.relation('Node', backref=db.backref("alarms"))

    def __repr__(self):
        return '<Alarm%r>' % self.alarm_alias

    def __unicode__(self):
        return u'<当前告警 %s>'% self.alarm_alias

class AlarmClass(db.Model):

    """告警类型表"""

    __tablename__ = 'alarm_classes'

    id = db.Column(db.Integer, primary_key=True)
    category_id = db.Column(db.Integer, db.ForeignKey('categories.id'))
    name = db.Column(db.String(60))
    alias = db.Column(db.String(60))
    severity = db.Column(db.Integer)
    probable_cause = db.Column(db.String(200))
    specific_problem = db.Column(db.String(200))
    additional_info = db.Column(db.String(200))
    remark = db.Column(db.String(100))
    created_at = db.Column(db.DateTime)
    updated_at  = db.Column(db.DateTime)

    category    = db.relation('Category')


    def __repr__(self):
        return '<AlarmClass %r>' % self.name

    def __unicode__(self):
        return u'<告警类型 %s>'% self.alias

class AlarmJournal(db.Model):

    __tablename__ = 'alarm_journals'

    id          = db.Column(db.Integer, primary_key=True)
    uid         = db.Column(db.Integer, db.ForeignKey("users.id"))
    alarm_id    = db.Column(db.Integer, db.ForeignKey("alarms.id"))
    title       = db.Column(db.String(200))
    summary     = db.Column(db.String(200))
    created_at  = db.Column(db.DateTime)

    user        = db.relationship('User')
    alarm       = db.relationship('Alarm', backref=db.backref('journals', order_by=id))

class AlarmVar(db.Model):

    __tablename__ = 'alarm_vars'

    id          = db.Column(db.Integer, primary_key=True)
    alarm_id    = db.Column(db.Integer, db.ForeignKey("alarms.id"))
    name        = db.Column(db.String(60))
    value       = db.Column(db.String(100))

    alarm       = db.relationship('Alarm', backref=db.backref('vars', order_by=id))

class AlarmSeverity(db.Model):

    __tablename__ = 'alarm_severities'

    id          = db.Column(db.Integer, primary_key=True)
    name        = db.Column(db.String(60))
    alias       = db.Column(db.String(60))
    color       = db.Column(db.String(60)) 
    sound       = db.Column(db.String(60))
    remark      = db.Column(db.String(60))

    @property
    def count(self):
        from tango.login import current_user
        from nodes.models import Node, Area
        query = object_session(self).query(func.count(Alarm.id)).filter(Alarm.severity == self.id)
        if not current_user.is_province_user:
            query = query.outerjoin(Node, Node.id == Alarm.node_id)
            query = query.outerjoin(Area, Area.id == Node.area_id)
            query = query.filter(current_user.domain.clause_permit)
        return query.scalar()

    @staticmethod
    def name2id(name):
        name_dict = {
            'clear'         : 0,
            'indeterminate' : 1,
            'warning'       : 2,
            'minor'         : 3,
            'major'         : 4,
            'critical'      : 5
        }
        return name_dict[name] if name in name_dict else -1

    def __repr__(self):
        return '<Severity%r>' % self.name

    def __unicode__(self):
        return u'<告警级别 %s>' % self.alias

def query_severities():
    return AlarmSeverity.query.order_by(AlarmSeverity.id.desc()).all()

class AlarmKnowledge(db.Model):

    __tablename__ = 'alarm_knowledges'

    id              = db.Column(db.Integer, primary_key=True)
    class_id        = db.Column(db.Integer, db.ForeignKey("alarm_classes.id"))
    probable_cause  = db.Column(db.String(200))
    resolvent       = db.Column(db.String(200))
    probability     = db.Column(db.Integer)
    apply_count     = db.Column(db.Integer)
    created_at      = db.Column(db.DateTime)
    updated_at      = db.Column(db.DateTime)

    alarm_class     = db.relation('AlarmClass')

    def __unicode__(self):
        return u'<告警知识库 %s>'% self.alarm_alias

class History(db.Model):

    __tablename__ = 'histories'

    id = db.Column(db.Integer, primary_key=True)
    alarm_key               = db.Column(db.String(200))
    alarm_class             = db.Column(db.String(60))
    alarm_name              = db.Column(db.String(60))
    alarm_alias             = db.Column(db.String(200))
    alarm_state             = db.Column(db.Integer)
    manager                 = db.Column(db.String(60))   
    agent                   = db.Column(db.String(60))  
    node_id                 = db.Column(db.Integer, db.ForeignKey('nodes.id')) 
    node_class              = db.Column(db.Integer) 
    node_alias              = db.Column(db.String(200))  
    node_addr               = db.Column(db.String(100))  
    node_managed_state      = db.Column(db.Integer) 
    node_prod_state         = db.Column(db.Integer) 
    source                  = db.Column(db.String(60))
    source_class            = db.Column(db.String(60))  
    severity                = db.Column(db.Integer) 
    summary                 = db.Column(db.String(200))
    state_change            = db.Column(db.DateTime)
    first_occurrence        = db.Column(db.DateTime)
    last_occurrence         = db.Column(db.DateTime)
    occur_count             = db.Column(db.Integer) 
    priority                = db.Column(db.Integer)
    graded                  = db.Column(db.Integer) 
    location                = db.Column(db.String(100))  
    service                 = db.Column(db.String(100))  
    customer                = db.Column(db.String(100))  
    sequence_no             = db.Column(db.Integer) 
    x733_type               = db.Column(db.Integer) 
    probable_cause          = db.Column(db.String(200))  
    specific_problem        = db.Column(db.String(200)) 
    additional_information  = db.Column(db.String(200))
    proposed_repaire_action = db.Column(db.String(200))  
    acked                   = db.Column(db.Integer) 
    acked_user              = db.Column(db.String(60))   
    acked_time              = db.Column(db.DateTime) 
    acked_note              = db.Column(db.String(60))   
    cleared                 = db.Column(db.Integer) 
    cleared_user            = db.Column(db.String(60))   
    cleared_time            = db.Column(db.DateTime) 
    cleared_note            = db.Column(db.String(100)) 
    order_state             = db.Column(db.Integer) 
    root_cause              = db.Column(db.Integer) 
    cause_type              = db.Column(db.Integer) 
    extended_attrs          = db.Column(db.Text)
    created_at              = db.Column(db.DateTime) 
    updated_at              = db.Column(db.DateTime) 

    #node                    = db.relation('Node', backref=db.backref("histories"))
    def __unicode__(self):
        return u'<历史告警 %s>'% self.alarm_alias
