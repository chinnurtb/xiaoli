#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango import db

class Alarm(db.Model):
    __tablename__ = 'alarms'
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

    node                    = db.relation('Node')

    def __repr__(self):
        return '<Alarm%r>' % self.alarm_alias

