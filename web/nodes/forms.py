#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re

from wtforms import BooleanField 
from wtforms import validators as v
from tango.forms import SelectFieldPro
from .models import Node, Board, Port, Area, Vendor, Model

from flask_wtf import (Form, TextField, PasswordField, IntegerField,NumberRange,
                       TextAreaField, ValidationError, required, equal_to, email)

class NodeNewForm(Form):
    area_id         = SelectFieldPro(u'所属区域',validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择区域')] + [(unicode(r.id), r.name) for r in Area.query])
    name            = TextField(u'节点名称', validators=[required(message=u'必填')])
    category        = SelectFieldPro(u'节点类型',validators=[required(message=u'必填')],
        choices=[('',u'请选择节点类型'),('1',u'OLT'),('2',u'ONU'),('3',u'DSLAM'),('4',u'EOC'),('5',u'Switch')])
    addr            = TextField(u'IP 地址', validators=[required(message=u'必填')])
    vendor_id       = SelectFieldPro(u'生产厂家',validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择生产厂家')] + [(unicode(r.id), r.alias) for r in Vendor.query])
    model_id        = SelectFieldPro(u'设备型号',validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择设备型号')] + [(unicode(r.id), r.alias) for r in Model.query])
    snmp_comm       = TextField(u'读团体名')
    snmp_wcomm      = TextField(u'写团体名')
    snmp_ver        = TextField(u'SNMP版本')
    snmp_port       = IntegerField(u'SNMP端口', default=0, validators=[NumberRange(min=0,message=u"端口不能是负数")])

class NodeSearchForm(Form):
    area_id         = SelectFieldPro(u'所属区域',
        choices=lambda: [('', u'请选择区域')] + [(unicode(r.id), r.name) for r in Area.query])
    name            = TextField(u'节点名称', validators=[required(message=u'必填')])
    ip              = TextField(u'IP 地址', validators=[required(message=u'必填')])
    vendor_id       = SelectFieldPro(u'生产厂家',
        choices=lambda: [('', u'请选择生产厂家')] + [(unicode(r.id), r.alias) for r in Vendor.query])
    model_id        = SelectFieldPro(u'设备型号',
        choices=lambda: [('', u'请选择设备型号')] + [(unicode(r.id), r.alias) for r in Model.query])
