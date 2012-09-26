#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re

from wtforms import BooleanField 
from wtforms import validators as v
from .models import Node, Board, Port, Area, Vendor, Model

from flask_wtf import (Form, TextField, PasswordField, IntegerField,NumberRange,
                       TextAreaField, ValidationError, required, equal_to, email)

from tango.form.forms import FormPro
from tango.form.fields import AreaSelectField
from tango.form.fields import SelectFieldPro
from tango.ui.tables.utils import Attrs

class NodeNewForm(FormPro):
    area_id         = AreaSelectField(u'所属区域', select_mode=1, validators=[required(message=u'必填')])
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

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )


class NodeSearchForm(FormPro):
    name            = TextField(u'节点名称')
    area            = AreaSelectField(u'所属区域')
    vendor_id       = SelectFieldPro(u'生产厂家',
        choices=lambda: [('', u'请选择生产厂家')] + [(unicode(r.id), r.alias) for r in Vendor.query])
    model_id        = SelectFieldPro(u'设备型号',
        choices=lambda: [('', u'请选择设备型号')] + [(unicode(r.id), r.alias) for r in Model.query])
    ip              = TextField(u'IP 地址', validators=[required(message=u'必填')])

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )
        list_display = ('name','area','vendor_id','model_id')
