#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re

from wtforms import BooleanField
from wtforms import validators as v
from .models import Node, NodeOlt, Board, Port, Area, Vendor, Model,SNMP_VER_DICT

from flask_wtf import (Form, TextField, PasswordField, IntegerField,NumberRange,SubmitField,RadioField,
                       TextAreaField, ValidationError, required, equal_to, email)

from tango.form.forms import FormPro
from tango.form.fields import AreaSelectField
from tango.form.fields import SelectFieldPro
from tango.ui.tables.utils import Attrs
from tango.models import  Category

class NodeNewForm(FormPro):
    cityid          = SelectFieldPro(u'所属地市', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择地市')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==1)])
    town         = SelectFieldPro(u'', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择区县')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==2)])
    area_id         = SelectFieldPro(u'所属区域', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择分局')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==3)])
    name            = TextField(u'节点名称', validators=[required(message=u'必填')])
    alias           = TextField(u'节点别名', validators=[required(message=u'必填')])
    category_id     = SelectFieldPro(u'节点类型',validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择节点类型')] + [(unicode(r.id), r.alias) for r in Category.query.filter(Category.obj=="node").filter(Category.is_valid==1)])
    addr            = TextField(u'IP 地址', validators=[required(message=u'必填')])
    snmp_comm       = TextField(u'读团体名', validators=[required(message=u'必填')])
    snmp_wcomm      = TextField(u'写团体名', validators=[required(message=u'必填')])
    snmp_ver       = RadioField(u'SNMP版本',default=SNMP_VER_DICT.keys()[0], validators=[required(message=u'必填')],
        choices=[(value, label) for value, label in SNMP_VER_DICT.items()])
    location        = TextField(u'位置')
    remark          = TextAreaField(u'备注信息')

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )


class NodeSearchForm(FormPro):
    name            = TextField(u'IP 地址')
    area            = AreaSelectField(u'所属区域')
    vendor_id       = SelectFieldPro(u'生产厂家',
        choices=lambda: [('', u'请选择生产厂家')] + [(unicode(r.id), r.alias) for r in Vendor.query])
    model_id        = SelectFieldPro(u'设备型号',
        choices=lambda: [('', u'请选择设备型号')] + [(unicode(r.id), r.alias) for r in Model.query])

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )
        list_display = ('area','vendor_id','model_id')

class OltNewForm(FormPro):
    cityid          = SelectFieldPro(u'所属地市', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择地市')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==1)])
    town         = SelectFieldPro(u'', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择区县')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==2)])
    area_id         = SelectFieldPro(u'所属区域', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择分局')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==3)])
    name            = TextField(u'OLT名称', validators=[required(message=u'必填')])
    alias           = TextField(u'OLT别名', validators=[required(message=u'必填')])
    addr            = TextField(u'IP 地址', validators=[required(message=u'必填')])
    snmp_comm       = TextField(u'读团体名', validators=[required(message=u'必填')])
    snmp_wcomm      = TextField(u'写团体名', validators=[required(message=u'必填')])
    snmp_ver       = RadioField(u'SNMP版本',default=SNMP_VER_DICT.keys()[0], validators=[required(message=u'必填')],
        choices=[(value, label) for value, label in SNMP_VER_DICT.items()])
    vendor_id       = SelectFieldPro(u'OLT厂商', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择厂商')] + [(unicode(r.id), r.alias) for r in Vendor.query.filter(Vendor.is_valid==1)])
    model_id        = SelectFieldPro(u'OLT型号', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择型号')] + [(unicode(r.id), r.alias) for r in Model.query.filter(Model.is_valid==1).filter(Model.category_id==20)])
    mask            = TextField(u'子网掩码')
    location        = TextField(u'位置')
    remark          = TextAreaField(u'备注信息')

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )

class SwitchNewForm(FormPro):
    cityid          = SelectFieldPro(u'所属地市', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择地市')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==1)])
    town         = SelectFieldPro(u'', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择区县')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==2)])
    area_id         = SelectFieldPro(u'所属区域', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择接入点')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==4)])
    name            = TextField(u'交换机名称', validators=[required(message=u'必填')])
    alias           = TextField(u'交换机别名', validators=[required(message=u'必填')])
    addr            = TextField(u'IP 地址', validators=[required(message=u'必填')])
    snmp_comm       = TextField(u'读团体名', validators=[required(message=u'必填')])
    snmp_wcomm       = TextField(u'写团体名', validators=[required(message=u'必填')])
    snmp_ver       = RadioField(u'SNMP版本',default=SNMP_VER_DICT.keys()[0], validators=[required(message=u'必填')],
        choices=[(value, label) for value, label in SNMP_VER_DICT.items()])
    mask            = TextField(u'子网掩码')
    location        = TextField(u'位置')
    remark          = TextAreaField(u'备注信息')

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )

class OnuNewForm(FormPro):
    controller_id      = SelectFieldPro(u'所属OLT', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择OLT')] + [(unicode(r.id), r.alias+' <'+r.addr+'>') for r in NodeOlt.query])
    area_id         = SelectFieldPro(u'接入点', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择接入点')] + [(unicode(r.id), r.alias) for r in Area.query.filter(Area.area_type==4)])
    name            = TextField(u'ONU名称', validators=[required(message=u'必填')])
    alias           = TextField(u'ONU别名', validators=[required(message=u'必填')])
    addr            = TextField(u'IP 地址', validators=[required(message=u'必填')])
    snmp_comm       = TextField(u'读团体名', validators=[required(message=u'必填')])
    snmp_wcomm      = TextField(u'写团体名', validators=[required(message=u'必填')])
    snmp_ver       = RadioField(u'SNMP版本',default=SNMP_VER_DICT.keys()[0], validators=[required(message=u'必填')],
        choices=[(value, label) for value, label in SNMP_VER_DICT.items()])
    vendor_id       = SelectFieldPro(u'ONU厂商', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择厂商')] + [(unicode(r.id), r.alias) for r in Vendor.query.filter(Vendor.is_valid==1)])
    model_id        = SelectFieldPro(u'ONU型号', validators=[required(message=u'必填')],
        choices=lambda: [('', u'请选择型号')] + [(unicode(r.id), r.alias) for r in Model.query.filter(Model.is_valid==1).filter(Model.category_id==21)])
    mask            = TextField(u'子网掩码')
    location        = TextField(u'位置')
    remark          = TextAreaField(u'备注信息')

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )

class OltSearchForm(FormPro):
    name            = TextField(u'IP 地址')
    area            = AreaSelectField(u'所属区域')
    vendor_id       = SelectFieldPro(u'生产厂家',
        choices=lambda: [('', u'请选择生产厂家')] + [(unicode(r.id), r.alias) for r in Vendor.query])
    model_id        = SelectFieldPro(u'设备型号',
        choices=lambda: [('', u'请选择设备型号')] + [(unicode(r.id), r.alias) for r in Model.query])

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )
        list_display = ('area','vendor_id','model_id')

class OnuSearchForm(FormPro):
    name            = TextField(u'IP 地址')
    area            = AreaSelectField(u'所属区域')
    vendor_id       = SelectFieldPro(u'生产厂家',
        choices=lambda: [('', u'请选择生产厂家')] + [(unicode(r.id), r.alias) for r in Vendor.query])
    model_id        = SelectFieldPro(u'设备型号',
        choices=lambda: [('', u'请选择设备型号')] + [(unicode(r.id), r.alias) for r in Model.query])

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )
        list_display = ('area','vendor_id','model_id')

class SwitchSearchForm(FormPro):
    name            = TextField(u'IP 地址')
    area            = AreaSelectField(u'所属区域')
    vendor_id       = SelectFieldPro(u'生产厂家',
        choices=lambda: [('', u'请选择生产厂家')] + [(unicode(r.id), r.alias) for r in Vendor.query])
    model_id        = SelectFieldPro(u'设备型号',
        choices=lambda: [('', u'请选择设备型号')] + [(unicode(r.id), r.alias) for r in Model.query])

    class Meta():
        attrs = Attrs(
            label={'style':'width:80px;text-align: right;padding-bottom: 10px;'},
            field={'style':'padding-left: 10px;padding-bottom: 10px;'}
        )
        list_display = ('area','vendor_id','model_id')
