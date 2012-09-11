#!/usr/bin/env python  
# -*- coding: utf-8 -*-
from flask import url_for
from tango.ui import tables
from jinja2 import Markup
from .models import Node,Board,Port

class NodeTable(tables.Table):
    check       = tables.CheckBoxColumn()
    status      = tables.EnumColumn(verbose_name=u'状态', name='state', enums={0: u'不可用', 1: u'可用'},  orderable=True)
    name        = tables.LinkColumn(endpoint='nodes.node_edit',verbose_name=u'名称',orderable=True)
    category    = tables.EnumColumn(verbose_name=u'类型',name='category', enums={1:u'OLT',2:u'ONU',3:u'DSLAM',4:u'EOC',5:u'Switch'}, orderable=True)
    addr        = tables.Column(verbose_name=u'IP', orderable=True)
    full_name   = tables.Column(verbose_name=u'所属区域', orderable=True, accessor='area.full_name')
    vendor_name = tables.Column(verbose_name=u'厂家', orderable=True, accessor='vendor.name')
    model_name  = tables.Column(verbose_name=u'型号', orderable=True, accessor='model.name')

    class Meta():
        model = Node
        per_page = 30
        order_by = '-alias'
        #url_makers = {'name': lambda record: url_for('nodes.node_edit', id=record.id)}

class BoardTable(tables.Table):
    check       = tables.CheckBoxColumn()
    status      = tables.Column(verbose_name=u'状态')
    alias       = tables.Column(verbose_name=u'名称', orderable=True)
    class Meta():
        model = Board
        per_page = 30
        order_by = '-alias'

class PortTable(tables.Table):
    check       = tables.CheckBoxColumn()
    alias       = tables.Column(verbose_name=u'名称', orderable=True)
    class Meta():
        model = Port
        per_page = 30
        order_by = '-alias'

class AreaTable(tables.Table):
    name        = tables.Column(verbose_name=u'名称', orderable=True)
    node_count     = tables.Column(verbose_name=u'节点数量')
    onu_num     = tables.Column(verbose_name=u'ONU数量')

class VendorTable(tables.Table):
    alias       = tables.Column(verbose_name=u'名称', orderable=True)
    node_count  = tables.Column(verbose_name=u'数量')
    node_status1_count  = tables.Column(verbose_name=u'可用')
    node_status0_count  = tables.Column(verbose_name=u'不可用')

    def render_node_count(self, value):
        html = u'<span style="color:red;">{text}</span>'.format(
            text= '%.2f' % value
        )
        return Markup(html)
