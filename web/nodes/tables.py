#!/usr/bin/env python  
# -*- coding: utf-8 -*-
from flask import url_for
from tango.ui import tables
from tango.ui.tables.utils import Attrs
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

class CategoryTable(tables.Table):
    category_name   = tables.EnumColumn(verbose_name=u'类型',name='category',
        enums={1:u'OLT',2:u'ONU',3:u'DSLAM',4:u'EOC',5:u'Switch'}, orderable=True)
    total_count  = tables.Column(verbose_name=u'数量')
    status1_count = tables.Column(verbose_name=u'可用')
    status0_count = tables.Column(verbose_name=u'不可用')
    node_status_percent = tables.Column(verbose_name=u'可用率',attrs=Attrs(th={"style": "width:190px;"}))

    def render_node_status_percent(self, value, record, bound_column):
        percent = (record.status1_count*1.0 / record.total_count) if int(record.total_count) != 0 else 0
        text= '%.2f' % (percent * 100) + '%'
        if percent < 0.5:
            bar = 'bar-danger'
            font_color = '#DD514C'
        elif percent >= 0.5 and percent < 0.9:
            bar = 'bar-warning'
            font_color = '#FAA732'
        elif percent >= 0.9:
            bar = 'bar-success'
            font_color = '#5EB95E'
        html = u'''
        <div class="pull-left"><span style="color:{font_color};">{text}&nbsp;&nbsp;</span></div>
        <div class="progress pull-right" style="width:130px;margin-bottom:5px;">
            <div class="bar {bar}" style="width:{text}"></div>
        </div>
        '''.format(text=text,bar=bar,font_color=font_color)
        return Markup(html)


class VendorTable(tables.Table):
    alias       = tables.Column(verbose_name=u'名称', orderable=True)
    node_count  = tables.Column(verbose_name=u'数量')
    node_status1_count  = tables.Column(verbose_name=u'可用')
    node_status0_count  = tables.Column(verbose_name=u'不可用')
    node_status_percent = tables.Column(verbose_name=u'可用率',attrs=Attrs(th={"style": "width:190px;"}))

    def render_node_status_percent(self, value, record, bound_column):
        percent = (record.node_status1_count*1.0 / record.node_count) if int(record.node_count) != 0 else 0
        text= '%.2f' % (percent * 100) + '%'
        if percent < 0.5:
            bar = 'bar-danger'
            font_color = '#DD514C'
        elif percent >= 0.5 and percent < 0.9:
            bar = 'bar-warning'
            font_color = '#FAA732'
        elif percent >= 0.9:
            bar = 'bar-success'
            font_color = '#5EB95E'
        html = u'''
        <div class="pull-left"><span style="color:{font_color};">{text}&nbsp;&nbsp;</span></div>
        <div class="progress pull-right" style="width:130px;margin-bottom:5px;">
            <div class="bar {bar}" style="width:{text}"></div>
        </div>
        '''.format(text=text,bar=bar,font_color=font_color)
        return Markup(html)