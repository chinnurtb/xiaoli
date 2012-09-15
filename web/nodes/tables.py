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
    #town_count     = tables.Column(verbose_name=u'区县')
    town_count      = tables.LinkColumn(verbose_name=u'区县')
    branch_count     = tables.LinkColumn(verbose_name=u'分局')
    entrance_count     = tables.LinkColumn(verbose_name=u'接入点')
    total_count     = tables.Column(verbose_name=u'节点数')
    olt_count     = tables.Column(verbose_name=u'OLT数')
    onu_count     = tables.Column(verbose_name=u'ONU数')
    dslam_count     = tables.Column(verbose_name=u'DSLAM数')
    eoc_count     = tables.Column(verbose_name=u'EOC数')
    switch_count     = tables.Column(verbose_name=u'Switch数')

    # 根据节点类型决定 name是否有链接，接入点没有链接
    def render_name(self, value, record, bound_column):
        if record.area_type == 4:
            return value
        else:
            html = u'''<a href="/areas/?base={id}">{text}</a>'''.format(
                id = record.id,
                text = record.name
            )
            return Markup(html)

    class Meta():
        model = Node
        url_makers = {
            'town_count': lambda record: url_for('nodes.areas', base=record.id, query_gran=2),
            'branch_count':lambda record: url_for('nodes.areas', base=record.id, query_gran=3),
            'entrance_count':lambda record: url_for('nodes.areas', base=record.id, query_gran=4),
        }

class CategoryTable(tables.Table):
    category_name   = tables.EnumColumn(verbose_name=u'类型',name='category',
        enums={1:u'OLT',2:u'ONU',3:u'DSLAM',4:u'EOC',5:u'Switch'}, orderable=True)
    total_count  = tables.Column(verbose_name=u'数量')
    status1_count = tables.Column(verbose_name=u'可用')
    status0_count = tables.Column(verbose_name=u'不可用')
    node_status_percent = tables.Column(verbose_name=u'可用率',attrs=Attrs(th={"style": "width:175px;"}))

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
        <div class="progress pull-left" style="height:9px;width:100px;margin:4px 5px 5px 0;">
            <div class="bar {bar}" style="width:{text}"></div>
        </div>
        <div class="pull-left"><span style="color:{font_color};">{text}&nbsp;&nbsp;</span></div>
        '''.format(text=text,bar=bar,font_color=font_color)
        return Markup(html)

    class Meta():
        model = Node


class VendorTable(tables.Table):
    alias       = tables.Column(verbose_name=u'名称', orderable=True)
    node_count  = tables.Column(verbose_name=u'数量')
    node_status1_count  = tables.Column(verbose_name=u'可用')
    node_status0_count  = tables.Column(verbose_name=u'不可用')
    node_status_percent = tables.Column(verbose_name=u'可用率',attrs=Attrs(th={"style": "width:175px;"}))

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
        <div class="progress pull-left" style="height:9px;width:100px;margin:4px 5px 5px 0;">
            <div class="bar {bar}" style="width:{text}"></div>
        </div>
        <div class="pull-left"><span style="color:{font_color};">{text}&nbsp;&nbsp;</span></div>
        '''.format(text=text,bar=bar,font_color=font_color)
        return Markup(html)

    class Meta():
        model = Node