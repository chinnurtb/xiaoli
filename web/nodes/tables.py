#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango.ui import tables

from .models import Node,Board,Port

class NodeTable(tables.Table):
    check       = tables.CheckBoxColumn()
    status      = tables.Column(verbose_name=u'状态')
    name        = tables.Column(verbose_name=u'名称', orderable=True)
    addr        = tables.Column(verbose_name=u'IP', orderable=True)
    area_name   = tables.Column(verbose_name=u'位置', orderable=True, accessor='area.name')
    vendor_name = tables.Column(verbose_name=u'厂家', orderable=True, accessor='vendor.name')
    model_name  = tables.Column(verbose_name=u'型号', orderable=True, accessor='model.name')

    class Meta():
        model = Node
        per_page = 30
        order_by = '-alias'

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
