#!/usr/bin/env python  
# -*- coding: utf-8 -*-
from jinja2 import Markup
from flask import url_for

from tango.ui import tables
from tango.ui.tables.utils import Attrs

from nodes import constants
from .models import Node, NodeSwitch, NodeRouter, NodeOlt, NodeOnu, Board, Port, NodeHost, NODE_STATUS_DICT

redirct_dict = {
    1: "routers",
    2: "switches",
    20: "olts",
    21: "onus",
    50: "eocs"
}

def redirect_node_show(record):
    return u'/nodes/%s/%s/' % (redirct_dict.get(record.category_id), record.id)

def redirect_node_edit(record):
    return u'/nodes/%s/edit/%s/' % (redirct_dict.get(record.category_id), record.id)

class NodeTable(tables.Table):
    edit = tables.Action(name=u'编辑',url=lambda record: redirect_node_edit(record))
    delete = tables.Action(name=u'删除', endpoint='nodes.nodes_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = tables.EnumColumn(
        verbose_name=u'状态',
        name='state',
        enums=NODE_STATUS_DICT,
        orderable=True
    )
    name = tables.LinkColumn(verbose_name=u'名称',orderable=True)
    alias = tables.Column(verbose_name=u'别名',orderable=True)
    category = tables.Column(verbose_name=u'节点类型',orderable=True, accessor='category.alias')
    vendor_name = tables.Column(verbose_name=u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(verbose_name=u'型号', orderable=True, accessor='model.alias')
    addr = tables.Column(verbose_name=u'IP', orderable=True)
    area_name = tables.Column(verbose_name=u'所属区域', orderable=True, accessor='area.full_name')
    last_check = tables.Column(verbose_name=u'上次同步',orderable=True)
    summary = tables.Column(verbose_name=u'状态信息')

    class Meta():
        model = Node
        per_page = 30
        url_makers = {
            'name': lambda record: redirect_node_show(record),
            'edit': lambda record: redirect_node_show(record)
        }

class OltTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.olts_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.olts_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = tables.EnumColumn(
        verbose_name=u'状态',
        name='state',
        enums=NODE_STATUS_DICT,
        orderable=True
    )
    name = tables.LinkColumn(endpoint='nodes.olts_show',verbose_name=u'名称',orderable=True)
    alias = tables.Column(verbose_name=u'别名',orderable=True)
    vendor_name = tables.Column(verbose_name=u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(verbose_name=u'型号', orderable=True, accessor='model.alias')
    addr = tables.Column(verbose_name=u'IP', orderable=True)
    area_name = tables.Column(verbose_name=u'所属区域', orderable=True, accessor='area.full_name')
    last_check = tables.Column(verbose_name=u'上次同步',orderable=True)
    onu_count_plan = tables.Column(verbose_name=u'已规划ONU')
    onu_count_unplan = tables.Column(verbose_name=u'未规划ONU')

    class Meta():
        model = NodeOlt
        per_page = 30

class SwitchTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.switches_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.switches_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = tables.EnumColumn(
        verbose_name=u'状态',
        name='state',
        enums=NODE_STATUS_DICT,
        orderable=True
    )
    name = tables.LinkColumn(endpoint='nodes.switches_show',verbose_name=u'名称',orderable=True)
    addr = tables.Column(verbose_name=u'IP', orderable=True)
    area_name = tables.Column(verbose_name=u'所属区域', orderable=True, accessor='area.full_name')
    vendor_name = tables.Column(verbose_name=u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(verbose_name=u'型号', orderable=True, accessor='model.alias')
    last_check = tables.Column(verbose_name=u'上次同步',orderable=True)
    location = tables.Column(verbose_name=u'位置')

    class Meta():
        model = NodeSwitch
        per_page = 30

class RouterTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.routers_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.routers_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = tables.EnumColumn(
        verbose_name=u'状态',
        name='state',
        enums=NODE_STATUS_DICT,
        orderable=True
    )
    name = tables.LinkColumn(endpoint='nodes.routers_show',verbose_name=u'名称',orderable=True)
    addr = tables.Column(verbose_name=u'IP', orderable=True)
    area_name = tables.Column(verbose_name=u'所属区域', orderable=True, accessor='area.full_name')
    vendor_name = tables.Column(verbose_name=u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(verbose_name=u'型号', orderable=True, accessor='model.alias')
    last_check = tables.Column(verbose_name=u'上次同步',orderable=True)
    location = tables.Column(verbose_name=u'位置')

    class Meta():
        model = NodeRouter
        per_page = 30

class OnuTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.onus_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.onus_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = tables.EnumColumn(
        verbose_name=u'状态',
        name='state',
        enums=NODE_STATUS_DICT,
        orderable=True
    )
    name = tables.LinkColumn(endpoint='nodes.onus_show',verbose_name=u'名称',orderable=True)
    addr = tables.Column(verbose_name=u'IP', orderable=True)
    area_name = tables.Column(verbose_name=u'所属区域', orderable=True, accessor='area.full_name')
    vendor_name = tables.Column(verbose_name=u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(verbose_name=u'型号', orderable=True, accessor='model.alias')
    mac = tables.Column(verbose_name=u'认证标识',orderable=True)
    olt_border_no = tables.Column(verbose_name=u'OLT板位')
    olt_name = tables.Column(verbose_name=u'所属OLT', accessor='olt.alias')

    class Meta():
        model = NodeOnu
        per_page = 30

class NodeHostTable(tables.Table):
    helpdoc = u'查看所有主机'
    edit    = tables.Action(name=u'编辑', endpoint='system.hosts_edit')
    check   = tables.CheckBoxColumn()

    name       = tables.Column(verbose_name=u'名称')
    alias      = tables.Column(verbose_name=u'显示名')
    status     = tables.EnumColumn('state', verbose_name=u'状态', enums=constants.STATUS, orderable=True)
    ifaces     = tables.Column(verbose_name=u'接口地址')
    cpu_info   = tables.Column(verbose_name=u'CPU 信息')
    mem_info   = tables.Column(verbose_name=u'内存 信息')
    disk_info  = tables.Column(verbose_name=u'磁盘 信息')
    worker_num = tables.Column(verbose_name=u'采集进程数')
    os_type    = tables.Column(verbose_name=u'操作系统')
    updated_at = tables.DateTimeColumn(verbose_name=u'更新时间')

    class Meta():
        model = NodeHost

        
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
    total_count     = tables.Column(verbose_name=u'节点')
    olt_count     = tables.Column(verbose_name=u'OLT')
    onu_count     = tables.Column(verbose_name=u'ONU')
    dslam_count     = tables.Column(verbose_name=u'DSLAM')
    eoc_count     = tables.Column(verbose_name=u'EOC')
    switch_count     = tables.Column(verbose_name=u'Switch')

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

class AreaStatisticsTable(tables.Table):
    name        = tables.Column(verbose_name=u'区域')
    total_count     = tables.Column(verbose_name=u'节点')
    router_count     = tables.Column(verbose_name=u'路由器')
    switch_count     = tables.Column(verbose_name=u'交换机')
    firewall_count     = tables.Column(verbose_name=u'防火墙')
    bras_count     = tables.Column(verbose_name=u'BRAS')
    olt_count     = tables.Column(verbose_name=u'OLT')
    onu_count     = tables.Column(verbose_name=u'ONU')
    ac_count     = tables.Column(verbose_name=u'AC')
    fatap_count     = tables.Column(verbose_name=u'FatAP')
    fitap_count     = tables.Column(verbose_name=u'FitAP')
    dslam_count     = tables.Column(verbose_name=u'DSLAM')
    eoc_count     = tables.Column(verbose_name=u'EOC')
    cpe_count     = tables.Column(verbose_name=u'CPE')

    class Meta():
        model = Node

class CategoryTable(tables.Table):
    category_name   = tables.EnumColumn(verbose_name=u'类型',name='category',
        enums={1:u'OLT',2:u'ONU',3:u'DSLAM',4:u'EOC',5:u'Switch'}, orderable=True)
    total_count  = tables.Column(verbose_name=u'数量')
    status1_count  = tables.Column(verbose_name=u'正常')
    status2_count  = tables.Column(verbose_name=u'宕机')
    status3_count  = tables.Column(verbose_name=u'不可达')
    status4_count  = tables.Column(verbose_name=u'未监控')

    class Meta():
        model = Node

class VendorTable(tables.Table):
    alias       = tables.Column(verbose_name=u'名称', orderable=True)
    node_count  = tables.Column(verbose_name=u'数量')
    node_status1_count  = tables.Column(verbose_name=u'正常')
    node_status2_count  = tables.Column(verbose_name=u'宕机')
    node_status3_count  = tables.Column(verbose_name=u'不可达')
    node_status4_count  = tables.Column(verbose_name=u'未监控')

    class Meta():
        model = Node
