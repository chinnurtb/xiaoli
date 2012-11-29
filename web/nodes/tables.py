#!/usr/bin/env python  
# coding: utf-8
from jinja2 import Markup
from flask import url_for

from tango.ui import tables
from tango.ui.tables.utils import Attrs

from nodes import constants
from .models import Node, NodeSwitch, NodeRouter, NodeOlt, NodeOnu, NodeEoc, NodeCpe, Port, NodeHost, NODE_STATUS_DICT,Area

redirct_dict = {
    1:"routers",
    2:"switches",
    3:"firewalls",
    10:"brases",
    20:"olts",
    21:"onus",
    30:"dslams",
    40:"acs",
    41:"fataps",
    42:"fitaps",
    50:"eocs",
    51:"cpes",
    90:"hosts",
}

def redirect_node_show(record):
    return u'/nodes/%s/%s/' % (redirct_dict.get(record.category_id), record.id)

def redirect_node_edit(record):
    return u'/nodes/%s/edit/%s/' % (redirct_dict.get(record.category_id), record.id)

class StatusColumn(tables.EnumColumn):

    def __init__(self, attrs=None, **extra):
        super(StatusColumn, self).__init__(u'状态', name='status', enums=NODE_STATUS_DICT,orderable=True)

    def render(self, value, record, bound_column):
        text = super(StatusColumn, self).render(value, record, bound_column)
        return Markup('<span class="label status-%s">%s</span>' % (value, text))

class NodeTable(tables.Table):
    edit = tables.Action(name=u'编辑',url=lambda record: redirect_node_edit(record))
    delete = tables.Action(name=u'删除', endpoint='nodes.nodes_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = StatusColumn()
    name = tables.LinkColumn(u'名称',orderable=True)
    alias = tables.Column(u'别名',orderable=True)
    category = tables.Column(u'节点类型',orderable=True, accessor='category.alias')
    vendor_name = tables.Column(u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(u'型号', orderable=True, accessor='model.alias')
    addr = tables.Column(u'IP', orderable=True)
    area_name = tables.Column(u'所属区域', accessor='area.full_name')
    last_check = tables.Column(u'上次同步',orderable=True)
    summary = tables.Column(u'状态信息')

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
    status = StatusColumn()
    name = tables.LinkColumn(u'名称', endpoint='nodes.olts_show', orderable=True)
    alias = tables.Column(u'别名',orderable=True)
    vendor_name = tables.Column(u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(u'型号', orderable=True, accessor='model.alias')
    addr = tables.Column(u'IP', orderable=True)
    area_name = tables.Column(u'所属区域', accessor='area.full_name')
    last_check = tables.Column(u'上次同步',orderable=True)
    onu_count_plan = tables.Column(u'ONU数量')
    onu_count_unplan = tables.Column(u'未规划ONU')

    class Meta():
        model = NodeOlt
        per_page = 30

class EocTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.eocs_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.eocs_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = StatusColumn()
    name = tables.LinkColumn(u'名称', endpoint='nodes.eocs_show', orderable=True)
    alias = tables.Column(u'别名',orderable=True)
    vendor_name = tables.Column(u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(u'型号', orderable=True, accessor='model.alias')
    addr = tables.Column(u'IP', orderable=True)
    area_name = tables.Column(u'所属区域', accessor='area.full_name')
    onu_name = tables.Column(u'所属ONU', accessor='onu.name')
    last_check = tables.Column(u'上次同步',orderable=True)
    cpe_count_plan = tables.Column(u'CPE终端数量')
    cpe_count_unplan = tables.Column(u'未规划CPE终端')

    class Meta():
        model = NodeEoc
        per_page = 30
        url_makers = {
            'onu_name': lambda record: url_for('nodes.onus_show',id=record.olt.id),
            }

class CpeTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.cpes_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.cpes_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = StatusColumn()
    name = tables.LinkColumn(endpoint='nodes.cpes_show',verbose_name=u'名称',orderable=True)
    alias = tables.Column(verbose_name=u'别名',orderable=True)
    vendor_name = tables.Column(verbose_name=u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(verbose_name=u'型号', orderable=True, accessor='model.alias')
    mac = tables.Column(verbose_name=u'Mac地址', orderable=True)
    area_name = tables.Column(verbose_name=u'所属区域', accessor='area.full_name')
    eoc_name = tables.LinkColumn(verbose_name=u'所属EOC', accessor='eoc.name')
    last_check = tables.Column(verbose_name=u'上次同步',orderable=True)

    class Meta():
        model = NodeCpe
        per_page = 30
        url_makers = {
            'eoc_name': lambda record: url_for('nodes.eocs_show',id=record.eoc.id),
            }

class SwitchTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.switches_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.switches_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = StatusColumn()
    name = tables.LinkColumn(u'名称', endpoint='nodes.switches_show', orderable=True)
    addr = tables.Column(u'IP', orderable=True)
    area_name = tables.Column(u'所属区域', accessor='area.full_name')
    vendor_name = tables.Column(u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(u'型号', orderable=True, accessor='model.alias')
    last_check = tables.Column(u'上次同步',orderable=True)
    location = tables.Column(u'位置')

    class Meta():
        model = NodeSwitch
        per_page = 30

class RouterTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.routers_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.routers_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = StatusColumn()
    name = tables.LinkColumn(u'名称', endpoint='nodes.routers_show', orderable=True)
    addr = tables.Column(u'IP', orderable=True)
    area_name = tables.Column(u'所属区域', accessor='area.full_name')
    vendor_name = tables.Column(u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(u'型号', orderable=True, accessor='model.alias')
    last_check = tables.Column(u'上次同步',orderable=True)
    location = tables.Column(u'位置')

    class Meta():
        model = NodeRouter
        per_page = 30

class OnuTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.onus_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.onus_delete',attrs=Attrs(a={"class": "delete"}))
    check = tables.CheckBoxColumn()
    status = StatusColumn()
    name = tables.LinkColumn(u'名称', endpoint='nodes.onus_show', orderable=True)
    addr = tables.Column(u'IP', orderable=True)
    area_name = tables.Column(u'所属区域', accessor='area.full_name')
    vendor_name = tables.Column(u'厂商', orderable=True, accessor='vendor.alias')
    model_name = tables.Column(u'型号', orderable=True, accessor='model.alias')
    mac = tables.Column(u'认证标识',orderable=True)
    olt_border_no = tables.Column(u'OLT板位')
    olt_name = tables.LinkColumn(u'所属OLT', accessor='olt.name')

    class Meta():
        model = NodeOnu
        per_page = 30
        url_makers = {
            'olt_name': lambda record: url_for('nodes.olts_show',id=record.olt.id),
            }

class NodeHostTable(tables.Table):
    helpdoc = u'查看所有主机'
    edit    = tables.Action(name=u'编辑', endpoint='system.hosts_edit')
    check   = tables.CheckBoxColumn()

    name       = tables.Column(u'名称')
    alias      = tables.Column(u'显示名')
    status     = tables.EnumColumn(u'状态', 'state', enums=constants.STATUS, orderable=True)
    ifaces     = tables.Column(u'接口地址')
    cpu_info   = tables.Column(u'CPU 信息')
    mem_info   = tables.Column(u'内存 信息')
    disk_info  = tables.Column(u'磁盘 信息')
    worker_num = tables.Column(u'采集进程数')
    os_type    = tables.Column(u'操作系统')
    updated_at = tables.DateTimeColumn(u'更新时间')

    class Meta():
        model = NodeHost

class CityTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.cities_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.cities_delete',attrs=Attrs(a={"class": "delete"}))
    check   = tables.CheckBoxColumn()

    name        = tables.Column(u'地市名称', orderable=True)
    town_count      = tables.LinkColumn(u'区县')
    branch_count     = tables.LinkColumn(u'分局')
    entrance_count     = tables.LinkColumn(u'接入点')
    total_count     = tables.LinkColumn(u'节点')
    router_count     = tables.LinkColumn(u'路由器')
    switch_count     = tables.LinkColumn(u'交换机')
    firewall_count     = tables.LinkColumn(u'防火墙')
    bras_count     = tables.LinkColumn(u'BRAS')
    olt_count     = tables.LinkColumn(u'OLT')
    onu_count     = tables.LinkColumn(u'ONU')
    ac_count     = tables.LinkColumn(u'AC')
    fatap_count     = tables.LinkColumn(u'FatAP')
    fitap_count     = tables.LinkColumn(u'FitAP')
    dslam_count     = tables.LinkColumn(u'DSLAM')
    eoc_count     = tables.LinkColumn(u'EOC')
    cpe_count     = tables.LinkColumn(u'CPE')

    class Meta():
        model = Area
        url_makers = {
            'town_count': lambda record: url_for('nodes.towns', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'branch_count': lambda record: url_for('nodes.branches', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'entrance_count': lambda record: url_for('nodes.entrances', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'total_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'router_count': lambda record: url_for('nodes.routers', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'switch_count': lambda record: url_for('nodes.switches', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'firewall_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'bras_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'olt_count': lambda record: url_for('nodes.olts', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'onu_count': lambda record: url_for('nodes.onus', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'ac_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'fatap_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'fitap_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'dslam_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'eoc_count': lambda record: url_for('nodes.eocs', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            'cpe_count':lambda record: url_for('nodes.cpes', area=record.name, area_netloc='areas.cityid='+str(record.id), area_selected=record.id),
            }

class TownTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.towns_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.towns_delete',attrs=Attrs(a={"class": "delete"}))
    check   = tables.CheckBoxColumn()

    name        = tables.Column(u'区县名称', orderable=True)
    branch_count     = tables.LinkColumn(u'分局')
    entrance_count     = tables.LinkColumn(u'接入点')
    total_count     = tables.LinkColumn(u'节点')
    router_count     = tables.LinkColumn(u'路由器')
    switch_count     = tables.LinkColumn(u'交换机')
    firewall_count     = tables.LinkColumn(u'防火墙')
    bras_count     = tables.LinkColumn(u'BRAS')
    olt_count     = tables.LinkColumn(u'OLT')
    onu_count     = tables.LinkColumn(u'ONU')
    ac_count     = tables.LinkColumn(u'AC')
    fatap_count     = tables.LinkColumn(u'FatAP')
    fitap_count     = tables.LinkColumn(u'FitAP')
    dslam_count     = tables.LinkColumn(u'DSLAM')
    eoc_count     = tables.LinkColumn(u'EOC')
    cpe_count     = tables.LinkColumn(u'CPE')

    class Meta():
        model = Area
        url_makers = {
            'branch_count': lambda record: url_for('nodes.branches', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'entrance_count': lambda record: url_for('nodes.entrances', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'total_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'router_count': lambda record: url_for('nodes.routers', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'switch_count': lambda record: url_for('nodes.switches', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'firewall_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'bras_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'olt_count': lambda record: url_for('nodes.olts', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'onu_count': lambda record: url_for('nodes.onus', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'ac_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'fatap_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'fitap_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'dslam_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'eoc_count': lambda record: url_for('nodes.eocs', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            'cpe_count':lambda record: url_for('nodes.cpes', area=record.name, area_netloc='areas.town='+str(record.id), area_selected=record.id),
            }

class BranchTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.branches_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.branches_delete',attrs=Attrs(a={"class": "delete"}))
    check   = tables.CheckBoxColumn()

    name        = tables.Column(u'分局名称', orderable=True)
    entrance_count     = tables.LinkColumn(u'接入点')
    total_count     = tables.LinkColumn(u'节点')
    router_count     = tables.LinkColumn(u'路由器')
    switch_count     = tables.LinkColumn(u'交换机')
    firewall_count     = tables.LinkColumn(u'防火墙')
    bras_count     = tables.LinkColumn(u'BRAS')
    olt_count     = tables.LinkColumn(u'OLT')
    onu_count     = tables.LinkColumn(u'ONU')
    ac_count     = tables.LinkColumn(u'AC')
    fatap_count     = tables.LinkColumn(u'FatAP')
    fitap_count     = tables.LinkColumn(u'FitAP')
    dslam_count     = tables.LinkColumn(u'DSLAM')
    eoc_count     = tables.LinkColumn(u'EOC')
    cpe_count     = tables.LinkColumn(u'CPE')

    class Meta():
        model = Area
        url_makers = {
            'entrance_count': lambda record: url_for('nodes.entrances', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'total_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'router_count': lambda record: url_for('nodes.routers', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'switch_count': lambda record: url_for('nodes.switches', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'firewall_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'bras_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'olt_count': lambda record: url_for('nodes.olts', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'onu_count': lambda record: url_for('nodes.onus', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'ac_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'fatap_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'fitap_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'dslam_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'eoc_count': lambda record: url_for('nodes.eocs', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            'cpe_count':lambda record: url_for('nodes.cpes', area=record.name, area_netloc='areas.branch='+str(record.id), area_selected=record.id),
            }

class EntranceTable(tables.Table):
    edit = tables.Action(name=u'编辑', endpoint='nodes.entrances_edit')
    delete = tables.Action(name=u'删除', endpoint='nodes.entrances_delete',attrs=Attrs(a={"class": "delete"}))
    check   = tables.CheckBoxColumn()

    name        = tables.Column(u'接入点名称', orderable=True)
    total_count     = tables.LinkColumn(u'节点')
    router_count     = tables.LinkColumn(u'路由器')
    switch_count     = tables.LinkColumn(u'交换机')
    firewall_count     = tables.LinkColumn(u'防火墙')
    bras_count     = tables.LinkColumn(u'BRAS')
    onu_count     = tables.LinkColumn(u'ONU')
    ac_count     = tables.LinkColumn(u'AC')
    fatap_count     = tables.LinkColumn(u'FatAP')
    fitap_count     = tables.LinkColumn(u'FitAP')
    dslam_count     = tables.LinkColumn(u'DSLAM')
    cpe_count     = tables.LinkColumn(u'CPE')

    class Meta():
        model = Area
        url_makers = {
            'total_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'router_count': lambda record: url_for('nodes.routers', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'switch_count': lambda record: url_for('nodes.switches', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'firewall_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'bras_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'onu_count': lambda record: url_for('nodes.onus', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'ac_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'fatap_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'fitap_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'dslam_count': lambda record: url_for('nodes.nodes', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            'cpe_count':lambda record: url_for('nodes.cpes', area=record.name, area_netloc='areas.entrance='+str(record.id), area_selected=record.id),
            }

class AreaStatisticsTable(tables.Table):
    name        = tables.Column(u'区域')
    total_count     = tables.Column(u'节点')
    router_count     = tables.Column(u'路由器')
    switch_count     = tables.Column(u'交换机')
    firewall_count     = tables.Column(u'防火墙')
    bras_count     = tables.Column(u'BRAS')
    olt_count     = tables.Column(u'OLT')
    onu_count     = tables.Column(u'ONU')
    ac_count     = tables.Column(u'AC')
    fatap_count     = tables.Column(u'FatAP')
    fitap_count     = tables.Column(u'FitAP')
    dslam_count     = tables.Column(u'DSLAM')
    eoc_count     = tables.Column(u'EOC')
    cpe_count     = tables.Column(u'CPE')

    class Meta():
        model = Node

class CategoryTable(tables.Table):
    category_name   = tables.EnumColumn(u'类型',name='category',
        enums={1:u'OLT',2:u'ONU',3:u'DSLAM',4:u'EOC',5:u'Switch'}, orderable=True)
    total_count  = tables.Column(u'数量')
    status1_count  = tables.Column(u'正常')
    status2_count  = tables.Column(u'宕机')
    status3_count  = tables.Column(u'不可达')
    status4_count  = tables.Column(u'未监控')

    class Meta():
        model = Node

class VendorTable(tables.Table):
    alias       = tables.Column(u'名称', orderable=True)
    node_count  = tables.Column(u'数量')
    node_status1_count  = tables.Column(u'正常')
    node_status2_count  = tables.Column(u'宕机')
    node_status3_count  = tables.Column(u'不可达')
    node_status4_count  = tables.Column(u'未监控')

    class Meta():
        model = Node
