#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango.ui import tables
from tango.models import Setting, DictCode

from nodes.models import NodeHost
from nodes import constants
from .models import OperationLog, SecurityLog, SubSystem

class SettingTable(tables.Table):
    helpdoc = u'查看系统设置参数'
    
    edit   = tables.Action(name=u'编辑', endpoint='system.setting_edit')
    
    check = tables.CheckBoxColumn()

    name  = tables.Column(verbose_name=u'参数名', orderable=True)
    value = tables.Column(verbose_name=u'参数值', orderable=True)
    unit  = tables.Column(verbose_name=u'参数单位', orderable=True)
    alias = tables.Column(verbose_name=u'说明')

    class Meta():
        model = Setting


class DictCodeTable(tables.Table):
    helpdoc = u'查看系统字典'
    
    edit   = tables.Action(name=u'编辑', endpoint='system.dict_codes_edit')
    
    check = tables.CheckBoxColumn()

    type_label = tables.Column(verbose_name=u'字典类型', orderable=True, accessor='type.type_label')
    code_label = tables.Column(verbose_name=u'字典值', orderable=True)
    is_valid   = tables.EnumColumn('is_valid', verbose_name=u'是否有效', enums={0: u'否', 1: u'是'})

    class Meta():
        model = DictCode
    
class OperationLogTable(tables.Table):

    helpdoc     = u'查看系统操作日志'

    uid            = tables.Column(verbose_name=u'用户名', orderable=True, accessor='user.username',
                                   attrs=tables.Attrs(th={'width': '10%'}))
    terminal_ip    = tables.Column(verbose_name=u'终端IP', orderable=True,
                                   attrs=tables.Attrs(th={'width': '15%'}))
    action         = tables.Column(verbose_name=u'操作', orderable=True,
                                   attrs=tables.Attrs(th={'width': '15%'}))
    oper_obj       = tables.Column(verbose_name=u'操作对象', orderable=True)
    created_at     = tables.DateTimeColumn(verbose_name=u'时间', orderable=True,format='%Y-%m-%d %H:%M:%S')

    class Meta():
        model = OperationLog
        order_by = '-created_at'
        

class SecurityLogTable(tables.Table):
    helpdoc     = u'查看系统安全日志'

    uid            = tables.Column(verbose_name=u'用户名', orderable=True, accessor='user.username')
    terminal_ip    = tables.Column(verbose_name=u'终端IP', orderable=True)
    summary        = tables.Column(verbose_name=u'操作', orderable=True)
    time           = tables.Column(verbose_name=u'时间')

    class Meta():
        model = SecurityLog
        order_by = '-id'


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

        
class SubSystemTable(tables.Table):
    helpdoc = u'查看所有子系统 (子采集)'

    name       = tables.Column(verbose_name=u'名称', orderable=True)
    alias      = tables.Column(verbose_name=u'显示名')
    host       = tables.Column(verbose_name=u'主机名', orderable=True)
    status     = tables.EnumColumn('state', verbose_name=u'状态', enums=constants.STATUS, orderable=True)
    descr      = tables.Column(verbose_name=u'描述')
    started_at = tables.DateTimeColumn(verbose_name=u'开始采集时间', orderable=True)

    class Meta():
        model = SubSystem
    
