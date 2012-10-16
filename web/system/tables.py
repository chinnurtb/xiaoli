#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango.ui import tables
from tango.models import Setting

from .models import OperationLog, SecurityLog

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
    
class OperationLogTable(tables.Table):

    helpdoc     = u'查看系统操作日志'

    uid            = tables.Column(verbose_name=u'用户名', orderable=True, accessor='user.username')
    terminal_ip    = tables.Column(verbose_name=u'终端IP', orderable=True)
    action         = tables.Column(verbose_name=u'操作', orderable=True)
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

