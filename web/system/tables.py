#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango.ui import tables

from .models import OperationLog

class OperationLogTable(tables.Table):
    uid            = tables.Column(verbose_name=u'用户名', orderable=True, accessor='user.username')
    terminal_ip    = tables.Column(verbose_name=u'终端IP', orderable=True)
    action         = tables.Column(verbose_name=u'操作', orderable=True)
    oper_obj       = tables.Column(verbose_name=u'操作对象', orderable=True)
    created_at     = tables.DateTimeColumn(verbose_name=u'时间', orderable=True,format='%Y-%m-%d %H:%M:%S')

    class Meta():
        model = OperationLog
        order_by = '-created_at'
