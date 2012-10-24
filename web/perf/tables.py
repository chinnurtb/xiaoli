#!/usr/bin/env python  
#coding=utf-8


from tango.ui import tables
from .models import Miboid

class MiboidTable(tables.Table):
    grp = tables.Column(verbose_name=u'分组')
    name = tables.Column(verbose_name=u'名称')
    oid = tables.Column(verbose_name=u'oid')
    alias = tables.Column(verbose_name=u'显示名')
    remark = tables.Column(verbose_name=u'备注')

    class Meta():
        model = Miboid
        group_by = 'grp'
