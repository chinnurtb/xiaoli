#!/usr/bin/env python  
#coding=utf-8

from tango.ui import tables
from .models import Miboid, Threshold

class ThresholdTable(tables.Table):
    helpdoc = u'阀值设置'
    edit    = tables.Action(name=u'编辑', endpoint='perf.thresholds_edit')
    check   = tables.CheckBoxColumn()

    alias         = tables.Column(verbose_name=u'阀值')
    summary       = tables.Column(verbose_name=u'描述')
    category_name = tables.Column(verbose_name=u'设备', accessor='category.name')
    
    occur_cond1   = tables.Column(verbose_name=u'一级产生条件')
    restore_cond1 = tables.Column(verbose_name=u'一级恢复条件')
    severity1     = tables.Column(verbose_name=u'一级告警条件')
    
    occur_cond2   = tables.Column(verbose_name=u'二级产生条件')
    restore_cond2 = tables.Column(verbose_name=u'二级恢复条件')
    severity2     = tables.Column(verbose_name=u'二级告警条件')
    enabled       = tables.EnumColumn('is_valid', verbose_name=u'是否有效', enums={0: u'否', 1: u'是'})

    class Meta():
        model = Threshold
    

class MiboidTable(tables.Table):
    grp      = tables.Column(verbose_name=u'分组')
    name     = tables.Column(verbose_name=u'名称')
    oid      = tables.Column(verbose_name=u'oid')
    alias    = tables.Column(verbose_name=u'显示名')
    is_valid = tables.EnumColumn('is_valid', verbose_name=u'是否有效', enums={0: u'否', 1: u'是'})
    remark   = tables.Column(verbose_name=u'备注')

    class Meta():
        model = Miboid
        group_by = 'grp'
