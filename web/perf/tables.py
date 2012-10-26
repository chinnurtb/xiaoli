#!/usr/bin/env python  
#coding=utf-8

from tango.ui import tables
from .models import Threshold, Metric

class ThresholdTable(tables.Table):
    helpdoc = u'阀值设置'
    edit    = tables.Action(name=u'编辑', endpoint='perf.thresholds_edit')
    check   = tables.CheckBoxColumn()

    alias         = tables.Column(verbose_name=u'阀值')
    summary       = tables.Column(verbose_name=u'摘要')
    category_name = tables.Column(verbose_name=u'设备', accessor='category.alias')
    
    occur_cond1   = tables.Column(verbose_name=u'一级产生条件')
    restore_cond1 = tables.Column(verbose_name=u'一级恢复条件')
    severity1     = tables.Column(verbose_name=u'一级告警条件')
    
    occur_cond2   = tables.Column(verbose_name=u'二级产生条件')
    restore_cond2 = tables.Column(verbose_name=u'二级恢复条件')
    severity2     = tables.Column(verbose_name=u'二级告警条件')
    enabled       = tables.EnumColumn('is_valid', verbose_name=u'是否有效', enums={0: u'否', 1: u'是'})

    class Meta():
        model = Threshold


class MetricTable(tables.Table):
    helpdoc = u'指标管理'
    
    grp    = tables.Column(verbose_name=u'分组')
    name   = tables.Column(verbose_name=u'名称')
    alias  = tables.Column(verbose_name=u'显示名')
    calc   = tables.Column(verbose_name=u'计算')
    unit   = tables.Column(verbose_name=u'单位')
    format = tables.Column(verbose_name=u'格式')
    descr  = tables.Column(verbose_name=u'说明')
    
    class Meta():
        model = Metric
