# coding: utf-8

from tango.ui import tables as t

from tango.models import Category

from .models import Module, Monitor

from nodes.models import Vendor, Model, SysOid

class CategoryTable(t.Table):

    helpdoc = u"分类表: 管理设备分类表"

    id      = t.Column(verbose_name=u'ID', orderable=True)
    obj     = t.Column(verbose_name=u'对象', orderable=True)
    name    = t.Column(verbose_name=u'分类名', orderable=True)
    alias   = t.Column(verbose_name=u'分类别名')
    is_valid= t.Column(verbose_name=u'是否有效')
    
    class Meta():
        model = Category
        order_by = 'id'

class VendorTable(t.Table):

    id      = t.Column(verbose_name=u'ID', orderable=True)
    name    = t.Column(verbose_name=u'供应商名', orderable=True)
    alias   = t.Column(verbose_name=u'供应商别名', orderable=True)
    url     = t.Column(verbose_name=u'供应商URL')
    is_valid= t.Column(verbose_name=u'是否有效')

    class Meta():
        model = Vendor
        order_by = 'id'
    
class ModelTable(t.Table):

    helpdoc = u"型号表: 管理维护设备型号"
    
    id          = t.Column(verbose_name=u'ID', orderable=True)
    category    = t.Column(verbose_name=u'设备分类', accessor='category.alias', orderable=True)   
    vendor      = t.Column(verbose_name=u'供应商', accessor='vendor.alias', orderable=True)
    name        = t.Column(verbose_name=u'名称', orderable=True)
    alias       = t.Column(verbose_name=u'别名')
    sysoid      = t.Column(verbose_name=u'SysOid')
    is_valid    = t.Column(verbose_name=u'是否有效')
    remark      = t.Column(verbose_name=u'备注')

    class Meta():
        model = Model
        order_by = 'id'
    

class ModuleTable(t.Table):
    
    helpdoc = u"采集模块表: 管理维护采集模块"

    id      = t.Column(verbose_name=u'ID', orderable=True)
    name    = t.Column(verbose_name=u'模块名', orderable=True)
    alias   = t.Column(verbose_name=u'模块别名')
    period  = t.Column(verbose_name=u'采集周期(分)')
    retries = t.Column(verbose_name=u'重试次数')
    timeout = t.Column(verbose_name=u'超时时间(秒)')
    remark  = t.Column(verbose_name=u'备注')

    class Meta():
        model = Module
        order_by = 'id'

class SysOidTable(t.Table):
    
    helpdoc = u'SysOid表: 管理设备系统OID'

    sysoid  = t.Column(verbose_name=u'OID', orderable=True)
    model   = t.Column(verbose_name=u'设备型号', accessor='model.alias', orderable=True)
    disco   = t.Column(verbose_name=u'发现模块')
    mib     = t.Column(verbose_name=u'MIB', orderable=True)
    remark  = t.Column(verbose_name=u'备注')

    class Meta():
        model = SysOid 
        order_by = 'id'
    
class MonitorTable(t.Table):

    helpdoc = u'监控器表: 管理配置监控器'

    category    = t.Column(verbose_name=u'设备分类', orderable=True)
    vendor      = t.Column(verbose_name=u'设备供应商', orderable=True)
    sysoid      = t.Column(verbose_name=u'设备sysoid', orderable=True)
    match       = t.Column(verbose_name=u'匹配规则')
    modid       = t.Column(verbose_name=u'采集模块', accessor='module.name', orderable=True)
    mib         = t.Column(verbose_name=u'MIB')
    remark      = t.Column(verbose_name=u'备注')

    class Meta():
        model = Monitor
        order_by = 'id'
    
