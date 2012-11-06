# coding: utf-8

from tango.ui import tables as t

from tango.models import Category

from .models import Module, Monitor, Miboid

from nodes.models import Vendor, Model, SysOid

class CategoryTable(t.Table):

    helpdoc = u"分类表: 管理设备分类表"

    edit   = t.Action(name=u'编辑', endpoint='admin.categories_edit')
    delete = t.Action(name=u'删除', endpoint='admin.categories_delete', modalable=True)
    check  = t.CheckBoxColumn()

    id      = t.Column(u'ID', orderable=True)
    obj     = t.Column(u'对象', orderable=True)
    name    = t.Column(u'分类名', orderable=True)
    alias   = t.Column(u'分类别名')
    is_valid= t.Column(u'是否有效')
    
    class Meta():
        model = Category
        order_by = 'id'

class VendorTable(t.Table):
    helpdoc = u'厂商表'
    
    edit   = t.Action(name=u'编辑', endpoint='admin.vendors_edit')
    delete = t.Action(name=u'删除', endpoint='admin.vendors_delete', modalable=True)
    check  = t.CheckBoxColumn()

    id      = t.Column(u'ID', orderable=True)
    name    = t.Column(u'供应商名', orderable=True)
    alias   = t.Column(u'供应商别名', orderable=True)
    url     = t.Column(u'供应商URL')
    is_valid= t.Column(u'是否有效')

    class Meta():
        model = Vendor
        order_by = 'id'
    
class ModelTable(t.Table):
    helpdoc = u"型号表: 管理维护设备型号"

    edit   = t.Action(name=u'编辑', endpoint='admin.models_edit')
    delete = t.Action(name=u'删除', endpoint='admin.models_delete', modalable=True)
    check  = t.CheckBoxColumn()
    
    id          = t.Column(u'ID', orderable=True)
    category    = t.Column(u'设备分类', accessor='category.alias', orderable=True)   
    vendor      = t.Column(u'供应商', accessor='vendor.alias', orderable=True)
    name        = t.Column(u'名称', orderable=True)
    alias       = t.Column(u'别名')
    sysoid      = t.Column(u'SysOid')
    is_valid    = t.Column(u'是否有效')
    remark      = t.Column(u'备注')

    class Meta():
        model = Model
        order_by = 'id'
    

class ModuleTable(t.Table):
    helpdoc = u"采集模块表: 管理维护采集模块"

    edit   = t.Action(name=u'编辑', endpoint='admin.modules_edit')
    delete = t.Action(name=u'删除', endpoint='admin.modules_delete', modalable=True)
    check  = t.CheckBoxColumn()
    
    id      = t.Column(u'ID', orderable=True)
    name    = t.Column(u'模块名', orderable=True)
    alias   = t.Column(u'模块别名')
    period  = t.Column(u'采集周期(分)')
    retries = t.Column(u'重试次数')
    timeout = t.Column(u'超时时间(秒)')
    remark  = t.Column(u'备注')

    class Meta():
        model = Module
        order_by = 'id'

class SysOidTable(t.Table):
    helpdoc = u'SysOid表: 管理设备系统OID'

    edit   = t.Action(name=u'编辑', endpoint='admin.sysoids_edit')
    delete = t.Action(name=u'删除', endpoint='admin.sysoids_delete', modalable=True)
    check  = t.CheckBoxColumn()

    sysoid  = t.Column(u'OID', orderable=True)
    model   = t.Column(u'设备型号', accessor='model.alias', orderable=True)
    disco   = t.Column(u'发现模块')
    mib     = t.Column(u'MIB', orderable=True)
    remark  = t.Column(u'备注')

    class Meta():
        model = SysOid 
        order_by = 'id'
    
class MonitorTable(t.Table):
    helpdoc = u'监控器表: 管理配置监控器'

    edit   = t.Action(name=u'编辑', endpoint='admin.monitors_edit')
    delete = t.Action(name=u'删除', endpoint='admin.monitors_delete', modalable=True)
    check  = t.CheckBoxColumn()

    category    = t.Column(u'设备分类', orderable=True)
    vendor      = t.Column(u'设备供应商', orderable=True)
    sysoid      = t.Column(u'设备sysoid', orderable=True)
    match       = t.Column(u'匹配规则')
    modid       = t.Column(u'采集模块', accessor='module.name', orderable=True)
    mib         = t.Column(u'MIB')
    remark      = t.Column(u'备注')

    class Meta():
        model = Monitor
        order_by = 'id'
    

class MiboidTable(t.Table):
    helpdoc = u'Mib 表'

    edit   = t.Action(name=u'编辑', endpoint='admin.miboids_edit')
    delete = t.Action(name=u'删除', endpoint='admin.miboids_delete', modalable=True)
    check  = t.CheckBoxColumn()
    
    grp      = t.Column(u'分组')
    name     = t.Column(u'名称')
    oid      = t.Column(u'oid')
    alias    = t.Column(u'显示名')
    is_valid = t.EnumColumn(u'是否有效', 'is_valid', enums={0: u'否', 1: u'是'})
    remark   = t.Column(u'备注')

    class Meta():
        model = Miboid
        group_by = 'grp'
