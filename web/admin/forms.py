#coding=utf-8

from tango.models import db, Category
from nodes.models import Vendor, Model
from .models import Miboid, Module

from flask_wtf import Form, TextField, PasswordField, HiddenField, SelectField, IntegerField, \
    QuerySelectField, TextAreaField, widgets, ValidationError, required, equal_to, email

    
class SearchForm(Form):
    keyword = TextField()
    

class CategoryForm(Form):
    id       = TextField(validators=[required(message=u'必填')])
    obj      = TextField(u'分组', [required(message=u'必填')])
    name     = TextField(u'名称', [required(message=u'必填')])
    alias    = TextField(u'显示名', [required(message=u'必填')])
    is_valid = SelectField(u'有效性', [required(message=u'必填')], choices=[(u'0', u'无效'),(u'1', u'有效')])

    
class PermissionForm(Form):
    endpoint           = TextField(u'Endpoint')
    module_text        = TextField(u'模块显示名')
    name               = TextField(u'子模块显示名')
    operation          = TextField(u'操作名')
    default_permission = SelectField(u'有效性', [required(message=u'必填')], choices=[(u'0', u'无权限'),(u'1', u'有权限')])
    next               = HiddenField()    

    
class VendorForm(Form):
    name     = TextField(u'名称', [required(message=u'必填')])
    alias    = TextField(u'显示名', [required(message=u'必填')])
    url      = TextField(u'厂商主页')
    is_valid = SelectField(u'有效性', [required(message=u'必填')], choices=[(u'0', u'无效'),(u'1', u'有效')])

    
class ModelForm(Form):
    category = QuerySelectField(u'类别', get_label=u'alias',
                                query_factory=lambda: Category.query.filter_by(obj='node'))
    name     = TextField(u'名称', [required(message=u'必填')])
    alias    = TextField(u'显示名', [required(message=u'必填')])
    sysoid   = TextField(u'Sysoid')
    vendor   = QuerySelectField(u'厂商', get_label=u'alias',
                                query_factory=lambda: Vendor.query)
    is_valid = SelectField(u'有效性', [required(message=u'必填')], choices=[(u'0', u'无效'),(u'1', u'有效')])
    remark   = TextAreaField(u'备注')

    
class SysoidForm(Form):
    sysoid = TextField(u'SysOid', [required(message=u'必填')])
    model  = QuerySelectField(u'设备型号', get_label=u'alias',
                              query_factory=lambda:Model.query)
    disco  = TextField(u'发现模块')
    mib      = QuerySelectField(u'Mib文件', get_pk=lambda x: x, get_label=lambda x: x,
                                query_factory=lambda: [m[0] for m in db.session.query(Miboid.mib).distinct().all()])
    remark = TextAreaField(u'备注')
    
    
class ModuleForm(Form):
    name    = TextField(u'名称', [required(message=u'必填')])
    alias   = TextField(u'显示名', [required(message=u'必填')])
    period  = IntegerField(u'周期(min)')
    retries = IntegerField(u'重试次数(次)')
    timeout = IntegerField(u'超时(s)')
    remark  = TextAreaField(u'备注')

    
class  MonitorForm(Form):
    category = TextField(u'分类')
    vendor   = TextField(u'供应商')
    sysoid   = TextField(u'Sysoid')
    match    = TextField(u'匹配规则')
    module   = QuerySelectField(u'采集模块', get_label=u'alias',
                                query_factory=lambda:Module.query) 
    mib      = QuerySelectField(u'Mib文件', get_pk=lambda x: x, get_label=lambda x: x,
                                query_factory=lambda: [m[0] for m in db.session.query(Miboid.mib).distinct().all()])
    remark   = TextAreaField(u'备注')


class MiboidForm(Form):
    mib      = TextField(u'mib', [required(message=u'必填')])
    grp      = TextField(u'分组', [required(message=u'必填')])
    name     = TextField(u'名称', [required(message=u'必填')])
    alias    = TextField(u'显示名', [required(message=u'必填')])
    oid      = TextField(u'oid')
    is_valid = SelectField(u'有效性', [required(message=u'必填')], choices=[(u'0', u'无效'),(u'1', u'有效')])
    remark   = TextAreaField(u'备注')

    
