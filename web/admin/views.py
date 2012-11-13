# coding: utf-8

from sqlalchemy import func
from flask import Blueprint, request, url_for, redirect, render_template, flash, current_app

from tango import db
from tango.ui.tables import make_table

from tango.models import Category
from nodes.models import Vendor, SysOid, Model
from users.models import Permission

from .models import Module, Monitor, Miboid
from .forms import SearchForm, CategoryForm, PermissionForm, VendorForm, ModelForm,\
    SysoidForm, ModuleForm, MonitorForm, MiboidForm
from .tables import CategoryTable, PermissionTable, VendorTable, ModuleTable, \
    ModelTable, SysOidTable, MonitorTable, MiboidTable

adminview = Blueprint('admin', __name__, url_prefix='/admin')

@adminview.context_processor
def jnject_mibs():
    return dict(mibs=[(m[0], m[0], url_for('admin.miboids', mib=m[0])) for m
                      in db.session.query(Miboid.mib).order_by(Miboid.mib.asc()).distinct().all()])

@adminview.route('/')
def index():
    return render_template('admin/index.html')

# ==============================================================================
#  分类
# ==============================================================================    
@adminview.route('/categories/')
def categories():
    form = SearchForm(formdata=request.args)
    cls, table_cls = Category, CategoryTable
    query = cls.query
    if form.keyword.data:
        ikeyword = '%' + form.keyword.data + '%'
        query = query.filter(db.or_(cls.name.ilike(ikeyword),
                                    cls.alias.ilike(ikeyword)))
    table = make_table(query, table_cls)
    return render_template('admin/categories/index.html',
                           table = table, form=form)

    
@adminview.route('/categories/new', methods=['GET', 'POST'])
def categories_new():
    form = CategoryForm()
    if form.is_submitted and form.validate_on_submit():
        category = Category()
        form.populate_obj(category)
        db.session.add(category)
        db.session.commit()
        flash(u'%s 添加成功' % category.alias, 'success')
        return redirect(url_for('admin.categories'))
        
    kwargs = {
        'title' : u'添加类别',
        'action' : url_for('admin.categories_new'),
        'form' : form,
    }
    return render_template('admin/categories/new-edit.html', **kwargs)

@adminview.route('/categories/edit/<int:id>', methods=['GET', 'POST'])
def categories_edit(id):
    form = CategoryForm()
    category = Category.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(category)
        db.session.commit()
        flash(u'%s 修改成功' % category.alias, 'success')
        return redirect(url_for('admin.categories'))
        
    form.process(obj=category)
    kwargs = {
        'title': u'编辑类别',
        'action': url_for('admin.categories_edit', id=id),
        'form': form
    }
    return render_template('admin/categories/new-edit.html', **kwargs)
    
@adminview.route('/categories/delete/<int:id>', methods=['GET', 'POST'])
def categories_delete(id):
    category = Category.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(category)
        db.session.commit()
        flash(u'%s 已删除' % category.alias, 'success')
        return redirect(url_for('admin.categories'))
        
    kwargs = {
        'title': u'删除类别',
        'action': url_for('admin.categories_delete', id=id),
        'fields': [(u'分组', category.obj), (u'显示名', category.alias)],
        'type' : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)
    
    
@adminview.route('/categories/delete/all', methods=['GET', 'POST'])
def categories_delete_all():
    name, alias, cls = 'categories', u'类别', Category
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            db.session.delete(cls.query.get(int(i)))
        db.session.commit()
        flash(u'成功删除 %d 个%s!' % (len(ids), alias) , 'success')
        return redirect(url_for('admin.%s' % name))

    ids = dict(request.values.lists()).get('id[]', [])
    objs = cls.query.filter(cls.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title': u'批量删除%s' % alias,
        'action': url_for('admin.%s_delete_all' % name),
        'fields': [(obj.id, alias, obj.alias) for obj in objs], # Diff
        'type' : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)


# ==============================================================================
#  权限管理
# ==============================================================================    

MODULE_TEXTS = {
    'tango'  : u'公共',
    'home'   : u'首页',
    'topo'   : u'拓扑管理',
    'nodes'  : u'节点管理',
    'alarms' : u'故障管理',
    'perf'   : u'性能管理',
    'report' : u'报表管理',
    'users'  : u'用户管理',
    'system' : u'系统管理',
    'admin'  : u'后台管理',
}

NAMES = {
    # Admin
    'admin.categories'   : u'类别',
    'admin.miboids'      : u'MIB文件',
    'admin.models'       : u'设备型号',
    'admin.modules'      : u'采集模块',
    'admin.monitors'     : u'监控器',
    'admin.permissions'  : u'权限',
    'admin.sysoids'      : u'Sysoid',
    'admin.vendors'      : u'厂商',
    # Alarms
    'alarms.classes'     : u'告警类型',
    'alarms.histories'   : u'历史告警',
    'alarms.knowledges'  : u'告警知识库',
    'alarms.settings'    : u'',
    # Nodes
    'nodes.categories'   : u'类别',
    'nodes.areas'        : u'区域',
    'nodes.cities'       : u'城市',
    'nodes.branches'     : u'',
    'nodes.towns'        : u'镇',
    'nodes.entrances'    : u'接入点',
    'nodes.managers'     : u'',
    'nodes.eocs'         : u'EOC',
    'nodes.cpes'         : u'CPE',
    'nodes.nodes'        : u'节点',
    'nodes.olts'         : u'OLT',
    'nodes.onus'         : u'ONU',
    'nodes.routers'      : u'路由器',
    'nodes.switches'     : u'交换机',
    'nodes.vendors'      : u'厂商',
    # 性能
    'perf.ping'        : u'Ping延时',
    'perf.cpumen'      : u'CPU/内存',
    'perf.intfusage'   : u'流量/占用率',
    'perf.intftraffic' : u'流量流速',
    'perf.ponusage'    : u'PON口占用率',
    'perf.ponpower'    : u'PON光功率',
    # 系统
    'system.dict_codes'  : u'字典',
    'system.hosts'       : u'服务器',
    'system.metrics'     : u'指标',
    'system.oplogs'      : u'操作日志',
    'system.seclogs'     : u'安全日志',
    'system.settings'    : u'参数设置',
    'system.subsystems'  : u'子系统',
    'system.thresholds'  : u'阀值',
    'system.timeperiods' : u'采集规则',
    # 用户
    'users.domains'      : u'管理域',
    'users.roles'        : u'角色',
    'users.users'        : u'用户'
}
    
OPERATIONS = {
    'new'             : u'新建',
    'edit'            : u'编辑',
    'delete'          : u'删除',
    ('delete', 'all') : u'批量删除',
}

@adminview.route('/permissions/update')
def permissions_update():
    reset = request.args.get('reset', '')
    if reset == 'yes':
        db.session.execute('truncate permissions;')
        db.session.commit()
        
    permissions = {
        'exists' : [],
        'new'    : []
    }
    for rule in current_app.url_map.iter_rules():
        endpoint = rule.endpoint
        if endpoint in current_app.config['SAFE_ENDPOINTS'] \
           or endpoint.find('demo') > -1 or endpoint.find('test') > -1:
            print 'Safe>> ', endpoint
            continue
        if endpoint.find('.') == -1:
            raise ValueError('UnExcepted endpoint: %s' % endpoint)

        p = Permission.query.filter_by(endpoint=endpoint).first()
        if p:
            permissions['exists'].append(p)
            print 'Exists>> ', endpoint
            continue
            
        name = ''
        SPECIAL_NAMES = {
            'index' : u'首页',
            'timeline' : u'TimeLine',
        }
        endpoint_tail = endpoint.split('.')[-1]
        if  endpoint_tail in SPECIAL_NAMES.keys():
            name = SPECIAL_NAMES[endpoint_tail]
        else:
            for k in NAMES.keys():
                if endpoint.find(k) == 0:
                    name = NAMES[k]
        module = endpoint.split('.')[0]
        module_text = MODULE_TEXTS[module]
        pieces = endpoint.split('_')
        operation = ''
        if endpoint in NAMES.keys():
            operation = u'查看'
        elif len(pieces) > 1:
            if pieces[-1] in OPERATIONS:
                operation = OPERATIONS[pieces[-1]]
            elif tuple(pieces[-2:]) in OPERATIONS:
                operation = OPERATIONS[tuple(pieces[-2:])]
                
        p = Permission()
        p.name = name               # 子模块名
        p.module_text = module_text # 模块显示名
        p.operation = operation     # 操作名
        p.module = module
        p.endpoint = endpoint
        db.session.add(p)
        db.session.commit()
        permissions['new'].append(p)
        print 'New>> ', rule, endpoint
    flash(u'权限列表更新成功', 'success')
    
    permissions['new'].sort(cmp=lambda x,y: cmp(x.endpoint, y.endpoint))
    return render_template('admin/permissions/update-feedback.html',
                           permissions=permissions)
    
    
@adminview.route('/permissions/')
def permissions():
    form = SearchForm(formdata=request.args)
    cls, table_cls = Permission, PermissionTable
    query = cls.query
    if form.keyword.data:
        ikeyword = '%' + form.keyword.data + '%'
        query = query.filter(db.or_(cls.endpoint.ilike(ikeyword),
                                    cls.name.ilike(ikeyword),
                                    cls.operation.ilike(ikeyword)))
    table = make_table(query, table_cls)
    return render_template('admin/permissions/index.html',
                           table = table, form=form)
    
    
@adminview.route('/permissions/edit/<int:id>', methods=['GET', 'POST'])
def permissions_edit(id):
    form = PermissionForm()
    permission = Permission.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        permission.module_text = form.module_text.data
        permission.name = form.name.data
        permission.operation = form.operation.data
        permission.default_permission = form.default_permission.data
        db.session.commit()
        flash(u'权限项修改成功!', 'success')
        return redirect(request.referrer)
        
    form.process(obj=permission)
    kwargs = {
        'menuid' : 'permissions',
        'title'  : u'修改权限',
        'action' : url_for('.permissions_edit', id=id),
        'form'   : form
    }
    return render_template('admin/permissions/edit.html', **kwargs)
    

@adminview.route('/permissions/delete/<int:id>', methods=['GET', 'POST'])
def permissions_delete(id):
    permission = Permission.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(permission)
        db.session.commit()
        flash(u'权限项 %s 已删除' % permission.endpoint, 'success')
        return redirect(url_for('.permissions'))
        
    kwargs = {
        'title': u'删除权限项',
        'action': url_for('.permissions_delete', id=id),
        'fields': [(u'Endpoint', permission.endpoint),
                   (u'子模块名', permission.name),
                   (u'操作名', permission.operation)],
        'type' : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)
    
    
# ==============================================================================
#  供应商
# ==============================================================================    
@adminview.route('/vendors/')
def vendors():
    form = SearchForm(formdata=request.args)
    cls, table_cls = Vendor, VendorTable
    query = cls.query
    if form.keyword.data:
        ikeyword = '%' + form.keyword.data + '%'
        query = query.filter(db.or_(cls.name.ilike(ikeyword),
                                    cls.alias.ilike(ikeyword)))
    table = make_table(query, table_cls)
    return render_template('admin/vendors/index.html',
                           table = table, form=form)

    
@adminview.route('/vendors/new', methods=['GET', 'POST'])
def vendors_new():
    form = VendorForm()
    if form.is_submitted and form.validate_on_submit():
        vendor = Vendor()
        form.populate_obj(vendor)
        db.session.add(vendor)
        db.session.commit()
        flash(u'%s 添加成功' % vendor.alias, 'success') # Diff
        return redirect(url_for('admin.vendors'))
        
    kwargs = {
        'title' : u'添加厂商',
        'action' : url_for('admin.vendors_new'),
        'form' : form,
    }
    return render_template('admin/vendors/new-edit.html', **kwargs)
    

@adminview.route('/vendors/edit/<int:id>', methods=['GET', 'POST'])
def vendors_edit(id):
    form = VendorForm()
    vendor = Vendor.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(vendor)
        db.session.commit()
        flash(u'%s 修改成功' % vendor.alias, 'success') # Diff
        return redirect(url_for('admin.vendors'))
        
    form.process(obj=vendor)
    kwargs = {
        'title': u'编辑厂商',
        'action': url_for('admin.vendors_edit', id=id),
        'form': form
    }
    return render_template('admin/vendors/new-edit.html', **kwargs)

    
@adminview.route('/vendors/delete/<int:id>', methods=['GET', 'POST'])
def vendors_delete(id):
    vendor = Vendor.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(vendor)
        db.session.commit()
        flash(u'%s 已删除' % vendor.alias, 'success') # Diff
        return redirect(url_for('admin.vendors'))
        
    kwargs = {
        'title': u'删除厂商',
        'action': url_for('admin.vendors_delete', id=id),
        'fields': [(u'显示名', vendor.alias)], # Diff
        'type' : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)

    
@adminview.route('/vendors/delete/all', methods=['GET', 'POST'])
def vendors_delete_all():
    name, alias, cls = 'vendors', u'供应商', Vendor
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            db.session.delete(cls.query.get(int(i)))
        db.session.commit()
        flash(u'成功删除 %d 个%s!' % (len(ids), alias) , 'success')
        return redirect(url_for('admin.%s' % name))

    ids = dict(request.values.lists()).get('id[]', [])
    objs = cls.query.filter(cls.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title': u'批量删除%s' % alias,
        'action': url_for('admin.%s_delete_all' % name),
        'fields': [(obj.id, alias, obj.alias) for obj in objs], # Diff
        'type' : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)


# ==============================================================================
#  设备
# ==============================================================================    
@adminview.route('/models/')
def models():
    form = SearchForm(formdata=request.args)
    cls, table_cls = Model, ModelTable
    query = cls.query
    if form.keyword.data:
        ikeyword = '%' + form.keyword.data + '%'
        query = query.filter(db.or_(cls.name.ilike(ikeyword),
                                    cls.alias.ilike(ikeyword)))
    table = make_table(query, table_cls)
    return render_template('admin/models/index.html',
                           table = table, form=form)

    
@adminview.route('/models/new', methods=['GET', 'POST'])
def models_new():
    form = ModelForm()
    if form.is_submitted and form.validate_on_submit():
        model = Model()
        form.populate_obj(model)
        db.session.add(model)
        db.session.commit()
        flash(u'%s 添加成功' % model.alias, 'success') # Diff
        return redirect(url_for('admin.models'))
        
    kwargs = {
        'title' : u'添加设备',
        'action' : url_for('admin.models_new'),
        'form' : form,
    }
    return render_template('admin/models/new-edit.html', **kwargs)

@adminview.route('/models/edit/<int:id>', methods=['GET', 'POST'])
def models_edit(id):
    form = ModelForm()
    model = Model.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(model)
        db.session.commit()
        flash(u'%s 修改成功' % model.alias, 'success') # Diff
        return redirect(url_for('admin.models'))
        
    form.process(obj=model)
    kwargs = {
        'title': u'编辑设备',
        'action': url_for('admin.models_edit', id=id),
        'form': form
    }
    return render_template('admin/models/new-edit.html', **kwargs)
    
@adminview.route('/models/delete/<int:id>', methods=['GET', 'POST'])
def models_delete(id):
    model = Model.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(model)
        db.session.commit()
        flash(u'%s 已删除' % model.alias, 'success') # Diff
        return redirect(url_for('admin.models'))
        
    kwargs = {
        'title': u'删除设备',
        'action': url_for('admin.models_delete', id=id),
        'fields': [(u'显示名', model.alias)], # Diff
        'type' : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)

    
@adminview.route('/models/delete/all', methods=['GET', 'POST'])
def models_delete_all():
    name, alias, cls = 'models', u'设备', Model
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            db.session.delete(cls.query.get(int(i)))
        db.session.commit()
        flash(u'成功删除 %d 个%s!' % (len(ids), alias) , 'success')
        return redirect(url_for('admin.%s' % name))

    ids = dict(request.values.lists()).get('id[]', [])
    objs = cls.query.filter(cls.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title': u'批量删除%s' % alias,
        'action': url_for('admin.%s_delete_all' % name),
        'fields': [(obj.id, alias, obj.alias) for obj in objs], # Diff
        'type' : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)


    
# ==============================================================================
#  SysOID
# ==============================================================================    
@adminview.route('/sysoids/')
def sysoids():
    form = SearchForm(formdata=request.args)
    cls, table_cls = SysOid, SysOidTable
    query = cls.query
    if form.keyword.data:
        ikeyword = '%' + form.keyword.data + '%'
        query = query.filter(db.or_(cls.sysoid.ilike(ikeyword),
                                    cls.disco.ilike(ikeyword),
                                    cls.mib.ilike(ikeyword),
                                    cls.remark.ilike(ikeyword)))
    table = make_table(query, table_cls)
    return render_template('admin/sysoids/index.html',
                           table = table, form=form)

    
@adminview.route('/sysoids/new', methods=['GET', 'POST'])
def sysoids_new():
    form = SysoidForm()
    if form.is_submitted and form.validate_on_submit():
        sysoid = SysOid()
        form.populate_obj(sysoid)
        db.session.add(sysoid)
        db.session.commit()
        return redirect(url_for('admin.sysoids'))
        
    kwargs = {
        'title' : u'添加Sysoid',
        'action' : url_for('admin.sysoids_new'),
        'form' : form,
    }
    return render_template('admin/sysoids/new-edit.html', **kwargs)

@adminview.route('/sysoids/edit/<int:id>', methods=['GET', 'POST'])
def sysoids_edit(id):
    form = SysoidForm()
    sysoid = SysOid.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(sysoid)
        db.session.commit()
        return redirect(url_for('admin.sysoids'))
        
    form.process(obj=sysoid)
    kwargs = {
        'title': u'编辑Sysoid',
        'action': url_for('admin.sysoids_edit', id=id),
        'form': form
    }
    return render_template('admin/sysoids/new-edit.html', **kwargs)
    
@adminview.route('/sysoids/delete/<int:id>', methods=['GET', 'POST'])
def sysoids_delete(id):
    sysoid = SysOid.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(sysoid)
        db.session.commit()
        return redirect(url_for('admin.sysoids'))
        
    kwargs = {
        'title': u'删除Sysoid',
        'action': url_for('admin.sysoids_delete', id=id),
        'fields': [(u'Sysoid', sysoid.sysoid)],
        'type' : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)

    
@adminview.route('/sysoids/delete/all', methods=['GET', 'POST'])
def sysoids_delete_all():
    name, alias, cls = 'sysoids', u'Sysoid', SysOid 
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            db.session.delete(cls.query.get(int(i)))
        db.session.commit()
        flash(u'成功删除 %d 个%s!' % (len(ids), alias) , 'success')
        return redirect(url_for('admin.%s' % name))

    ids = dict(request.values.lists()).get('id[]', [])
    objs = cls.query.filter(cls.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title': u'批量删除%s' % alias,
        'action': url_for('admin.%s_delete_all' % name),
        'fields': [(obj.id, alias, obj.sysoid) for obj in objs], # Diff
        'type' : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)


# ==============================================================================
#  采集模块
# ==============================================================================    
@adminview.route('/modules/')
def modules():
    form = SearchForm(formdata=request.args)
    cls, table_cls = Module, ModuleTable
    query = cls.query
    if form.keyword.data:
        ikeyword = '%' + form.keyword.data + '%'
        query = query.filter(db.or_(cls.name.ilike(ikeyword),
                                    cls.alias.ilike(ikeyword)))
    table = make_table(query, table_cls)
    return render_template('admin/modules/index.html',
                           table = table, form=form)

    
@adminview.route('/modules/new', methods=['GET', 'POST'])
def modules_new():
    form = ModuleForm()
    if form.is_submitted and form.validate_on_submit():
        module = Module()
        form.populate_obj(module)
        db.session.add(module)
        db.session.commit()
        flash(u'%s 添加成功' % module.alias, 'success') # Diff
        return redirect(url_for('admin.modules'))
        
    kwargs = {
        'title' : u'添加采集模块',
        'action' : url_for('admin.modules_new'),
        'form' : form,
    }
    return render_template('admin/modules/new-edit.html', **kwargs)

@adminview.route('/modules/edit/<int:id>', methods=['GET', 'POST'])
def modules_edit(id):
    form = ModuleForm()
    module = Module.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(module)
        db.session.commit()
        flash(u'%s 修改成功' % module.alias, 'success') # Diff
        return redirect(url_for('admin.modules'))
        
    form.process(obj=module)
    kwargs = {
        'title': u'编辑采集模块',
        'action': url_for('admin.modules_edit', id=id),
        'form': form
    }
    return render_template('admin/modules/new-edit.html', **kwargs)
    
@adminview.route('/modules/delete/<int:id>', methods=['GET', 'POST'])
def modules_delete(id):
    module = Module.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(module)
        db.session.commit()
        flash(u'%s 已删除' % module.alias, 'success') # Diff
        return redirect(url_for('admin.modules'))
        
    kwargs = {
        'title': u'删除采集模块',
        'action': url_for('admin.modules_delete', id=id),
        'fields': [(u'显示名', module.alias)], # Diff
        'type' : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)

    
@adminview.route('/modules/delete/all', methods=['GET', 'POST'])
def modules_delete_all():
    name, alias, cls = 'modules', u'采集模块', Module
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            db.session.delete(cls.query.get(int(i)))
        db.session.commit()
        flash(u'成功删除 %d 个%s!' % (len(ids), alias) , 'success')
        return redirect(url_for('admin.%s' % name))

    ids = dict(request.values.lists()).get('id[]', [])
    objs = cls.query.filter(cls.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title': u'批量删除%s' % alias,
        'action': url_for('admin.%s_delete_all' % name),
        'fields': [(obj.id, alias, obj.alias) for obj in objs], # Diff
        'type' : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)


    
# ==============================================================================
#  监控器
# ==============================================================================    
@adminview.route('/monitors/')
def monitors():
    form = SearchForm(formdata=request.args)
    cls, table_cls = Monitor, MonitorTable
    query = cls.query
    if form.keyword.data:
        ikeyword = '%' + form.keyword.data + '%'
        query = query.filter(db.or_(cls.category.ilike(ikeyword),
                                    cls.vendor.ilike(ikeyword),
                                    cls.remark.ilike(ikeyword)))
    table = make_table(query, table_cls)
    return render_template('admin/monitors/index.html',
                           table = table, form=form)

    
@adminview.route('/monitors/new', methods=['GET', 'POST'])
def monitors_new():
    form = MonitorForm()
    if form.is_submitted and form.validate_on_submit():
        monitor = Monitor()
        form.populate_obj(monitor)
        db.session.add(monitor)
        db.session.commit()
        return redirect(url_for('admin.monitors'))
        
    kwargs = {
        'title' : u'添加监控器',
        'action' : url_for('admin.monitors_new'),
        'form' : form,
    }
    return render_template('admin/monitors/new-edit.html', **kwargs)

@adminview.route('/monitors/edit/<int:id>', methods=['GET', 'POST'])
def monitors_edit(id):
    form = MonitorForm()
    monitor = Monitor.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(monitor)
        db.session.commit()
        return redirect(url_for('admin.monitors'))
        
    form.process(obj=monitor)
    kwargs = {
        'title': u'编辑监控器',
        'action': url_for('admin.monitors_edit', id=id),
        'form': form
    }
    return render_template('admin/monitors/new-edit.html', **kwargs)
    
@adminview.route('/monitors/delete/<int:id>', methods=['GET', 'POST'])
def monitors_delete(id):
    monitor = Monitor.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(monitor)
        db.session.commit()
        return redirect(url_for('admin.monitors'))
        
    kwargs = {
        'title': u'删除监控器',
        'action': url_for('admin.monitors_delete', id=id),
        'fields': [(u'备注', monitor.remark)],
        'type' : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)

    
@adminview.route('/monitors/delete/all', methods=['GET', 'POST'])
def monitors_delete_all():
    name, alias, cls = 'monitors', u'监控器', Monitor
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            db.session.delete(cls.query.get(int(i)))
        db.session.commit()
        flash(u'成功删除 %d 个%s!' % (len(ids), alias) , 'success')
        return redirect(url_for('admin.%s' % name))

    ids = dict(request.values.lists()).get('id[]', [])
    objs = cls.query.filter(cls.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title': u'批量删除%s' % alias,
        'action': url_for('admin.%s_delete_all' % name),
        'fields': [(obj.id, alias, obj.remark) for obj in objs], # Diff
        'type' : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)
    
    
# ==============================================================================
#  MIB管理
# ==============================================================================    
@adminview.route('/miboids/')
@adminview.route('/miboids/<mib>')
def miboids(mib='mib1'):
    form = SearchForm(formdata=request.args)
    cls, table_cls = Miboid, MiboidTable
    query = cls.query.filter_by(mib=mib)
    
    if form.keyword.data:
        ikeyword = '%' + form.keyword.data + '%'
        query = query.filter(db.or_(cls.name.ilike(ikeyword),
                                    cls.alias.ilike(ikeyword)))
    table = make_table(query, table_cls)
    kwargs = {
        'menuid' : mib,
        'form'   : form,
        'table'  : table,
        'mib'    : mib,
    }
    return render_template("admin/miboids/index.html", **kwargs)
    
    
@adminview.route('/miboids/new', methods=['GET', 'POST'])
def miboids_new():
    form = MiboidForm()
    if form.is_submitted and form.validate_on_submit():
        miboid = Miboid()
        form.populate_obj(miboid)
        db.session.add(miboid)
        db.session.commit()
        return redirect(url_for('admin.miboids'))
        
    kwargs = {
        'title'  : u'添加MIB',
        'action' : url_for('admin.miboids_new'),
        'form'   : form,
    }
    return render_template('admin/miboids/new-edit.html', **kwargs)

@adminview.route('/miboids/edit/<int:id>', methods=['GET', 'POST'])
def miboids_edit(id):
    form = MiboidForm()
    miboid = Miboid.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(miboid)
        db.session.commit()
        return redirect(url_for('admin.miboids'))
        
    form.process(obj=miboid)
    kwargs = {
        'title'  : u'编辑MIB',
        'menuid' : miboid.mib,
        'action' : url_for('admin.miboids_edit', id=id),
        'form'   : form,
    }
    return render_template('admin/miboids/new-edit.html', **kwargs)
    
@adminview.route('/miboids/delete/<int:id>', methods=['GET', 'POST'])
def miboids_delete(id):
    miboid = Miboid.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(miboid)
        db.session.commit()
        return redirect(url_for('admin.miboids'))
        
    kwargs = {
        'title'  : u'删除MIB',
        'action' : url_for('admin.miboids_delete', id=id),
        'fields' : [(u'备注', miboid.remark)],
        'type'   : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)

    
@adminview.route('/miboids/delete/all', methods=['GET', 'POST'])
def miboids_delete_all():
    name, alias, cls = 'miboids', u'MIB', Miboid
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            db.session.delete(cls.query.get(int(i)))
        db.session.commit()
        flash(u'成功删除 %d 个%s!' % (len(ids), alias) , 'success')
        return redirect(url_for('admin.%s' % name))

    ids = dict(request.values.lists()).get('id[]', [])
    objs = cls.query.filter(cls.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title'  : u'批量删除%s' % alias,
        'action' : url_for('admin.%s_delete_all' % name),
        'fields' : [(obj.id, alias, obj.alias) for obj in objs], # Diff
        'type'   : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)
