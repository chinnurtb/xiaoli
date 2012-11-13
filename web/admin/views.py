# coding: utf-8

from sqlalchemy import func
from flask import (Blueprint, request, url_for, redirect, render_template, flash)

from tango import db
from tango.ui.tables import make_table
from tango.models import Category

from nodes.models import Vendor, SysOid, Model

from .models import Module, Monitor, Miboid
from .forms import SearchForm, CategoryForm, VendorForm, ModelForm,\
    SysoidForm, ModuleForm, MonitorForm, MiboidForm
from .tables import CategoryTable, VendorTable, ModuleTable, \
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
