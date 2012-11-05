# coding: utf-8

from sqlalchemy import func
from flask import (Blueprint, request, url_for, redirect, render_template, flash)

from tango import db
from tango.ui.tables import make_table
from tango.models import Category

from nodes.models import Vendor, SysOid, Model

from .models import Module, Monitor, Miboid
from .forms import CategoryForm
from .tables import CategoryTable, VendorTable, ModuleTable, \
    ModelTable, SysOidTable, MonitorTable, MiboidTable

adminview = Blueprint('admin', __name__, url_prefix='/admin')

@adminview.route('/')
def index():
    return render_template('admin/index.html')

# ==============================================================================
#  分类
# ==============================================================================    
@adminview.route('/categories/')
def categories():
    table = make_table(Category.query, CategoryTable)
    return render_template('admin/categories/index.html',
        table = table)

    
@adminview.route('/categories/new')
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

@adminview.route('/categories/edit/<int:id>')
def categories_edit(id):
    pass
    
@adminview.route('/categories/delete/<int:id>')
def categories_delete(id):
    pass
    
    
@adminview.route('/categories/delete/all')
def categories_delete_all():
    pass
    
    
# ==============================================================================
#  供应商
# ==============================================================================    
@adminview.route('/vendors/')
def vendors():
    table = make_table(Vendor.query, VendorTable)
    return render_template('admin/vendors/index.html',
        table = table)

@adminview.route('/vendors/new')
def vendors_new():
    pass

@adminview.route('/vendors/edit/<int:id>')
def vendors_edit(id):
    pass
    

@adminview.route('/vendors/delete/<int:id>')
def vendors_delete(id):
    pass

@adminview.route('/vendors/delete/all')
def vendors_delete_all():
    pass

    
# ==============================================================================
#  型号
# ==============================================================================    
@adminview.route('/models/')
def models():
    table = make_table(Model.query, ModelTable)
    return render_template('admin/models/index.html',
        table = table)

@adminview.route('/models/new')
def models_new():
    pass

@adminview.route('/models/edit/<int:id>')
def models_edit(id):
    pass

@adminview.route('/models/delete/<int:id>')
def models_delete(id):
    pass

@adminview.route('/models/delete/all')
def models_delete_all():
    pass
    
# ==============================================================================
#  SysOID
# ==============================================================================    
@adminview.route('/sysoids/')
def sysoids():
    table = make_table(SysOid.query, SysOidTable)
    return render_template('admin/sysoids/index.html',
        table = table)
    
    
# ==============================================================================
#  监控模块
# ==============================================================================    
@adminview.route('/modules/')
def modules():
    table = make_table(Module.query, ModuleTable)
    return render_template('admin/modules/index.html',
        table=table)

# ==============================================================================
#  监控器
# ==============================================================================    
@adminview.route('/monitors/')
def monitors():
    table = make_table(Monitor.query, MonitorTable)
    return render_template('admin/monitors/index.html',
        table = table)

# ==============================================================================
#  MIB管理
# ==============================================================================    
@adminview.route('/miboids/')
def miboids():
    mib = request.args.get('mib', '')
    query = Miboid.query
    if mib:
        query = query.filter_by(mib=mib)
    table = make_table(query, MiboidTable)
    mibs = db.session.query(func.distinct(Miboid.mib)).all()
    return render_template("admin/miboids/index.html", table=table, mibs=mibs, mib=mib)
