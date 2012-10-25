# coding: utf-8

from sqlalchemy import func
from flask import (Blueprint, request, url_for, redirect, render_template, flash)

from tango import db

from tango.base import make_table

from tango.models import Category

from nodes.models import Vendor, SysOid, Model

from .models import Module, Monitor, Miboid

from .tables import CategoryTable, VendorTable, ModuleTable, \
    ModelTable, SysOidTable, MonitorTable, MiboidTable

adminview = Blueprint('admin', __name__, url_prefix='/admin')

@adminview.route('/')
def index():
    return render_template('admin/index.html')

@adminview.route('/categories/')
def categories():
    table = make_table(Category.query, CategoryTable)
    return render_template('admin/categories/index.html',
        table = table)

@adminview.route('/vendors/')
def vendors():
    table = make_table(Vendor.query, VendorTable)
    return render_template('admin/vendors/index.html',
        table = table)

@adminview.route('/models/')
def models():
    table = make_table(Model.query, ModelTable)
    return render_template('admin/models/index.html',
        table = table)

@adminview.route('/sysoids/')
def sysoids():
    table = make_table(SysOid.query, SysOidTable)
    return render_template('admin/sysoids/index.html',
        table = table)

@adminview.route('/modules/')
def modules():
    table = make_table(Module.query, ModuleTable)
    return render_template('admin/modules/index.html',
        table=table)

@adminview.route('/monitors/')
def monitors():
    table = make_table(Monitor.query, MonitorTable)
    return render_template('admin/monitors/index.html',
        table = table)

@adminview.route('/miboids/')
def miboids():
    mib = request.args.get('mib', '')
    query = Miboid.query
    if mib:
        query = query.filter_by(mib=mib)
    table = make_table(query, MiboidTable)
    mibs = db.session.query(func.distinct(Miboid.mib)).all()
    return render_template("admin/miboids/index.html", table=table, mibs=mibs, mib=mib)
