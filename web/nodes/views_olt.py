#!/usr/bin/env python
# coding: utf-8
from datetime import datetime, timedelta
import time

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash, current_app
from flask import json,send_file

from sqlalchemy import or_

from tango import db,get_profile
from tango.ui.tables import make_table
from tango.login import current_user
from tango.models import Profile, Category
from tango.excel import XlsExport

from .models import NodeOlt,NODE_STATUS_DICT, Area, Vendor, Model, Node
from .tables import OltTable
from .forms import  OltSearchForm, OltNewForm
from .views import nodeview

import errdb

@nodeview.route('/nodes/olts.xls/', methods=['POST', 'GET'])
@nodeview.route('/nodes/olts/', methods=['POST', 'GET'])
def olts():
    form = OltSearchForm()
    query = NodeOlt.query.outerjoin(Area, NodeOlt.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("keyword"):
        query=query.filter(or_(
            NodeOlt.name.like('%'+query_dict["keyword"]+'%'),
            NodeOlt.alias.like('%'+query_dict["keyword"]+'%'),
            NodeOlt.addr.like('%'+query_dict["keyword"]+'%')
        ))
    if query_dict.get("area"):
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(NodeOlt.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(NodeOlt.model_id == query_dict["model_id"])    # ==
    if query_dict.get("status"): query=query.filter(NodeOlt.status == query_dict["status"])
    if not current_user.is_province_user: query = query.filter(current_user.domain.clause_permit)
    form.process(**query_dict)
    table = make_table(query, OltTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeOlt.query.filter(NodeOlt.status == status)
        if not current_user.is_province_user: num = num.outerjoin(Area, NodeOlt.area_id==Area.id).filter(current_user.domain.clause_permit)
        num = num.count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})

    if request.base_url.endswith(".xls/"):
        csv = XlsExport('olts',columns=NodeOlt.export_columns())
        return send_file(csv.export(query,format={'status': lambda value: NODE_STATUS_DICT.get(value)}),as_attachment=True,attachment_filename='olts.xls')
    else:
        return render_template('/nodes/olts/index.html', table = table, form=form, status_statistcs=status_statistcs)

@nodeview.route('/nodes/olts/new/', methods=['GET','POST'])
def olts_new():
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = OltNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        node = NodeOlt()
        form.populate_obj(node)
        if NodeOlt.query.filter(NodeOlt.name==node.name).count() > 0:
            flash(u'OLT名称不能重复','error')
        elif NodeOlt.query.filter(NodeOlt.alias==node.alias).count() > 0:
            flash(u'OLT别名不能重复','error')
        elif Node.query.filter(Node.addr==node.addr).count() > 0:
            flash(u'IP地址不能重复','error')
        else:
            node.status = 1
            node.category_id = 20
            db.session.add(node)
            db.session.commit()
            flash(u'添加OLT %s 成功'% node.name, 'success')
            return redirect(url_for('nodes.olts'))
    return render_template('nodes/olts/new.html', form = form, next=next)

@nodeview.route('/nodes/olts/edit/<int:id>/', methods=['POST', 'GET'])
def olts_edit(id):
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = OltNewForm()
    node = NodeOlt.query.get_or_404(id)
    if request.method == 'POST':
        if form.validate_on_submit():
            if node.name != form.name.data and NodeOlt.query.filter(NodeOlt.name==form.name.data).count() > 0:
                flash(u'OLT名称不能重复','error')
            elif node.alias != form.alias.data and NodeOlt.query.filter(NodeOlt.alias==form.alias.data).count() > 0:
                flash(u'OLT别名不能重复','error')
            elif node.addr != form.addr.data and Node.query.filter(Node.addr==form.addr.data).count() > 0:
                flash(u'IP地址不能重复','error')
            else:
                del form._fields["cityid"]
                del form._fields["town"]
                form.populate_obj(node)
                node.updated_at = datetime.now()
                db.session.add(node)
                db.session.commit()
                flash(u'修改OLT %s 成功'% node.name,'success')
                return redirect(url_for('nodes.olts'))
    else:
        form.process(obj=node)
    return render_template('/nodes/olts/edit.html', node=node, form=form, next=next)

@nodeview.route('/nodes/olts/delete/', methods=['POST'])
def olts_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            node = NodeOlt.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除OLT成功','success')
        return redirect(url_for('nodes.olts'))

@nodeview.route('/nodes/olts/<int:id>/', methods=['GET'])
def olts_show(id):
    node = NodeOlt.query.get(id)
    if node is None:
        return render_template('/nodes/not_exist.html', menuid='olts', message=u'OLT不存在，可能已经被删除',title=u'OLT')
    data_ifInOctets, data_ifOutOctets = node.get_traffic()
    chartdata = [
        {"area": True, "key" : u"接收流量" , "color": 'lime', "values" : data_ifInOctets} ,
        {"area": True, "key" : u"发送流量" , "color": '#773EF7',"values" :data_ifOutOctets} ,
    ];
    data = [{'label': u'完全故障', 'color': 'red', 'value': 1},
            {'label': u'部分故障', 'color': 'yellow', 'value': 2},
            {'label': u'完全正常', 'color': 'green', 'value': 19},
            {'label': u'数据缺失', 'color': 'blue', 'value': 2}]
    chartdata2 = [{'values': data}]
    return render_template('nodes/olts/show.html', node = node, chartdata = chartdata, chartdata2 = chartdata2)

@nodeview.route('/nodes/olts/ping_delay/<int:id>/', methods=['GET'])
def ping_delay(id):
    node = NodeOlt.query.get(id)
    data = node.get_ping_delay()
    chartdata = [{"area": True, "key" : u"接收流量" , "color": 'lime', "values" : data}];
    return render_template('nodes/olts/ping_delay.html', chartdata = chartdata)

import os
from flask import Markup
from werkzeug import secure_filename
@nodeview.route('/nodes/olts/import/', methods=['POST'])
def olts_import():
    if request.method == 'POST':
        file = request.files['file']
        if file and file.filename.endswith('xls'):
            filename = secure_filename(file.filename)
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','static','file','upload')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file_path = os.path.join(root_path, filename.split('.')[0]+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.xls')
            file.save(file_path)
            from tango.excel import OltImport
            reader = OltImport(engine=db.session.bind)
            vendor_dict = dict([(vendor.alias, vendor.id) for vendor in Vendor.query.filter(Vendor.is_valid==1).all()])
            info = reader.read(file=file_path, data_dict={'branch_name':current_user.domain.import_permit(3),'vendor_id':vendor_dict,'snmp_ver':['v1','v2c']})
            flash(Markup(info), 'success')
        else:
            flash(u"上传文件格式错误", 'error')
    return redirect(url_for('nodes.olts'))
