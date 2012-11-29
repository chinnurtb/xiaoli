#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json,send_file

from sqlalchemy import or_

from tango import db,get_profile
from tango.ui.tables import make_table
from tango.login import current_user
from tango.models import Profile, Category
from tango.excel.CsvExport import CsvExport

from .models import NodeOlt,NODE_STATUS_DICT, Area, Vendor, Model
from .tables import OltTable
from .forms import  OltSearchForm, OltNewForm
from .views import nodeview

@nodeview.route('/nodes/olts.csv/', methods=['POST', 'GET'])
@nodeview.route('/nodes/olts/', methods=['POST', 'GET'])
def olts():
    form = OltSearchForm()
    query = NodeOlt.query
    query = query.outerjoin(Area, NodeOlt.area_id==Area.id)

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
    query = query.filter(Area.id.in_(current_user.domain.area_ids(3)))
    form.process(**query_dict)
    table = make_table(query, OltTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeOlt.query.filter(NodeOlt.status == status).filter(NodeOlt.area_id.in_(current_user.domain.area_ids(3))).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})

    if request.base_url.endswith(".csv/"):
        csv = CsvExport('olts',columns=NodeOlt.export_columns())
        return send_file(csv.export(query,format={'status': lambda value: NODE_STATUS_DICT.get(value)}),as_attachment=True,attachment_filename='olts.csv')
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
        elif NodeOlt.query.filter(NodeOlt.addr==node.addr).count() > 0:
            flash(u'OLT IP地址不能重复','error')
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
            elif node.addr != form.addr.data and NodeOlt.query.filter(NodeOlt.addr==form.addr.data).count() > 0:
                flash(u'OLT IP地址不能重复','error')
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
    node = NodeOlt.query.get_or_404(id)
    chartdata = [
        {
            "area": True,
            "key" : u"接收流量" ,
            "color": 'lime',
            "values" : [ {'x':1352937600000 , 'y':27.38478809681} ,
                { 'x':1352947600000 , 'y':27.371377218208} ,
                { 'x':1352957600000 , 'y':26.309915460827} ,
                {  'x':1352967600000 , 'y':26.425199957521} ,
                {  'x':1352977600000 ,'y': 26.823411519395} ,
                {  'x':1352987600000 ,'y': 23.850443591584} ,
                {  'x':1352997600000 ,'y': 23.158355444054} ,
                {  'x':1353007600000 , 'y':22.998689393694} ,
                {  'x':1353017600000 ,'y': 27.977128511299} ,]
        } ,
        {
            "area": True,
            "color": '#773EF7',
            "key" : u"发送流量" ,
            "values" :[{'x':1352937600000 , 'y':12} ,
                    { 'x':1352947600000 , 'y':110} ,
                    { 'x':1352957600000 , 'y':110} ,
                    {  'x':1352967600000 , 'y':30} ,
                    {  'x':1352977600000 ,'y': 60} ,
                    {  'x':1352987600000 ,'y': 6} ,
                    {  'x':1352997600000 ,'y': 12} ,
                    {  'x':1353007600000 , 'y':10} ,
                    {  'x':1353017600000 ,'y': 0} ,]
        } ,
    ];
    data = [{'label': u'完全故障', 'color': 'red', 'value': 1},
            {'label': u'部分故障', 'color': 'yellow', 'value': 2},
            {'label': u'完全正常', 'color': 'green', 'value': 19},
            {'label': u'数据缺失', 'color': 'blue', 'value': 2}]
    chartdata2 = [{'values': data}]
    return render_template('nodes/olts/show.html', node = node, chartdata = chartdata, chartdata2 = chartdata2)

import os
import operator
from flask import Markup
from werkzeug import secure_filename
from tango.excel.CsvImport import CsvImport,ImportColumn
@nodeview.route('/nodes/olts/import/', methods=['POST'])
def olts_import():
    if request.method == 'POST':
        file = request.files['file']
        if file and file.filename.endswith('csv'):
            filename = secure_filename(file.filename)
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','static','file','upload')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file_path = os.path.join(root_path, filename.split('.')[0]+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
            file.save(file_path)
            reader = CsvImport(session=db.session.bind, table='node_olts')
            reader.addColumn(
                ImportColumn(u'名称', 'name', 'character varying(40)')
            ).addColumn(
                ImportColumn(u'别名', 'alias', 'character varying(200)')
            ).addColumn(
                ImportColumn(u'节点类型', 'category_id', 'integer', default=20)
            ).addColumn(
                ImportColumn(u'IP地址', 'addr', 'character varying(200)', is_key=True)
            ).addColumn(
                ImportColumn(u'所属区域', 'area_id', 'integer',allow_null=False, existed_data=dict([(area.full_name, area.id) for area in Area.query.filter(Area.area_type==3)]), )
            ).addColumn(
                ImportColumn(u'厂商', 'vendor_id', 'integer',allow_null=False, existed_data=dict([(vendor.alias, vendor.id) for vendor in Vendor.query.filter(Vendor.is_valid==1)]),)
            ).addColumn(
                ImportColumn(u'型号', 'model_id', 'integer',allow_null=False, existed_data=dict([(model.alias, model.id) for model in Model.query.filter(Model.is_valid==1)]),)
            ).addColumn(
                ImportColumn(u'子网掩码', 'mask', 'character varying(200)')
            ).addColumn(
                ImportColumn(u'读团体名', 'snmp_comm', 'character varying(50)')
            ).addColumn(
                ImportColumn(u'写团体名', 'snmp_wcomm', 'character varying(50)')
            ).addColumn(
                ImportColumn(u'SNMP版本', 'snmp_ver', 'character varying(50)')
            ).addColumn(
                ImportColumn(u'位置', 'location', 'character varying(200)')
            ).addColumn(
                ImportColumn(u'备注', 'remark', 'character varying(200)')
            )
            update_dict = {}
            key_list = [column.name_en for column in reader.columns if column.is_key]
            attr_list = ['id',]+[column.name_en for column in reader.columns]
            for node in NodeOlt.query.all():
                f = operator.attrgetter(*key_list)
                key = (f(node),) if len(key_list) == 1 else f(node)
                f2 = operator.attrgetter(*attr_list)
                update_dict[key] = dict(zip(attr_list, f2(node)))
            reader.load_update(permit_update_dict=update_dict, update_dict=update_dict)
            info = reader.read(file=file_path)
            flash(Markup(info), 'success')
        else:
            flash(u"上传文件格式错误", 'error')
    return redirect(url_for('nodes.olts'))
