#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json,send_file

from sqlalchemy import or_

from tango import db,get_profile
from tango.ui.tables import make_table
from tango.login import current_user, login_required
from tango.models import Profile, Category
from tango.excel.CsvExport import CsvExport

from .models import NodeSwitch,NODE_STATUS_DICT, Area
from .tables import SwitchTable
from .forms import  SwitchSearchForm, SwitchNewForm
from .views import nodeview

@nodeview.route('/nodes/switches.csv/', methods=['POST', 'GET'])
@nodeview.route('/nodes/switches/', methods=['POST', 'GET'])
@login_required
def switches():
    form = SwitchSearchForm()
    query = NodeSwitch.query
    query = query.outerjoin(Area, NodeSwitch.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("keyword"):
        query=query.filter(or_(
            NodeSwitch.name.like('%'+query_dict["keyword"]+'%'),
            NodeSwitch.alias.like('%'+query_dict["keyword"]+'%'),
            NodeSwitch.addr.like('%'+query_dict["keyword"]+'%')
        ))
    if query_dict.get("area"):
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(NodeSwitch.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(NodeSwitch.model_id == query_dict["model_id"])    # ==
    if query_dict.get("status"): query=query.filter(NodeSwitch.status == query_dict["status"])
    form.process(**query_dict)
    table = make_table(query, SwitchTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeSwitch.query.filter(NodeSwitch.status == status).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})

    if request.base_url.endswith(".csv/"):
        csv = CsvExport('switches',columns=NodeSwitch.export_columns())
        return send_file(csv.export(query,format={'status': lambda value: NODE_STATUS_DICT.get(value)}),as_attachment=True,attachment_filename='switches.csv')
    else:
        return render_template('/nodes/switches/index.html', table = table, form=form, status_statistcs=status_statistcs)

@nodeview.route('/nodes/switches/new/', methods=['GET','POST'])
@login_required
def switches_new():
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = SwitchNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        node = NodeSwitch()
        form.populate_obj(node)
        if NodeSwitch.query.filter(NodeSwitch.name==node.name).count() > 0:
            flash(u'交换机名称不能重复','error')
        elif NodeSwitch.query.filter(NodeSwitch.alias==node.alias).count() > 0:
            flash(u'交换机别名不能重复','error')
        elif NodeSwitch.query.filter(NodeSwitch.addr==node.addr).count() > 0:
            flash(u'交换机 IP地址不能重复','error')
        else:
            node.status = 1
            node.category_id = 2
            db.session.add(node)
            db.session.commit()
            flash(u'添加交换机 %s 成功'% node.name, 'success')
            return redirect(url_for('nodes.switches'))
    return render_template('nodes/switches/new.html', form = form, next=next)

@nodeview.route('/nodes/switches/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def switches_edit(id):
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = SwitchNewForm()
    node = NodeSwitch.query.get_or_404(id)
    if request.method == 'POST':
        if form.validate_on_submit():
            if node.name != form.name.data and NodeSwitch.query.filter(NodeSwitch.name==form.name.data).count() > 0:
                flash(u'交换机名称不能重复','error')
            elif node.alias != form.alias.data and NodeSwitch.query.filter(NodeSwitch.alias==form.alias.data).count() > 0:
                flash(u'交换机别名不能重复','error')
            elif node.addr != form.addr.data and NodeSwitch.query.filter(NodeSwitch.addr==form.addr.data).count() > 0:
                flash(u'交换机 IP地址不能重复','error')
            else:
                del form._fields["cityid"]
                del form._fields["town"]
                form.populate_obj(node)
                node.updated_at = datetime.now()
                db.session.add(node)
                db.session.commit()
                flash(u'修改交换机 %s 成功'% node.name,'success')
                return redirect(url_for('nodes.switches'))
    else:
        form.process(obj=node)
    return render_template('/nodes/switches/edit.html', node=node, form=form, next=next)

@nodeview.route('/nodes/switches/delete/', methods=['POST'])
def switches_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            node = NodeSwitch.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除交换机成功','success')
        return redirect(url_for('nodes.switches'))

@nodeview.route('/nodes/switches/<int:id>/', methods=['GET'])
@login_required
def switches_show(id):
    node = NodeSwitch.query.get_or_404(id)
    data = [{'label': u'完全故障', 'color': 'red', 'value': 1},
            {'label': u'部分故障', 'color': 'yellow', 'value': 2},
            {'label': u'完全正常', 'color': 'green', 'value': 19},
            {'label': u'数据缺失', 'color': 'blue', 'value': 2}]
    chartdata2 = [{'values': data}]
    return render_template('nodes/switches/show.html', node = node, chartdata2 = chartdata2)

import os
import operator
from flask import Markup
from werkzeug import secure_filename
from tango.excel.CsvImport import CsvImport,ImportColumn
@nodeview.route('/nodes/switches/import/', methods=['POST'])
@login_required
def switches_import():
    if request.method == 'POST':
        file = request.files['file']
        if file and file.filename.endswith('csv'):
            filename = secure_filename(file.filename)
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','static','file','upload')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file_path = os.path.join(root_path, filename.split('.')[0]+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
            file.save(file_path)
            reader = CsvImport(session=db.session.bind, table='node_switchs')
            reader.addColumn(
                ImportColumn(u'名称', 'name', 'character varying(40)')
            ).addColumn(
                ImportColumn(u'别名', 'alias', 'character varying(200)')
            ).addColumn(
                ImportColumn(u'节点类型', 'category_id', 'integer', default=2)
            ).addColumn(
                ImportColumn(u'IP地址', 'addr', 'character varying(200)', is_key=True)
            ).addColumn(
                ImportColumn(u'所属区域', 'area_id', 'integer',allow_null=False, existed_data=dict([(area.full_name, area.id) for area in Area.query.filter(Area.area_type==4)]), )
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
            for node in NodeSwitch.query.all():
                f = operator.attrgetter(*key_list)
                key = (f(node),) if len(key_list) == 1 else f(node)
                f2 = operator.attrgetter(*attr_list)
                update_dict[key] = dict(zip(attr_list, f2(node)))
            reader.load_update(permit_update_dict=update_dict, update_dict=update_dict)
            info = reader.read(file=file_path)
            flash(Markup(info), 'success')
        else:
            flash(u"上传文件格式错误", 'error')
    return redirect(url_for('nodes.switches'))
