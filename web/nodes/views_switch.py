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
from tango.excel import XlsExport

from .models import NodeSwitch,NODE_STATUS_DICT, Area, Node
from .tables import SwitchTable
from .forms import  SwitchSearchForm, SwitchNewForm
from .views import nodeview

@nodeview.route('/nodes/switches.xls/', methods=['POST', 'GET'])
@nodeview.route('/nodes/switches/', methods=['POST', 'GET'])
def switches():
    form = SwitchSearchForm()
    query = NodeSwitch.query.outerjoin(Area, NodeSwitch.area_id==Area.id)

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
    if not current_user.is_province_user: query = query.filter(current_user.domain.clause_permit)
    form.process(**query_dict)
    table = make_table(query, SwitchTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeSwitch.query.filter(NodeSwitch.status == status)
        if not current_user.is_province_user: num = num.outerjoin(Area, NodeSwitch.area_id==Area.id).filter(current_user.domain.clause_permit)
        num = num.count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})

    if request.base_url.endswith(".xls/"):
        csv = XlsExport('switches',columns=NodeSwitch.export_columns())
        return send_file(csv.export(query,format={'status': lambda value: NODE_STATUS_DICT.get(value)}),as_attachment=True,attachment_filename='switches.xls')
    else:
        return render_template('/nodes/switches/index.html', table = table, form=form, status_statistcs=status_statistcs)

@nodeview.route('/nodes/switches/new/', methods=['GET','POST'])
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
        elif Node.query.filter(Node.addr==node.addr).count() > 0:
            flash(u'IP地址不能重复','error')
        else:
            node.status = 1
            node.category_id = 2
            db.session.add(node)
            db.session.commit()
            flash(u'添加交换机 %s 成功'% node.name, 'success')
            return redirect(url_for('nodes.switches'))
    return render_template('nodes/switches/new.html', form = form, next=next)

@nodeview.route('/nodes/switches/edit/<int:id>/', methods=['POST', 'GET'])
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
            elif node.addr != form.addr.data and Node.query.filter(Node.addr==form.addr.data).count() > 0:
                flash(u'IP地址不能重复','error')
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
def switches_show(id):
    node = NodeSwitch.query.get(id)
    if node is None:
        return render_template('/nodes/not_exist.html', menuid='switches', message=u'交换机不存在，可能已经被删除',title=u'交换机')
    data = [{'label': u'完全故障', 'color': 'red', 'value': 1},
            {'label': u'部分故障', 'color': 'yellow', 'value': 2},
            {'label': u'完全正常', 'color': 'green', 'value': 19},
            {'label': u'数据缺失', 'color': 'blue', 'value': 2}]
    chartdata2 = [{'values': data}]
    return render_template('nodes/switches/show.html', node = node, chartdata2 = chartdata2)

import os
from flask import Markup
from werkzeug import secure_filename
@nodeview.route('/nodes/switches/import/', methods=['POST'])
def switches_import():
    if request.method == 'POST':
        file = request.files['file']
        if file and file.filename.endswith('xls'):
            filename = secure_filename(file.filename)
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','static','file','upload')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file_path = os.path.join(root_path, filename.split('.')[0]+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.xls')
            file.save(file_path)
            from tango.excel import SwitchImport
            reader = SwitchImport(engine=db.session.bind)
            info = reader.read(file=file_path, data_dict={'entrance_name':current_user.domain.import_permit(4)})
            flash(Markup(info), 'success')
        else:
            flash(u"上传文件格式错误", 'error')
    return redirect(url_for('nodes.switches'))
