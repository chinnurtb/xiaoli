#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json, send_file

from sqlalchemy import or_

from tango import db,get_profile
from tango.ui.tables import make_table
from tango.login import current_user, login_required
from tango.models import Profile, Category
from tango.excel.CsvExport import CsvExport

from .models import NodeEoc,NODE_STATUS_DICT, Area
from .tables import EocTable
from .forms import  EocSearchForm, EocNewForm
from .views import nodeview

@nodeview.route('/nodes/eocs.csv/', methods=['POST', 'GET'])
@nodeview.route('/nodes/eocs/', methods=['POST', 'GET'])
@login_required
def eocs():
    form = EocSearchForm()
    query = NodeEoc.query
    query = query.outerjoin(Area, NodeEoc.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("keyword"):
        query=query.filter(or_(
            NodeEoc.name.like('%'+query_dict["keyword"]+'%'),
            NodeEoc.alias.like('%'+query_dict["keyword"]+'%'),
            NodeEoc.addr.like('%'+query_dict["keyword"]+'%')
        ))
    if query_dict.get("area"):
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(NodeEoc.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(NodeEoc.model_id == query_dict["model_id"])    # ==
    if query_dict.get("status"): query=query.filter(NodeEoc.status == query_dict["status"])
    form.process(**query_dict)
    table = make_table(query, EocTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeEoc.query.filter(NodeEoc.status == status).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})

    if request.base_url.endswith(".csv/"):
        csv = CsvExport('eocs',columns=NodeEoc.export_columns())
        return send_file(csv.export(query,format={'status': lambda value: NODE_STATUS_DICT.get(value)}),as_attachment=True,attachment_filename='eocs.csv')
    else:
        return render_template('/nodes/eocs/index.html', table = table, form=form, status_statistcs=status_statistcs)

@nodeview.route('/nodes/eocs/new/', methods=['GET','POST'])
@login_required
def eocs_new():
    form = EocNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        node = NodeEoc()
        form.populate_obj(node)
        node.status = 1
        node.category_id = 50
        db.session.add(node)
        db.session.commit()
        flash(u'添加EOC成功', 'success')
        return redirect(url_for('nodes.eocs'))
    return render_template('nodes/eocs/new.html', form = form)

@nodeview.route('/nodes/eocs/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def eocs_edit(id):
    form = EocNewForm()
    node = NodeEoc.query.get_or_404(id)
    if request.method == 'POST':
        if form.validate_on_submit():
            del form._fields["cityid"]
            del form._fields["town"]
            form.populate_obj(node)
            node.updated_at = datetime.now()
            db.session.add(node)
            db.session.commit()
            flash(u'修改EOC成功','success')
            return redirect(url_for('nodes.eocs'))
    else:
        form.process(obj=node)
    return render_template('/nodes/eocs/edit.html', node=node, form=form)

@nodeview.route('/nodes/eocs/delete/', methods=['POST'])
def eocs_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            node = NodeEoc.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除EOC成功','success')
        return redirect(url_for('nodes.eocs'))

@nodeview.route('/nodes/eocs/<int:id>/', methods=['GET'])
@login_required
def eocs_show(id):
    node = NodeEoc.query.get_or_404(id)
    from tango.ui.charts.highcharts import LineTimeSeriesChart
    traffic_chart = LineTimeSeriesChart()
    traffic_chart.set_html_id("traffic")
    traffic_chart["title"]["text"] = None
    traffic_chart["subtitle"]["text"] = None
    traffic_chart["yAxis"]["title"] = None
    #traffic_chart.height = str(250)+"px"
    traffic_chart.set_yformatter()

    from tango.ui.charts.highcharts import PieBasicChart
    alarm_chart = PieBasicChart()
    alarm_chart.set_html_id("alarm")
    alarm_chart["title"]["text"] = u'最近24小时可用率'
    alarm_chart["plotOptions"]["pie"]["events"]["click"] = None
    alarm_chart.min_width = str(220)+"px"
    alarm_chart["series"][0]["data"] = [{'name': u'完全故障', 'y':1},{'name': u'部分故障', 'y':2},{'name': u'完全正常', 'y':19},{'name': u'数据缺失', 'y':2}]
    return render_template('nodes/eocs/show.html', node = node, traffic_chart = traffic_chart, alarm_chart = alarm_chart)

import os
import operator
from flask import Markup
from werkzeug import secure_filename
from tango.excel.CsvImport import CsvImport,ImportColumn
@nodeview.route('/nodes/eocs/import/', methods=['POST'])
@login_required
def eocs_import():
    if request.method == 'POST':
        file = request.files['file']
        if file and file.filename.endswith('csv'):
            filename = secure_filename(file.filename)
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','static','file','upload')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file_path = os.path.join(root_path, filename.split('.')[0]+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
            file.save(file_path)
            reader = CsvImport(session=db.session.bind, table='node_eocs')
            reader.addColumn(
                ImportColumn(u'名称', 'name', 'character varying(40)')
            ).addColumn(
                ImportColumn(u'别名', 'alias', 'character varying(200)')
            ).addColumn(
                ImportColumn(u'节点类型', 'category_id', 'integer', default=50)
            ).addColumn(
                ImportColumn(u'IP地址', 'addr', 'character varying(200)', is_key=True)
            ).addColumn(
                ImportColumn(u'所属区域', 'area_id', 'integer',allow_null=False, existed_data=dict([(area.full_name, area.id) for area in Area.query.filter(Area.area_type==3)]), )
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
            for node in NodeEoc.query.all():
                f = operator.attrgetter(*key_list)
                key = (f(node),) if len(key_list) == 1 else f(node)
                f2 = operator.attrgetter(*attr_list)
                update_dict[key] = dict(zip(attr_list, f2(node)))
            reader.load_update(permit_update_dict=update_dict, update_dict=update_dict)
            info = reader.read(file=file_path)
            flash(Markup(info), 'success')
        else:
            flash(u"上传文件格式错误", 'error')
    return redirect(url_for('nodes.eocs'))
