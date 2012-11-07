#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json, send_file

from sqlalchemy import or_

from tango import db,user_profile
from tango.ui.tables import make_table
from tango.login import current_user, login_required
from tango.models import Profile, Category
from tango.excel.CsvExport import CsvExport

from .models import NodeEoc,NodeCpe,NODE_STATUS_DICT, Area
from .tables import CpeTable
from .forms import  CpeSearchForm, CpeNewForm
from .views import nodeview

@nodeview.route('/nodes/cpes.csv/', methods=['POST', 'GET'])
@nodeview.route('/nodes/cpes/', methods=['POST', 'GET'])
@login_required
def cpes():
    form = CpeSearchForm()
    query = NodeCpe.query
    query = query.outerjoin(Area, NodeCpe.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("keyword"):
        query=query.filter(or_(
            NodeCpe.name.like('%'+query_dict["keyword"]+'%'),
            NodeCpe.alias.like('%'+query_dict["keyword"]+'%'),
            NodeCpe.addr.like('%'+query_dict["keyword"]+'%')
        ))
    if query_dict.get("area"):
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(NodeCpe.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(NodeCpe.model_id == query_dict["model_id"])    # ==
    if query_dict.get("status"): query=query.filter(NodeCpe.status == query_dict["status"])
    form.process(**query_dict)
    table = make_table(query, CpeTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeCpe.query.filter(NodeCpe.status == status).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})

    if request.base_url.endswith(".csv/"):
        csv = CsvExport('cpes',columns=NodeCpe.export_columns())
        return send_file(csv.export(query,format={'status': lambda value: NODE_STATUS_DICT.get(value)}),as_attachment=True,attachment_filename='cpes.csv')
    else:
        return render_template('/nodes/cpes/index.html', table = table, form=form, status_statistcs=status_statistcs)

@nodeview.route('/nodes/cpes/new/', methods=['GET','POST'])
@login_required
def cpes_new():
    form = CpeNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        node = NodeCpe()
        form.populate_obj(node)
        node.status = 1
        node.category_id = 51
        db.session.add(node)
        db.session.commit()
        flash(u'添加CPE成功', 'success')
        return redirect(url_for('nodes.cpes'))
    return render_template('nodes/cpes/new.html', form = form)

@nodeview.route('/nodes/cpes/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def cpes_edit(id):
    form = CpeNewForm()
    node = NodeCpe.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(node)
        node.updated_at = datetime.now()
        db.session.add(node)
        db.session.commit()
        flash(u'修改CPE成功','success')
        return redirect(url_for('nodes.cpes'))
    form.process(obj=node)
    return render_template('/nodes/cpes/edit.html', node=node, form=form)

@nodeview.route('/nodes/cpes/delete/', methods=['POST'])
def cpes_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            node = NodeCpe.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除CPE成功','success')
        return redirect(url_for('nodes.cpes'))

@nodeview.route('/nodes/cpes/<int:id>/', methods=['GET'])
@login_required
def cpes_show(id):
    node = NodeCpe.query.get_or_404(id)
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
    return render_template('nodes/cpes/show.html', node = node, traffic_chart = traffic_chart, alarm_chart = alarm_chart)

@nodeview.route('/nodes/cpes/ajax_entrances_for_eoc', methods=['GET'])
def ajax_entrances_for_eoc():
    eoc_id = request.args.get('key')
    eoc = NodeEoc.query.get(eoc_id)
    entrances = Area.query.filter(Area.parent_id==eoc.area_id)
    return json.dumps([{'value':entrance.id, 'name':entrance.alias} for entrance in entrances])