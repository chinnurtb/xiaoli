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
from tango.excelRW.CsvWriter import CsvWriter

from .models import NodeOnu, NODE_STATUS_DICT, Area, NodeOlt
from .tables import OnuTable
from .forms import  OnuSearchForm, OnuNewForm
from .views import nodeview

@nodeview.route('/nodes/onus.csv/', methods=['GET'])
@nodeview.route('/nodes/onus/', methods=['GET'])
@login_required
def onus():
    form = OnuSearchForm()
    query = NodeOnu.query
    query = query.outerjoin(Area, NodeOnu.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("name"):
        query=query.filter(or_(
            NodeOnu.name.like('%'+query_dict["name"]+'%'),
            NodeOnu.alias.like('%'+query_dict["name"]+'%'),
            NodeOnu.addr.like('%'+query_dict["name"]+'%')
        ))
    if query_dict.get("area"):
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(NodeOnu.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(NodeOnu.model_id == query_dict["model_id"])    # ==
    if request.args.get("olt_id"): query=query.filter(NodeOnu.controller_id == request.args["olt_id"])
    if query_dict.get("status"): query=query.filter(NodeOnu.status == query_dict["status"])
    form.process(**query_dict)
    table = make_table(query, OnuTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeOnu.query.filter(NodeOnu.status == status).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})

    if request.base_url.endswith(".csv/"):
        writer = CsvWriter('onus',columns=NodeOnu.export_columns())
        return send_file(writer.write(query,format={'status': lambda value: NODE_STATUS_DICT.get(value)}),as_attachment=True,attachment_filename='onus.csv')
    else:
        return render_template('/nodes/onus/index.html', table = table, form=form, status_statistcs=status_statistcs)

@nodeview.route('/nodes/onus/new/', methods=['GET','POST'])
@login_required
def onus_new():
    form = OnuNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        node = NodeOnu()
        form.populate_obj(node)
        node.status = 1
        node.category_id = 21
        db.session.add(node)
        db.session.commit()
        flash(u'添加ONU成功', 'success')
        return redirect(url_for('nodes.onus'))
    return render_template('nodes/onus/new.html', form = form)

@nodeview.route('/nodes/onus/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def onus_edit(id):
    form = OnuNewForm()
    node = NodeOnu.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(node)
        node.updated_at = datetime.now()
        db.session.add(node)
        db.session.commit()
        flash(u'修改ONU成功','success')
        return redirect(url_for('nodes.onus'))
    form.process(obj=node)
    return render_template('/nodes/onus/edit.html', node=node, form=form)

@nodeview.route('/nodes/onus/delete/', methods=['POST'])
def onus_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            node = NodeOnu.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除ONU成功','success')
        return redirect(url_for('nodes.onus'))

@nodeview.route('/nodes/onus/<int:id>/', methods=['GET'])
@login_required
def onus_show(id):
    node = NodeOnu.query.get_or_404(id)
    from tango.ui.charts.highcharts import LineTimeSeriesChart
    traffic_chart = LineTimeSeriesChart()
    traffic_chart.set_html_id("traffic")
    traffic_chart["title"]["text"] = None
    traffic_chart["subtitle"]["text"] = None
    traffic_chart["yAxis"]["title"] = None
    traffic_chart.set_yformatter()

    from tango.ui.charts.highcharts import PieBasicChart
    alarm_chart = PieBasicChart()
    alarm_chart.set_html_id("alarm")
    alarm_chart["title"]["text"] = u'最近24小时可用率'
    alarm_chart["plotOptions"]["pie"]["events"]["click"] = None
    alarm_chart.min_width = str(220)+"px"
    alarm_chart["series"][0]["data"] = [{'name': u'完全故障', 'y':1},{'name': u'部分故障', 'y':2},{'name': u'完全正常', 'y':19},{'name': u'数据缺失', 'y':2}]
    return render_template('nodes/onus/show.html', node = node, traffic_chart = traffic_chart, alarm_chart = alarm_chart)

@nodeview.route('/nodes/onus/ajax_entrances_for_olt', methods=['GET'])
def ajax_entrances_for_olt():
    olt_id = request.args.get('key')
    olt = NodeOlt.query.get(olt_id)
    entrances = Area.query.filter(Area.parent_id==olt.area_id)
    return json.dumps([{'value':entrance.id, 'name':entrance.alias} for entrance in entrances])
