#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json

from tango import db
from tango import user_profile
from tango.base import make_table
from tango.ui import menus, Menu
from tango.ui import add_widget, Widget, tables
from tango.login import current_user, login_required
from tango.models import Profile, Category

from .models import NodeOnu, NODE_STATUS_DICT, Area, NodeOlt
from .tables import OnuTable
from .forms import  OnuSearchForm, OnuNewForm
from .views import nodeview

@nodeview.route('/nodes/onus/', methods=['GET'])
@login_required
def onus():
    form = OnuSearchForm()
    query = NodeOnu.query
    query = query.outerjoin(Area, NodeOnu.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("ip"): query=query.filter(NodeOnu.addr.like('%'+query_dict["ip"]+'%'))         # ilike
    if query_dict.get("name"): query=query.filter(NodeOnu.name.like('%'+query_dict["name"]+'%'))     # ilike
    if query_dict.get("area"):
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(NodeOnu.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(NodeOnu.model_id == query_dict["model_id"])    # ==
    if request.args.get("olt_id"): query=query.filter(NodeOnu.controller_id == request.args["olt_id"])
    form.process(**query_dict)
    table = make_table(query, OnuTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeOnu.query.filter(NodeOnu.status == status).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})
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
        flash(u'新建ONU成功', 'info')
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
        flash(u'修改ONU成功','info')
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
        flash(u'删除ONU成功','info')
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
    alarm_chart["title"]["text"] = None
    alarm_chart["plotOptions"]["pie"]["events"]["click"] = None
    alarm_chart.min_width = str(220)+"px"
    return render_template('nodes/onus/show.html', node = node, traffic_chart = traffic_chart, alarm_chart = alarm_chart)

@nodeview.route('/nodes/onus/ajax_entrances_for_olt', methods=['GET'])
def ajax_entrances_for_olt():
    olt_id = request.args.get('key')
    olt = NodeOlt.query.get(olt_id)
    entrances = Area.query.filter(Area.parent_id==olt.area_id)
    return json.dumps([{'value':entrance.id, 'name':entrance.alias} for entrance in entrances])