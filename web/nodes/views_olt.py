#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json

from tango import db,user_profile
from tango.ui.tables import make_table
from tango.login import current_user, login_required
from tango.models import Profile, Category

from .models import NodeOlt,NODE_STATUS_DICT, Area
from .tables import OltTable
from .forms import  OltSearchForm, OltNewForm
from .views import nodeview

@nodeview.route('/nodes/olts/', methods=['POST', 'GET'])
@login_required
def olts():
    form = OltSearchForm()
    query = NodeOlt.query
    query = query.outerjoin(Area, NodeOlt.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("ip"): query=query.filter(NodeOlt.addr.like('%'+query_dict["ip"]+'%'))         # ilike
    if query_dict.get("name"): query=query.filter(NodeOlt.name.like('%'+query_dict["name"]+'%'))     # ilike
    if query_dict.get("area"):
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(NodeOlt.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(NodeOlt.model_id == query_dict["model_id"])    # ==
    if query_dict.get("status"): query=query.filter(NodeOlt.status == query_dict["status"])
    form.process(**query_dict)
    table = make_table(query, OltTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeOlt.query.filter(NodeOlt.status == status).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})
    return render_template('/nodes/olts/index.html', table = table, form=form, status_statistcs=status_statistcs)

@nodeview.route('/nodes/olts/new/', methods=['GET','POST'])
@login_required
def olts_new():
    form = OltNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        node = NodeOlt()
        form.populate_obj(node)
        node.status = 1
        node.category_id = 20
        db.session.add(node)
        db.session.commit()
        flash(u'添加OLT成功', 'info')
        return redirect(url_for('nodes.olts'))
    return render_template('nodes/olts/new.html', form = form)

@nodeview.route('/nodes/olts/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def olts_edit(id):
    form = OltNewForm()
    node = NodeOlt.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        form.populate_obj(node)
        node.updated_at = datetime.now()
        db.session.add(node)
        db.session.commit()
        flash(u'修改OLT成功','info')
        return redirect(url_for('nodes.olts'))
    form.process(obj=node)
    return render_template('/nodes/olts/edit.html', node=node, form=form)

@nodeview.route('/nodes/olts/delete/', methods=['POST'])
def olts_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            node = NodeOlt.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除OLT成功','info')
        return redirect(url_for('nodes.olts'))

@nodeview.route('/nodes/olts/<int:id>/', methods=['GET'])
@login_required
def olts_show(id):
    node = NodeOlt.query.get_or_404(id)
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
    return render_template('nodes/olts/show.html', node = node, traffic_chart = traffic_chart, alarm_chart = alarm_chart)
