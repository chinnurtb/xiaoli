#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json

from tango import db
from tango import user_profile
from tango.ui.tables import make_table
from tango.login import current_user, login_required
from tango.models import Profile, Category

from .models import NodeSwitch,NODE_STATUS_DICT, Area
from .tables import SwitchTable
from .forms import  SwitchSearchForm, SwitchNewForm
from .views import nodeview

@nodeview.route('/nodes/switches/', methods=['POST', 'GET'])
@login_required
def switches():
    form = SwitchSearchForm()
    query = NodeSwitch.query
    query = query.outerjoin(Area, NodeSwitch.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("ip"): query=query.filter(NodeSwitch.addr.like('%'+query_dict["ip"]+'%'))         # ilike
    if query_dict.get("name"): query=query.filter(NodeSwitch.name.like('%'+query_dict["name"]+'%'))     # ilike
    if query_dict.get("area"):
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(NodeSwitch.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(NodeSwitch.model_id == query_dict["model_id"])    # ==
    form.process(**query_dict)
    table = make_table(query, SwitchTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeSwitch.query.filter(NodeSwitch.status == status).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})
    return render_template('/nodes/switches/index.html', table = table, form=form, status_statistcs=status_statistcs)

@nodeview.route('/nodes/switches/new/', methods=['GET','POST'])
@login_required
def switches_new():
    form = SwitchNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        node = NodeSwitch()
        form.populate_obj(node)
        node.status = 1
        node.category_id = 2
        db.session.add(node)
        db.session.commit()
        flash(u'新建交换机成功', 'info')
        return redirect(url_for('nodes.switches'))
    return render_template('nodes/switches/new.html', form = form)

@nodeview.route('/nodes/switches/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def switches_edit(id):
    form = SwitchNewForm()
    node = NodeSwitch.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        form.populate_obj(node)
        node.updated_at = datetime.now()
        db.session.add(node)
        db.session.commit()
        flash(u'修改交换机成功','info')
        return redirect(url_for('nodes.switches'))
    form.process(obj=node)
    return render_template('/nodes/switches/edit.html', node=node, form=form)

@nodeview.route('/nodes/switches/delete/', methods=['POST'])
def switches_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            node = NodeSwitch.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除交换机成功','info')
        return redirect(url_for('nodes.switches'))

@nodeview.route('/nodes/switches/<int:id>/', methods=['GET'])
@login_required
def switches_show(id):
    node = NodeSwitch.query.get_or_404(id)
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
    return render_template('nodes/switches/show.html', node = node, traffic_chart = traffic_chart, alarm_chart = alarm_chart)
