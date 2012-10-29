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

from .models import NodeRouter,NODE_STATUS_DICT, Area
from .tables import RouterTable
from .forms import  RouterSearchForm, RouterNewForm
from .views import nodeview

@nodeview.route('/nodes/routers/', methods=['POST', 'GET'])
@login_required
def routers():
    form = RouterSearchForm()
    query = NodeRouter.query
    query = query.outerjoin(Area, NodeRouter.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("ip"): query=query.filter(NodeRouter.addr.like('%'+query_dict["ip"]+'%'))         # ilike
    if query_dict.get("name"): query=query.filter(NodeRouter.name.like('%'+query_dict["name"]+'%'))     # ilike
    if query_dict.get("area"):
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(NodeRouter.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(NodeRouter.model_id == query_dict["model_id"])    # ==
    form.process(**query_dict)
    table = make_table(query, RouterTable)

    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = NodeRouter.query.filter(NodeRouter.status == status).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})
    return render_template('/nodes/routers/index.html', table = table, form=form, status_statistcs=status_statistcs)

@nodeview.route('/nodes/routers/new/', methods=['GET','POST'])
@login_required
def routers_new():
    form = RouterNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        node = NodeRouter()
        form.populate_obj(node)
        node.status = 1
        node.category_id = 1
        db.session.add(node)
        db.session.commit()
        flash(u'新建路由器成功', 'info')
        return redirect(url_for('nodes.routers'))
    return render_template('nodes/routers/new.html', form = form)

@nodeview.route('/nodes/routers/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def routers_edit(id):
    form = RouterNewForm()
    node = NodeRouter.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        form.populate_obj(node)
        node.updated_at = datetime.now()
        db.session.add(node)
        db.session.commit()
        flash(u'修改路由器成功','info')
        return redirect(url_for('nodes.routers'))
    form.process(obj=node)
    return render_template('/nodes/routers/edit.html', node=node, form=form)

@nodeview.route('/nodes/routers/delete/', methods=['POST'])
def routers_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            node = NodeRouter.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除路由器成功','info')
        return redirect(url_for('nodes.routers'))

@nodeview.route('/nodes/routers/<int:id>/', methods=['GET'])
@login_required
def routers_show(id):
    node = NodeRouter.query.get_or_404(id)
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
    return render_template('nodes/routers/show.html', node = node, traffic_chart = traffic_chart, alarm_chart = alarm_chart)
