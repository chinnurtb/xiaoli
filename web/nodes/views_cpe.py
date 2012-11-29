#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json, send_file

from sqlalchemy import or_

from tango import db,get_profile
from tango.ui.tables import make_table
from tango.login import current_user
from tango.models import Profile, Category
from tango.excel.CsvExport import CsvExport

from .models import NodeEoc,NodeCpe,NODE_STATUS_DICT, Area
from .tables import CpeTable
from .forms import  CpeSearchForm, CpeNewForm
from .views import nodeview

@nodeview.route('/nodes/cpes.csv/', methods=['POST', 'GET'])
@nodeview.route('/nodes/cpes/', methods=['POST', 'GET'])
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
    query = query.filter(Area.id.in_(current_user.domain.area_ids(4)))
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
def cpes_new():
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = CpeNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        node = NodeCpe()
        form.populate_obj(node)
        if NodeCpe.query.filter(NodeCpe.name==node.name).count() > 0:
            flash(u'CPE名称不能重复','error')
        elif NodeCpe.query.filter(NodeCpe.alias==node.alias).count() > 0:
            flash(u'CPE别名不能重复','error')
        elif NodeCpe.query.filter(NodeCpe.mac==node.mac).count() > 0:
            flash(u'CPE MAC地址不能重复','error')
        else:
            node.status = 1
            node.category_id = 51
            db.session.add(node)
            db.session.commit()
            flash(u'添加CPE %s 成功'% node.name, 'success')
            return redirect(url_for('nodes.cpes'))
    return render_template('nodes/cpes/new.html', form = form, next=next)

@nodeview.route('/nodes/cpes/edit/<int:id>/', methods=['POST', 'GET'])
def cpes_edit(id):
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = CpeNewForm()
    node = NodeCpe.query.get_or_404(id)
    if request.method == 'POST':
        if form.validate_on_submit():
            if node.name != form.name.data and NodeCpe.query.filter(NodeCpe.name==form.name.data).count() > 0:
                flash(u'CPE名称不能重复','error')
            elif node.alias != form.alias.data and NodeCpe.query.filter(NodeCpe.alias==form.alias.data).count() > 0:
                flash(u'CPE别名不能重复','error')
            elif node.mac != form.mac.data and NodeCpe.query.filter(NodeCpe.mac==form.mac.data).count() > 0:
                flash(u'CPE MAC地址不能重复','error')
            else:
                form.populate_obj(node)
                node.updated_at = datetime.now()
                db.session.add(node)
                db.session.commit()
                flash(u'修改CPE %s 成功'% node.name,'success')
                return redirect(url_for('nodes.cpes'))
    else:
        form.process(obj=node)
    return render_template('/nodes/cpes/edit.html', node=node, form=form, next=next)

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
def cpes_show(id):
    node = NodeCpe.query.get_or_404(id)
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
    return render_template('nodes/cpes/show.html', node = node, chartdata = chartdata, chartdata2 = chartdata2)

@nodeview.route('/nodes/cpes/ajax_entrances_for_eoc', methods=['GET'])
def ajax_entrances_for_eoc():
    eoc_id = request.args.get('key')
    eoc = NodeEoc.query.get(eoc_id)
    entrances = Area.query.filter(Area.parent_id==eoc.area_id)
    return json.dumps([{'value':entrance.id, 'name':entrance.alias} for entrance in entrances])
