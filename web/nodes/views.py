#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from sqlalchemy import func
from sqlalchemy.orm import aliased

from tango import db
from tango.ui import menus, Menu
from tango.ui import add_widget, Widget, tables
from tango.login import current_user, login_required
from tango.models import Profile

from .models import Node, Board, Port, Area, Vendor
from .forms import NodeNewForm, NodeSearchForm
from .tables import NodeTable,PortTable,BoardTable,AreaTable,VendorTable,CategoryTable

nodeview = Blueprint('nodes', __name__)

@nodeview.route('/nodes/')
@login_required
def nodes():
    form = NodeSearchForm()
    query = Node.query
    query_dict = dict([(key, request.args.get(key))for key,value in form.data.items()])
    if query_dict.get("ip"): query=query.filter(Node.addr.like('%'+query_dict["ip"]+'%'))
    if query_dict.get("name"): query=query.filter(Node.name.like('%'+query_dict["name"]+'%'))
    if query_dict.get("area_id"): query=query.filter(Node.area_id == query_dict["area_id"])
    if query_dict.get("vendor_id"): query=query.filter(Node.vendor_id == query_dict["vendor_id"])
    if query_dict.get("model_id"): query=query.filter(Node.model_id == query_dict["model_id"])
    form.process(**query_dict)
    profile = Profile.load(current_user.id, NodeTable._meta.profile_grp)
    table = NodeTable(query).configure(profile)
    return render_template('nodes/index.html', table = table, form=form)

@nodeview.route('/nodes/<int:id>/', methods=['GET'])
@login_required
def node_show(id):
    Node = Node.query.get_or_404(id)
    return render_template('nodes/show.html', node = node)

@nodeview.route('/nodes/new/', methods=['GET','POST'])
@login_required
def node_new():
    form = NodeNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        node = Node()
        form.populate_obj(node)
        node.status = 0
        db.session.add(node)
        db.session.commit()
        flash(u'新建节点成功', 'info')
        return redirect(url_for('nodes.nodes'))
    return render_template('nodes/new.html', form = form)

@nodeview.route('/nodes/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def node_edit(id):
    form = NodeNewForm()
    node = Node.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(node)
        db.session.add(node)
        db.session.commit()
        flash(u'修改节点成功','info')
        return redirect(url_for('nodes.nodes'))

    form.process(obj=node)
    return render_template('/nodes/edit.html', node=node, form=form)

@nodeview.route('/users/delete/', methods=['POST'])
def node_delete():
    if request.method == 'POST':
        ids = request.form.getlist('ids')
        for id in ids:
            node = Node.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除节点成功','info')
        return redirect(url_for('nodes.nodes'))

@nodeview.route("/boards/")
@login_required
def boards():
    profile = Profile.load(current_user.id, BoardTable._meta.profile_grp)
    table = BoardTable(Board.query).configure(profile)
    return render_template('boards/index.html', table = table)

@nodeview.route("/ports/")
@login_required
def ports():
    profile = Profile.load(current_user.id, PortTable._meta.profile_grp)
    table = PortTable(Port.query).configure(profile)
    return render_template('ports/index.html', table = table)

@nodeview.route("/areas/")
@login_required
def areas():
    base = request.args.get("base")
    if base:
        base = Area.query.get(base)
    else:
        base = Area.query.filter(Area.area_type == 0).first()
    area_type_dict = {1:"cityid",2:"town",3:"branch",4:"entrance"}
    area_type = area_type_dict.get(base.area_type + 1)  # 区域类型，下面的子查询语句的字段将会根据它动态构造
    sub_query_list = []
    for index,category in enumerate(['total','olt','onu','dslam','eoc','switch']):
        sub_query = db.session.query(
            getattr(Area,area_type),func.count(Node.id).label(category+"_count")
        ).select_from(Node).outerjoin(
            Area, Node.area_id==Area.id
        )
        if index == 0:
            sub_query = sub_query.group_by(getattr(Area,area_type)).subquery()
        else:
            sub_query = sub_query.filter(Node.category==index).group_by(getattr(Area,area_type)).subquery()
        sub_query_list.append(sub_query)
    query = db.session.query(
        Area.id,Area.name,Area.area_type,
        func.coalesce(sub_query_list[0].c.total_count,0).label("total_count"),
        func.coalesce(sub_query_list[1].c.olt_count,0).label("olt_count"),
        func.coalesce(sub_query_list[2].c.onu_count,0).label("onu_count"),
        func.coalesce(sub_query_list[3].c.dslam_count,0).label("dslam_count"),
        func.coalesce(sub_query_list[4].c.eoc_count,0).label("eoc_count"),
        func.coalesce(sub_query_list[5].c.switch_count,0).label("switch_count")
    )
    for sub_query in sub_query_list:
        query = query.outerjoin(sub_query, getattr(sub_query.c,area_type)==Area.id)
    query = query.filter(Area.parent_id==base.id)

    table = AreaTable(query).configure({})
    breadcrumb = [base]
    while base.parent:
        breadcrumb.append(base.parent)
        base = base.parent

    return render_template('nodes/area_statistics.html', table = table, breadcrumb = breadcrumb)

@nodeview.route("/vendors/")
@login_required
def vendors():
    profile = Profile.load(current_user.id, VendorTable._meta.profile_grp)
    query = Vendor.query
    table = VendorTable(query).configure(profile)
    return render_template('nodes/vendor_statistics.html', table = table)

@nodeview.route("/categories/")
@login_required
def categories():
    profile = Profile.load(current_user.id, CategoryTable._meta.profile_grp)
    query_total = db.session.query(
        Node.category,func.count(Node.category).label("total_count")
    ).group_by(Node.category).subquery()

    query_status0 = db.session.query(
        Node.category,func.count(Node.category).label("status0_count")
    ).filter(Node.status==0).group_by(Node.category).subquery()

    query_status1 = db.session.query(
        Node.category,func.count(Node.category).label("status1_count")
    ).filter(Node.status==1).group_by(Node.category).subquery()

    query = db.session.query(
        query_total.c.category.label("category_name"),
        func.coalesce(query_total.c.total_count,0).label("total_count"),
        func.coalesce(query_status0.c.status0_count,0).label("status0_count"),
        func.coalesce(query_status1.c.status1_count,0).label("status1_count")
    ).outerjoin(
        query_status0,query_total.c.category==query_status0.c.category
    ).outerjoin(query_status1,query_total.c.category==query_status1.c.category)

    table = CategoryTable(query).configure(profile)
    return render_template('nodes/category_statistics.html', table = table)

menus.append(Menu('nodes', u'资源', '/nodes'))

#col2
add_widget(Widget('category_statistic', u'分类统计', content='<div style="height:100px">Dashboard3</div>', column = 'side'))
add_widget(Widget('vendor_statistic', u'厂商统计', content='<div style="height:100px">Dashboard4</div>', column = 'side'))
add_widget(Widget('area_statistic', u'区域统计', content='<div style="height:100px">Dashboard5</div>', column = 'side'))

