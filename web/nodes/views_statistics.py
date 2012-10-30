#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json

from sqlalchemy import func
from sqlalchemy import or_
from sqlalchemy.orm import aliased

from tango import db
from tango import user_profile
from tango.ui import navbar, dashboard
from tango.ui.tables import make_table
from tango.login import current_user, login_required
from tango.models import Profile, Category

from .models import Node,AREA_TYPE_DICT, Area, Vendor, NODE_STATUS_DICT
from .tables import AreaStatisticsTable,VendorTable,CategoryTable
from .views import nodeview
from .forms import AreaStatisticsForm

@nodeview.route("/nodes/statistics/areas/")
@login_required
def areas():
    form = AreaStatisticsForm()

    netloc = request.args.get('area_netloc')    # 区域过滤条件
    query_gran = request.args.get('query_gran',1)   # 统计粒度
    group_type = AREA_TYPE_DICT.get(int(query_gran))    # 分组group_by类型，下面的子查询语句的字段将会根据它动态构造

    # 构造各个分类统计的子查询
    sub_query_list = []
    categories = Category.query.filter(Category.is_valid == 1).filter(Category.obj=='node')
    for index,category in [(0,'total')]+[(category.id,category.name) for category in categories]:
        sub_query = db.session.query(
            getattr(Area,group_type),func.count(Node.id).label(category+"_count")
        ).select_from(Node).outerjoin(
            Area, Node.area_id==Area.id
        )
        if netloc:
            if 'or' in netloc: netloc = '('+netloc+')'
            sub_query = sub_query.filter(netloc)
        if index == 0: # 统计总节点数的子查询
            sub_query = sub_query.group_by(getattr(Area,group_type)).subquery()
        else:
            sub_query = sub_query.filter(Node.category_id==index).group_by(getattr(Area,group_type)).subquery()
        sub_query_list.append(sub_query)

    # 连接各个子查询
    query = 'db.session.query(Area.id,Area.name,Area.area_type,'
    for index,category in enumerate(['total']+[category.name for category in categories]):
        query += 'func.coalesce(sub_query_list[%(index)s].c.%(category)s_count,0).label("%(category)s_count"),' % {'index':index,'category': category}
    query += ')'
    query = eval(query)
    for index,sub_query in enumerate(sub_query_list):
        query = query.outerjoin(sub_query, getattr(sub_query.c,group_type)==getattr(Area, group_type))
    query = query.filter(Area.area_type==query_gran)
    if netloc:
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)

    # 隐藏is_valid = 0 的分类
    hiddens = u','.join([category.name+'_count' for category in Category.query.filter(Category.obj=='node').filter(Category.is_valid!=1)])
    profile = {"table.nodes.hiddens":hiddens}
    table = make_table(query, AreaStatisticsTable,profile)
    form.process(query_gran=query_gran)
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        return render_template('nodes/statistics/area_statistics.html', table = table, form = form)

@nodeview.route("/nodes/statistics/vendors/")
@login_required
def vendors():
    table = make_table(Vendor.query, VendorTable)
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        from tango.ui.charts.highcharts import BarStacked
        chart = BarStacked()
        xAxis_categories = [row["alias"] for row in table.rows]
        name_dict = {
            table.columns[2].name: table.columns[2].header,
            table.columns[3].name: table.columns[3].header,
            table.columns[4].name: table.columns[4].header,
            table.columns[5].name: table.columns[5].header,
            }
        series = [{"name": name_dict[name], "data": [ row[name] for row in table.rows ]} for name in name_dict.keys() ]
        chart.set_colors(['red', 'green'])
        chart["series"] = series
        chart["xAxis"]["categories"] = xAxis_categories
        chart["title"]["text"] = u"资源厂商统计"
        chart["yAxis"]["title"] = None
        chart.height = str(len(xAxis_categories)*50 + 100)+"px"
        return render_template('nodes/statistics/vendor_statistics.html', table = table, chart = chart)

@nodeview.route("/nodes/statistics/categories/")
@login_required
def categories():
    query_total = db.session.query(
        Node.category_id,func.count(Node.category_id).label("total_count")
    ).group_by(Node.category_id).subquery()

    query_status1 = db.session.query(
        Node.category_id,func.count(Node.category_id).label("status1_count")
    ).filter(Node.status==1).group_by(Node.category_id).subquery()

    query_status2 = db.session.query(
        Node.category_id,func.count(Node.category_id).label("status2_count")
    ).filter(Node.status==2).group_by(Node.category_id).subquery()

    query_status3 = db.session.query(
        Node.category_id,func.count(Node.category_id).label("status3_count")
    ).filter(Node.status==3).group_by(Node.category_id).subquery()

    query_status4 = db.session.query(
        Node.category_id,func.count(Node.category_id).label("status4_count")
    ).filter(Node.status==4).group_by(Node.category_id).subquery()

    query = db.session.query(
        Category.id, Category.alias.label("category_name"),
        func.coalesce(query_total.c.total_count,0).label("total_count"),
        func.coalesce(query_status1.c.status1_count,0).label("status1_count"),
        func.coalesce(query_status2.c.status2_count,0).label("status2_count"),
        func.coalesce(query_status3.c.status3_count,0).label("status3_count"),
        func.coalesce(query_status4.c.status4_count,0).label("status4_count"),
    ).outerjoin(
        query_total, query_total.c.category_id==Category.id
    ).outerjoin(
        query_status1,Category.id==query_status1.c.category_id
    ).outerjoin(
        query_status2,Category.id==query_status2.c.category_id
    ).outerjoin(
        query_status3,Category.id==query_status3.c.category_id
    ).outerjoin(
        query_status4,Category.id==query_status4.c.category_id
    ).filter(Category.obj == "node").filter(Category.is_valid == 1)

    table = make_table(query, CategoryTable)
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        from tango.ui.charts.highcharts import BarStacked
        chart = BarStacked()
        xAxis_categories = [row["category_name"] for row in table.rows]
        name_dict = {
            table.columns[2].name: table.columns[2].header,
            table.columns[3].name: table.columns[3].header,
            table.columns[4].name: table.columns[4].header,
            table.columns[5].name: table.columns[5].header,
        }
        series = [{"name": name_dict[name], "data": [ row[name] for row in table.rows ]} for name in name_dict.keys() ]
        chart.set_colors(['red', 'green'])
        chart["series"] = series
        chart["xAxis"]["categories"] = xAxis_categories
        chart["title"]["text"] = u"资源分类统计"
        chart["yAxis"]["title"] = None
        chart.height = str(len(xAxis_categories)*50 + 100)+"px"
        return render_template('nodes/statistics/category_statistics.html', table = table, chart = chart)

dashboard.add_widget('category_statistic', u'分类统计', url='/nodes/statistics/categories/?dashboard=true', column = 'side')
dashboard.add_widget('vendor_statistic', u'厂商统计', url='/nodes/statistics/vendors/?dashboard=true', column = 'side')
dashboard.add_widget('area_statistic', u'区域统计',url='/nodes/statistics/areas/?dashboard=true', column = 'side')
