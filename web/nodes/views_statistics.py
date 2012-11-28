#!/usr/bin/env python
# coding: utf-8
from datetime import datetime

from flask import Blueprint, request, session, url_for,\
    redirect, render_template, g, flash
from flask import json

from sqlalchemy import func

from tango import db,get_profile
from tango.ui import navbar, dashboard, Dashboard
from tango.ui.tables import make_table
from tango.login import current_user
from tango.models import Profile, Category

from .models import Node,AREA_TYPE_DICT, Area, Vendor, NODE_STATUS_DICT,NODE_STATUS_COLOR
from .tables import AreaStatisticsTable,VendorTable,CategoryTable
from .views import nodeview
from .forms import AreaStatisticsForm

@nodeview.route("/nodes/statistics/areas/")
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
def vendors():
    query = db.session.query(func.count(Node.id), Node.status, Vendor.id, Vendor.alias)
    query = query.outerjoin(Vendor, Vendor.id==Node.vendor_id)
    query = query.group_by(Node.status, Vendor.id, Vendor.alias).order_by(Vendor.id)
    rows = {(u"总数",u"总数"): {}}
    for count, status, vendor_id, vendor_name in query.all():
        row = rows.get((vendor_id, vendor_name), {"total":0})
        row[status] = count
        row["total"] += count
        rows[(u"总数",u"总数")][status] = rows[(u"总数",u"总数")].get(status,0)+count
        rows[(u"总数",u"总数")]["total"] = rows[(u"总数",u"总数")].get("total",0)+count
        rows[(vendor_id, vendor_name)] = row
    if request.args.get("dashboard"):
        return render_template('nodes/statistics/_vendors.html', NODE_STATUS_DICT = NODE_STATUS_DICT, rows = rows)
    else:
        def series(status):
            values = [{'series': status[1],'x': vendor[1] if vendor[1] else u'未知','y': row_dict.get(status[0],0)} for vendor, row_dict in sorted(rows.items(), key=lambda d:d[0])]
            return {'key': status[1], 'color': NODE_STATUS_COLOR.get(status[0]), 'values': values}
        data = [series(status) for status in NODE_STATUS_DICT.items()]
        return render_template('nodes/statistics/vendors.html', NODE_STATUS_DICT = NODE_STATUS_DICT, rows = rows,
            chartid = "category_vendors_chart",chartdata = data,)

@nodeview.route("/nodes/statistics/categories/")
def categories():
    query = db.session.query(func.count(Node.id), Node.status, Category.id, Category.alias)
    query = query.outerjoin(Category, Category.id==Node.category_id)
    query = query.group_by(Node.status, Category.id, Category.alias).order_by(Category.id)
    rows = {(u"总数",u"总数"): {}}
    for count, status, category_id, category_name in query.all():
        row = rows.get((category_id, category_name), {"total":0})
        row[status] = count
        row["total"] += count
        rows[(u"总数",u"总数")][status] = rows[(u"总数",u"总数")].get(status,0)+count
        rows[(u"总数",u"总数")]["total"] = rows[(u"总数",u"总数")].get("total",0)+count
        rows[(category_id, category_name)] = row
    if request.args.get("dashboard"):
        return render_template('nodes/statistics/_categories.html', NODE_STATUS_DICT = NODE_STATUS_DICT, rows = rows)
    else:
        def series(status):
            values = [{'series': status[1],'x': category[1],'y': row_dict.get(status[0],0)} for category, row_dict in sorted(rows.items(), key=lambda d:d[0])]
            return {'key': status[1], 'color':NODE_STATUS_COLOR.get(status[0]), 'values': values}
        data = [series(status) for status in NODE_STATUS_DICT.items()]
        return render_template('nodes/statistics/categories.html', NODE_STATUS_DICT = NODE_STATUS_DICT, rows = rows,
            chartid = "category_vendors_chart",chartdata = data,)

@nodeview.route("/nodes/statistics/category_vendors/")
def category_vendors():
    vendors = Vendor.query.filter(Vendor.is_valid==1).order_by(Vendor.id).all()
    query = db.session.query(func.count(Node.id), Vendor.id, Category.id, Category.alias)
    query = query.outerjoin(Category, Category.id==Node.category_id)
    query = query.outerjoin(Vendor, Vendor.id==Node.vendor_id)
    query = query.group_by(Vendor.id, Category.id, Category.alias).order_by(Category.id)
    rows = {(u"总数",u"总数"): {}}
    for count, vendor, category_id, category_name in query.all():
        row = rows.get((category_id, category_name), {"total":0})
        row[vendor] = count
        row["total"] += count
        rows[(u"总数",u"总数")][vendor] = rows[(u"总数",u"总数")].get(vendor,0)+count
        rows[(u"总数",u"总数")]["total"] = rows[(u"总数",u"总数")].get("total",0)+count
        rows[(category_id, category_name)] = row
    if request.args.get("dashboard"):
        return render_template('nodes/statistics/_category_vendors.html', vendors = vendors, rows = rows)
    else:
        def series(vendor):
            values = [{'series': vendor.alias,'x': category[1],'y': row_dict.get(vendor.id,0)} for category, row_dict in sorted(rows.items(), key=lambda d:d[0])]
            return {'key': vendor.alias,'values': values}
        data = [series(vendor) for vendor in vendors]
        data.append({'key': u'未知', 'values': [{'series': u'未知','x': category[1],'y': row_dict.get(None,0)} for category, row_dict in rows.items()]})
        return render_template('nodes/statistics/category_vendors.html', vendors = vendors, rows = rows,
            chartid = "category_vendors_chart",chartdata = data,)

dashboard.add_widget('category_statistic', u'分类统计', url='/nodes/statistics/categories/?dashboard=true', column = 'side')
dashboard.add_widget('vendor_statistic', u'厂商统计', url='/nodes/statistics/vendors/?dashboard=true', column = 'side')
dashboard.add_widget('area_statistic', u'区域统计',url='/nodes/statistics/areas/?dashboard=true', column = 'side')
dashboard.add_widget('category_vendors', u'设备分类厂商统计',url='/nodes/statistics/category_vendors/?dashboard=true', column = 'side')

