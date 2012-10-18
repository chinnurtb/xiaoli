#!/usr/bin/env python
# coding: utf-8

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash
from flask import json

from sqlalchemy import func
from sqlalchemy import or_
from sqlalchemy.orm import aliased

from tango import db
from tango import user_profile
from tango.base import make_table
from tango.ui import menus, Menu
from tango.ui import add_widget, Widget, tables
from tango.login import current_user, login_required
from tango.models import Profile

from .models import Node, Board, Port, Area, Vendor
from .forms import NodeNewForm, NodeSearchForm
from .tables import NodeTable,PortTable,BoardTable,AreaTable,VendorTable,CategoryTable

nodeview = Blueprint('nodes', __name__)

area_type = {0:'areas.province',1:'areas.cityid',2:'areas.town',3:'areas.branch',4:'areas.entrance'}

@nodeview.route('/area_select', methods=['POST', 'GET'])
def area_select():
    key = request.args.get('key')
    area_selected = [int(id) for id in request.args.get("selected_ids",'').split(',') if id ]
    # 获得树需要展开的所有节点
    expand_nodes = set(area_selected)
    if area_selected and not key:
        expand_areas = [Area.query.get(id) for id in area_selected]
        for area in expand_areas:
            while area.parent_id != -1:
                expand_nodes.add(area.parent_id)
                area = Area.query.get(area.parent_id)

    def make_node(area):
        node = {}
        node['title'] = area.name
        node['key'] = str(area.id)
        node['area_type'] = area_type[area.area_type]
        if area.id in area_selected:
            node['select'] = True
        if len(area.children) > 0:
            node['isLazy'] = True
        return node

    def make_nodes(area):
        node = make_node(area)
        if len(area.children) > 0:
            node['expand'] = False
            node['children'] = []
            for child in area.children:
                if child.id in expand_nodes:
                    node['expand'] = True
                    node['children'].append(make_nodes(child))
                else:
                    child_node = make_node(child)
                    node['children'].append(child_node)
        return node

    if key:
        trees = [make_node(area) for area in Area.query.filter(Area.parent_id==key)]
    else:
        # 依次找到当前用户的所有管理域，判断从属关系后合并，最后生成树
        city_ids = current_user.domain.city_list.split(',') if current_user.domain.city_list else []
        town_ids = current_user.domain.town_list.split(',') if current_user.domain.town_list else []
        branch_ids = current_user.domain.branch_list.split(',') if current_user.domain.branch_list else []
        entrance_ids = current_user.domain.entrance_list.split(',') if current_user.domain.entrance_list else []
        city_nodes = [Area.query.get(city) for city in city_ids]
        town_nodes = [Area.query.get(town) for town in town_ids]
        branch_nodes = [Area.query.get(branch) for branch in branch_ids]
        entrance_nodes = [Area.query.get(entrance) for entrance in entrance_ids]
        area_nodes = city_nodes
        area_nodes.extend([area for area in town_nodes if str(area.cityid) not in city_ids])
        area_nodes.extend([area for area in branch_nodes if str(area.cityid) not in city_ids
            and str(area.town) not in town_ids])
        area_nodes.extend([make_node(area) for area in entrance_nodes if str(area.cityid) not in city_ids
            and str(area.town) not in town_ids
            and str(area.branch) not in branch_ids])

        trees = [make_nodes(area) for area in area_nodes]

    return json.dumps(trees)


from tango.ui.queries import NodeForm
@nodeview.route('/nodes/', methods=['POST', 'GET'])
@login_required
def nodes():
    form = NodeSearchForm()
    query_form = NodeForm()
    if request.method == 'POST':
        print 'query_form.filters_str::', query_form.filters_str
    
    query = Node.query
    query = query.outerjoin(Area, Node.area_id==Area.id)

    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("ip"): query=query.filter(Node.addr.like('%'+query_dict["ip"]+'%'))         # ilike
    if query_dict.get("name"): query=query.filter(Node.name.like('%'+query_dict["name"]+'%'))     # ilike
    if query_dict.get("area"):
        # 区域树查询，是直接用的前台传过来的值作为where条件，如果包含or，需加括号
        # 注意：值如（areas.cityid=1001 or areas.town=1006），areas 应与实际上生成的sql语句一致
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(Node.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(Node.model_id == query_dict["model_id"])    # ==
    form.process(**query_dict)
    if request.method == 'POST':
        query = Node.query.filter(query_form.filters_str)
    table = make_table(query, NodeTable)
    return render_template('nodes/index.html', table = table, form=form, query_form=query_form)

@nodeview.route('/nodes/<int:id>/', methods=['GET'])
@login_required
def node_show(id):
    node = Node.query.get_or_404(id)
    from tango.ui.charts.highcharts import LineTimeSeriesChart
    traffic_chart = LineTimeSeriesChart()
    traffic_chart.set_html_id("traffic")
    traffic_chart["title"]["text"] = None
    traffic_chart["subtitle"]["text"] = None
    traffic_chart["yAxis"]["title"] = None
    traffic_chart.height = str(250)+"px"
    traffic_chart.set_yformatter()

    from tango.ui.charts.highcharts import PieBasicChart
    alarm_chart = PieBasicChart()
    alarm_chart.set_html_id("alarm")
    alarm_chart["title"]["text"] = None
    alarm_chart["plotOptions"]["pie"]["events"]["click"] = None
    alarm_chart.height = str(220)+"px"
    alarm_chart.width = str(220)+"px"
    alarm_chart.min_width = str(220)+"px"
    return render_template('nodes/show.html', node = node, traffic_chart = traffic_chart, alarm_chart = alarm_chart)

@nodeview.route('/nodes/new/', methods=['GET','POST'])
@login_required
def nodes_new():
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
def nodes_edit(id):
    form = NodeNewForm()
    node = Node.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(node)
        db.session.add(node)
        db.session.commit()
        flash(u'修改节点成功','info')
        return redirect(url_for('nodes.nodes'))
    node.area_id = {
        'area_value':node.area.name,
        'netloc_value':area_type[node.area.area_type]+'='+str(node.area.id),
        'selected_value':node.area.id
    }
    form.process(obj=node)
    return render_template('/nodes/edit.html', node=node, form=form)

@nodeview.route('/users/delete/', methods=['POST'])
def nodes_delete():
    if request.method == 'POST':
        ids = request.form.getlist('ids')
        for id in ids:
            node = Node.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除节点成功','info')
        return redirect(url_for('nodes.nodes'))

@nodeview.route('/nodes/import', methods=['GET'])
@login_required
def nodes_import():
    return render_template('/nodes/import.html')


@nodeview.route('/managers', methods=['GET'])
@login_required
def managers():
    return render_template('/managers/index.html')


@nodeview.route("/boards/")
def boards():
    table = make_table(Board.query, BoardTable)
    return render_template('boards/index.html', table = table)

@nodeview.route("/ports/")
@login_required
def ports():
    table = make_table(Port.query, PortTable)
    return render_template('ports/index.html', table = table)

@nodeview.route("/areas/")
@login_required
def areas():
    base = request.args.get("base")     # 所统计的区域
    base = Area.query.get(base) if base else Area.query.filter(Area.area_type == 0).first()
    query_gran = request.args.get("query_gran")     # 查询粒度，控制table中列的显示
    query_gran = (base.area_type + 1) if not query_gran else int(query_gran)
    if query_gran == 2:
        profile = {"table.nodes.hiddens":"town_count"}
    elif query_gran == 3:
        profile = {"table.nodes.hiddens":"town_count,branch_count"}
    elif query_gran == 4:
        profile = {"table.nodes.hiddens":"town_count,branch_count,entrance_count"}
    else:
        profile = {}

    # 构造各个统计的子查询
    area_type_dict = {1:"cityid",2:"town",3:"branch",4:"entrance"}
    group_type = area_type_dict.get(query_gran)    # 分组类型，下面的子查询语句的字段将会根据它动态构造

    sub_query_list = []
    for index,category in enumerate(['total','olt','onu','dslam','eoc','switch']):
        sub_query = db.session.query(
            getattr(Area,group_type),func.count(Node.id).label(category+"_count")
        ).select_from(Node).outerjoin(
            Area, Node.area_id==Area.id
        )
        if index == 0:
            sub_query = sub_query.group_by(getattr(Area,group_type)).subquery()
        else:
            sub_query = sub_query.filter(Node.category==index).group_by(getattr(Area,group_type)).subquery()
        sub_query_list.append(sub_query)

    for index,gran in enumerate(['town','branch','entrance']):
        sub_query = db.session.query(
            getattr(Area,group_type), func.count(Area.id).label(gran+"_count")
        ).filter(
            Area.area_type==(index+2)
        ).group_by(getattr(Area,group_type)).subquery()
        sub_query_list.append(sub_query)
    # 连接各个子查询
    query = db.session.query(
        Area.id,Area.name,Area.area_type,
        func.coalesce(sub_query_list[0].c.total_count,0).label("total_count"),
        func.coalesce(sub_query_list[1].c.olt_count,0).label("olt_count"),
        func.coalesce(sub_query_list[2].c.onu_count,0).label("onu_count"),
        func.coalesce(sub_query_list[3].c.dslam_count,0).label("dslam_count"),
        func.coalesce(sub_query_list[4].c.eoc_count,0).label("eoc_count"),
        func.coalesce(sub_query_list[5].c.switch_count,0).label("switch_count"),
        func.coalesce(sub_query_list[6].c.town_count,0).label("town_count"),
        func.coalesce(sub_query_list[7].c.branch_count,0).label("branch_count"),
        func.coalesce(sub_query_list[8].c.entrance_count,0).label("entrance_count")
    )
    for index,sub_query in enumerate(sub_query_list):
        query = query.outerjoin(sub_query, getattr(sub_query.c,group_type)==Area.id)
    query = query.filter(Area.area_type==query_gran)
    if query_gran != 1:
        query = query.filter(getattr(Area,area_type_dict[base.area_type])==base.id)

    table = make_table(query, AreaTable, profile=profile)
    breadcrumb = [base]
    while base.parent:
        breadcrumb.append(base.parent)
        base = base.parent
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        return render_template('nodes/area_statistics.html', table = table, breadcrumb = breadcrumb)

@nodeview.route("/vendors/")
@login_required
def vendors():
    table = make_table(Vendor.query, VendorTable)
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        from tango.ui.charts.highcharts import BarStacked
        chart = BarStacked()
        xAxis_categories = [row["alias"] for row in table.rows]
        name_dict = {table.columns[2].name: table.columns[2].header, table.columns[3].name: table.columns[3].header}
        series = [{"name": name_dict[name], "data": [ row[name] for row in table.rows ]} for name in name_dict.keys() ]
        chart.set_colors(['red', 'green'])
        chart["series"] = series
        chart["xAxis"]["categories"] = xAxis_categories
        chart["title"]["text"] = u"资源厂商统计"
        chart["yAxis"]["title"] = None
        chart.height = str(len(xAxis_categories)*50 + 100)+"px"
        return render_template('nodes/vendor_statistics.html', table = table, chart = chart)

@nodeview.route("/categories/")
@login_required
def categories():
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

    table = make_table(query, CategoryTable)
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        from tango.ui.charts.highcharts import BarStacked
        chart = BarStacked()
        xAxis_categories = [row["category_name"] for row in table.rows]
        name_dict = {table.columns[2].name: table.columns[2].header, table.columns[3].name: table.columns[3].header}
        series = [{"name": name_dict[name], "data": [ row[name] for row in table.rows ]} for name in name_dict.keys() ]
        chart.set_colors(['red', 'green'])
        chart["series"] = series
        chart["xAxis"]["categories"] = xAxis_categories
        chart["title"]["text"] = u"资源分类统计"
        chart["yAxis"]["title"] = None
        chart.height = str(len(xAxis_categories)*50 + 100)+"px"
        return render_template('nodes/category_statistics.html', table = table, chart = chart)

menus.append(Menu('nodes', u'资源', '/nodes'))

#col2
add_widget(Widget('category_statistic', u'分类统计', url='/categories/?dashboard=true', column = 'side'))
add_widget(Widget('vendor_statistic', u'厂商统计', url='/vendors/?dashboard=true', column = 'side'))
add_widget(Widget('area_statistic', u'区域统计',url='/areas/?dashboard=true', column = 'side'))
