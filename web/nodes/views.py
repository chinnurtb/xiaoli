# coding: utf-8

from datetime import datetime

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash
from flask import json, send_file

from sqlalchemy import or_

from tango import db,user_profile
from tango.ui import navbar, dashboard
from tango.ui.tables import make_table
from tango.login import current_user, login_required
from tango.models import Profile, Category
from tango.excel.CsvExport import CsvExport

from .models import Node, Area, Vendor, NODE_STATUS_DICT
from .forms import NodeNewForm, NodeSearchForm
from .tables import NodeTable

nodeview = Blueprint('nodes', __name__)

@nodeview.context_processor
def inject_navid():
    return dict(navid = 'nodes')

from .views_router import routers, routers_new, routers_edit, routers_show, routers_delete
from .views_switch import switches, switches_new, switches_edit, switches_show, switches_delete
from .views_olt import olts, olts_new, olts_edit, olts_delete, olts_show
from .views_onu import onus, onus_new, onus_edit, onus_delete, onus_show
from .views_eoc import eocs, eocs_new, eocs_edit, eocs_delete, eocs_show
from .views_cpe import cpes, cpes_new, cpes_edit, cpes_delete, cpes_show
from .views_statistics import areas, vendors, categories
from .views_area import cities

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
@nodeview.route('/nodes.csv/', methods=['POST', 'GET'])
@nodeview.route('/nodes/', methods=['POST', 'GET'])
@login_required
def nodes():
    form = NodeSearchForm()

    # 节点检索
    query = Node.query
    query = query.outerjoin(Area, Node.area_id==Area.id)
    query_dict = dict([(key, request.args.get(key))for key in form.data.keys()])
    if query_dict.get("keyword"):
        query=query.filter(or_(
            Node.name.like('%'+query_dict["keyword"]+'%'),
            Node.alias.like('%'+query_dict["keyword"]+'%'),
            Node.addr.like('%'+query_dict["keyword"]+'%')
        ))
    if query_dict.get("area"):
        # 区域树查询，是直接用的前台传过来的值作为where条件，如果包含or，需加括号
        # 注意：值如（areas.cityid=1001 or areas.town=1006），areas 应与实际上生成的sql语句一致
        netloc = request.args.get('area_netloc')
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)
    if query_dict.get("vendor_id"): query=query.filter(Node.vendor_id == query_dict["vendor_id"]) # ==
    if query_dict.get("model_id"): query=query.filter(Node.model_id == query_dict["model_id"])    # ==
    if query_dict.get("category_id"): query=query.filter(Node.category_id == query_dict["category_id"])
    if query_dict.get("status"): query=query.filter(Node.status == query_dict["status"])
    form.process(**query_dict)
    table = make_table(query, NodeTable)

    # 节点状态统计
    status_statistcs = []
    for status in NODE_STATUS_DICT.keys():
        num = Node.query.filter(Node.status == status).count()
        status_statistcs.append({"status": status, "number": num, "name": NODE_STATUS_DICT.get(status)})

    if request.base_url.endswith(".csv/"):
        csv = CsvExport('nodes',columns=Node.export_columns())
        return send_file(csv.export(query,format={'status': lambda value: NODE_STATUS_DICT.get(value)}),as_attachment=True,attachment_filename='nodes.csv')
    else:
        return render_template('nodes/index.html', table = table, form=form, status_statistcs=status_statistcs)


@nodeview.route('/nodes/<int:id>/', methods=['GET'])
@login_required
def nodes_show(id):
    node = Node.query.get_or_404(id)
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
    return render_template('nodes/show.html', node = node, traffic_chart = traffic_chart, alarm_chart = alarm_chart)

@nodeview.route('/nodes/new/', methods=['GET','POST'])
@login_required
def nodes_new():
    form = NodeNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        node = Node()
        form.populate_obj(node)
        node.status = 1
        db.session.add(node)
        db.session.commit()
        flash(u'添加节点成功', 'success')
        return redirect(url_for('nodes.nodes'))
    return render_template('nodes/new.html', form = form)

@nodeview.route('/nodes/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def nodes_edit(id):
    form = NodeNewForm()
    node = Node.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        form.populate_obj(node)
        node.updated_at = datetime.now()
        db.session.add(node)
        db.session.commit()
        flash(u'修改节点成功','success')
        return redirect(url_for('nodes.nodes'))
    form.process(obj=node)
    return render_template('/nodes/edit.html', node=node, form=form)

@nodeview.route('/nodes/delete/', methods=['POST'])
def nodes_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            node = Node.query.get(id)
            db.session.delete(node)
        db.session.commit()
        flash(u'删除节点成功','success')
        return redirect(url_for('nodes.nodes'))

@nodeview.route('/nodes/ajax_models_for_vendor', methods=['GET'])
def ajax_models_for_vendor():
    vendor_id = request.args.get('key')
    models = Vendor.query.get(vendor_id).models
    return json.dumps([{'value':model.id, 'name':model.alias} for model in models])

@nodeview.route('/nodes/ajax_towns_for_city', methods=['GET'])
def ajax_towns_for_city():
    cityid = request.args.get('key')
    towns = Area.query.filter(Area.cityid==cityid).filter(Area.area_type==2)
    return json.dumps([{'value':town.id, 'name':town.alias} for town in towns])

@nodeview.route('/nodes/ajax_branches_for_town', methods=['GET'])
def ajax_branches_for_town():
    town = request.args.get('key')
    branches = Area.query.filter(Area.town==town).filter(Area.area_type==3)
    return json.dumps([{'value':branch.id, 'name':branch.alias} for branch in branches])

@nodeview.route('/nodes/ajax_entrances_for_branch', methods=['GET'])
def ajax_entrances_for_branch():
    branch = request.args.get('key')
    entrances = Area.query.filter(Area.branch==branch).filter(Area.area_type==4)
    return json.dumps([{'value':entrance.id, 'name':entrance.alias} for entrance in entrances])

@nodeview.route('/nodes/ajax_entrances_for_town', methods=['GET'])
def ajax_entrances_for_town():
    town = request.args.get('key')
    entrances = Area.query.filter(Area.town==town).filter(Area.area_type==4)
    return json.dumps([{'value':entrance.id, 'name':entrance.alias} for entrance in entrances])


@nodeview.route('/nodes/import', methods=['GET'])
@login_required
def nodes_import():
    return render_template('/nodes/import.html')

@nodeview.route('/managers', methods=['GET'])
def managers():
    return render_template('/managers/index.html')

navbar.add('nodes', u'资源', 'tasks', '/nodes')
