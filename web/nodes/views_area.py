# coding: utf-8

from datetime import datetime

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash,json

from sqlalchemy import func,or_
from sqlalchemy.orm import aliased

from tango import db, get_profile
from tango.ui import navbar, dashboard
from tango.ui.tables import make_table
from tango.login import current_user, login_required
from tango.models import Profile, Category

from .models import Node, Board, Port, Area, Vendor, NODE_STATUS_DICT, Model
from .tables import CityTable, TownTable, BranchTable, EntranceTable
from .forms import CityNewForm, TownNewForm, BranchNewForm, EntranceNewForm
from .views import nodeview


@nodeview.route("/nodes/cities/")
@login_required
def cities():
    # 构造各个统计的子查询
    sub_query_list = []
    categories = Category.query.filter(Category.is_valid == 1).filter(Category.obj=='node')
    for index,category in [(0,'total')]+[(category.id,category.name) for category in categories]:
        sub_query = db.session.query(
            Area.cityid,func.count(Node.id).label(category+"_count")
        ).select_from(Node).outerjoin(
            Area, Node.area_id==Area.id
        )
        if index == 0: # 统计总节点数的子查询
            sub_query = sub_query.group_by(Area.cityid).subquery()
        else:
            sub_query = sub_query.filter(Node.category_id==index).group_by(Area.cityid).subquery()
        sub_query_list.append(sub_query)

    for index,gran in enumerate(['town','branch','entrance']):
        sub_query = db.session.query(
            Area.cityid, func.count(Area.id).label(gran+"_count")
        ).filter(
            Area.area_type==(index+2)
        ).group_by(Area.cityid).subquery()
        sub_query_list.append(sub_query)

    # 连接各个子查询
    query = 'db.session.query(Area.id,Area.name,Area.area_type,'
    for index,category in enumerate(['total']+[category.name for category in categories]):
        query += 'func.coalesce(sub_query_list[%(index)s].c.%(category)s_count,0).label("%(category)s_count"),' % {'index':index,'category': category}
    query += 'func.coalesce(sub_query_list[-3].c.town_count,0).label("town_count"),'
    query += 'func.coalesce(sub_query_list[-2].c.branch_count,0).label("branch_count"),'
    query += 'func.coalesce(sub_query_list[-1].c.entrance_count,0).label("entrance_count"),'
    query += ')'
    query = eval(query)
    for index,sub_query in enumerate(sub_query_list):
        query = query.outerjoin(sub_query, sub_query.c.cityid==Area.id)
    query = query.filter(Area.area_type==1)

    # 隐藏is_valid = 0 的分类
    hiddens = u','.join([category.name+'_count' for category in Category.query.filter(Category.obj=='node').filter(Category.is_valid!=1)])
    profile = {"table.areas.hiddens":hiddens}
    table = make_table(query, CityTable,profile)
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        return render_template('nodes/areas/cities.html', table = table)

@nodeview.route("/nodes/towns/")
@login_required
def towns():
    netloc = request.args.get('area_netloc')    # 区域过滤条件
    # 构造各个统计的子查询
    sub_query_list = []
    categories = Category.query.filter(Category.is_valid == 1).filter(Category.obj=='node')
    for index,category in [(0,'total')]+[(category.id,category.name) for category in categories]:
        sub_query = db.session.query(
            Area.town,func.count(Node.id).label(category+"_count")
        ).select_from(Node).outerjoin(
            Area, Node.area_id==Area.id
        )
        if index == 0: # 统计总节点数的子查询
            sub_query = sub_query.group_by(Area.town).subquery()
        else:
            sub_query = sub_query.filter(Node.category_id==index).group_by(Area.town).subquery()
        sub_query_list.append(sub_query)

    for index,gran in enumerate(['branch','entrance']):
        sub_query = db.session.query(
            Area.town, func.count(Area.id).label(gran+"_count")
        ).filter(
            Area.area_type==(index+3)
        ).group_by(Area.town).subquery()
        sub_query_list.append(sub_query)

    # 连接各个子查询
    query = 'db.session.query(Area.id,Area.name,Area.area_type,'
    for index,category in enumerate(['total']+[category.name for category in categories]):
        query += 'func.coalesce(sub_query_list[%(index)s].c.%(category)s_count,0).label("%(category)s_count"),' % {'index':index,'category': category}
    query += 'func.coalesce(sub_query_list[-2].c.branch_count,0).label("branch_count"),'
    query += 'func.coalesce(sub_query_list[-1].c.entrance_count,0).label("entrance_count"),'
    query += ')'
    query = eval(query)
    for index,sub_query in enumerate(sub_query_list):
        query = query.outerjoin(sub_query, sub_query.c.town==Area.id)
    query = query.filter(Area.area_type==2)
    if netloc:
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)

    # 隐藏is_valid = 0 的分类
    hiddens = u','.join([category.name+'_count' for category in Category.query.filter(Category.obj=='node').filter(Category.is_valid!=1)])
    profile = {"table.areas.hiddens":hiddens}
    table = make_table(query, TownTable,profile)
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        return render_template('nodes/areas/towns.html', table = table)

@nodeview.route("/nodes/branches/")
@login_required
def branches():
    netloc = request.args.get('area_netloc')    # 区域过滤条件
    # 构造各个统计的子查询
    sub_query_list = []
    categories = Category.query.filter(Category.is_valid == 1).filter(Category.obj=='node')
    for index,category in [(0,'total')]+[(category.id,category.name) for category in categories]:
        sub_query = db.session.query(
            Area.branch,func.count(Node.id).label(category+"_count")
        ).select_from(Node).outerjoin(
            Area, Node.area_id==Area.id
        )
        if index == 0: # 统计总节点数的子查询
            sub_query = sub_query.group_by(Area.branch).subquery()
        else:
            sub_query = sub_query.filter(Node.category_id==index).group_by(Area.branch).subquery()
        sub_query_list.append(sub_query)

    for index,gran in enumerate(['entrance']):
        sub_query = db.session.query(
            Area.branch, func.count(Area.id).label(gran+"_count")
        ).filter(
            Area.area_type==(index+4)
        ).group_by(Area.branch).subquery()
        sub_query_list.append(sub_query)

    # 连接各个子查询
    query = 'db.session.query(Area.id,Area.name,Area.area_type,'
    for index,category in enumerate(['total']+[category.name for category in categories]):
        query += 'func.coalesce(sub_query_list[%(index)s].c.%(category)s_count,0).label("%(category)s_count"),' % {'index':index,'category': category}
    query += 'func.coalesce(sub_query_list[-1].c.entrance_count,0).label("entrance_count"),'
    query += ')'
    query = eval(query)
    for index,sub_query in enumerate(sub_query_list):
        query = query.outerjoin(sub_query, sub_query.c.branch==Area.id)
    query = query.filter(Area.area_type==3)
    if netloc:
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)

    # 隐藏is_valid = 0 的分类
    hiddens = u','.join([category.name+'_count' for category in Category.query.filter(Category.obj=='node').filter(Category.is_valid!=1)])
    profile = {"table.areas.hiddens":hiddens}
    table = make_table(query, BranchTable,profile)
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        return render_template('nodes/areas/branches.html', table = table)

@nodeview.route("/nodes/entrances/")
@login_required
def entrances():
    netloc = request.args.get('area_netloc')    # 区域过滤条件
    # 构造各个统计的子查询
    sub_query_list = []
    categories = Category.query.filter(Category.is_valid == 1).filter(Category.obj=='node')
    for index,category in [(0,'total')]+[(category.id,category.name) for category in categories]:
        sub_query = db.session.query(
            Area.entrance,func.count(Node.id).label(category+"_count")
        ).select_from(Node).outerjoin(
            Area, Node.area_id==Area.id
        )
        if index == 0: # 统计总节点数的子查询
            sub_query = sub_query.group_by(Area.entrance).subquery()
        else:
            sub_query = sub_query.filter(Node.category_id==index).group_by(Area.entrance).subquery()
        sub_query_list.append(sub_query)

    # 连接各个子查询
    query = 'db.session.query(Area.id,Area.name,Area.area_type,'
    for index,category in enumerate(['total']+[category.name for category in categories]):
        query += 'func.coalesce(sub_query_list[%(index)s].c.%(category)s_count,0).label("%(category)s_count"),' % {'index':index,'category': category}
    query += ')'
    query = eval(query)
    for index,sub_query in enumerate(sub_query_list):
        query = query.outerjoin(sub_query, sub_query.c.entrance==Area.id)
    query = query.filter(Area.area_type==4)
    if netloc:
        if 'or' in netloc: netloc = '('+netloc+')'
        query = query.filter(netloc)

    # 隐藏is_valid = 0 的分类
    hiddens = u','.join([category.name+'_count' for category in Category.query.filter(Category.obj=='node').filter(Category.is_valid!=1)])
    profile = {"table.areas.hiddens":hiddens}
    table = make_table(query, EntranceTable,profile)
    if request.args.get("dashboard"):
        return table.as_html()
    else:
        return render_template('nodes/areas/entrances.html', table = table)

@nodeview.route('/nodes/cities/new/', methods=['GET','POST'])
@login_required
def cities_new():
    form = CityNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        area = Area()
        form.populate_obj(area)
        area.area_type = 1
        area.parent_id = Area.query.filter(Area.area_type==0).first().id
        db.session.add(area)
        db.session.commit()
        flash(u'添加地市成功', 'success')
        return redirect(url_for('nodes.cities'))
    return render_template('nodes/areas/cities_new.html', form = form)

@nodeview.route('/nodes/cities/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def cities_edit(id):
    form = CityNewForm()
    area = Area.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(area)
        area.updated_at = datetime.now()
        db.session.add(area)
        db.session.commit()
        flash(u'修改地市成功','success')
        return redirect(url_for('nodes.cities'))
    form.process(obj=area)
    return render_template('/nodes/areas/cities_edit.html', area=area, form=form)

@nodeview.route('/nodes/cities/delete/', methods=['POST'])
def cities_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            area = Area.query.get(id)
            if len(area.children) > 0:
                flash(u'删除地市失败，请先删除其所有区县', 'error')
                return redirect(url_for('nodes.cities'))
            db.session.delete(area)
        db.session.commit()
        flash(u'删除地市成功','success')
        return redirect(url_for('nodes.cities'))

@nodeview.route('/nodes/towns/new/', methods=['GET','POST'])
@login_required
def towns_new():
    form = TownNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        area = Area()
        form.populate_obj(area)
        area.area_type = 2
        db.session.add(area)
        db.session.commit()
        flash(u'添加区县成功', 'success')
        return redirect(url_for('nodes.towns'))
    return render_template('nodes/areas/towns_new.html', form = form)

@nodeview.route('/nodes/towns/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def towns_edit(id):
    form = TownNewForm()
    area = Area.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(area)
        area.updated_at = datetime.now()
        db.session.add(area)
        db.session.commit()
        flash(u'修改区县成功','success')
        return redirect(url_for('nodes.towns'))
    form.process(obj=area)
    return render_template('/nodes/areas/towns_edit.html', area=area, form=form)

@nodeview.route('/nodes/towns/delete/', methods=['POST'])
def towns_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            area = Area.query.get(id)
            if len(area.children) > 0:
                flash(u'删除区县失败，请先删除其所有分局', 'error')
                return redirect(url_for('nodes.towns'))
            db.session.delete(area)
        db.session.commit()
        flash(u'删除区县成功','success')
        return redirect(url_for('nodes.towns'))

@nodeview.route('/nodes/branches/new/', methods=['GET','POST'])
@login_required
def branches_new():
    form = BranchNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        area = Area()
        form.populate_obj(area)
        area.area_type = 3
        db.session.add(area)
        db.session.commit()
        flash(u'添加分局成功', 'success')
        return redirect(url_for('nodes.branches'))
    return render_template('nodes/areas/branches_new.html', form = form)

@nodeview.route('/nodes/branches/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def branches_edit(id):
    form = BranchNewForm()
    area = Area.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        form.populate_obj(area)
        area.updated_at = datetime.now()
        db.session.add(area)
        db.session.commit()
        flash(u'修改分局成功','success')
        return redirect(url_for('nodes.branches'))
    form.process(obj=area)
    return render_template('/nodes/areas/branches_edit.html', area=area, form=form)

@nodeview.route('/nodes/branches/delete/', methods=['POST'])
def branches_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            area = Area.query.get(id)
            if len(area.children) > 0:
                flash(u'删除分局失败，请先删除其所有接入点', 'error')
                return redirect(url_for('nodes.branches'))
            db.session.delete(area)
        db.session.commit()
        flash(u'删除分局成功','success')
        return redirect(url_for('nodes.branches'))

@nodeview.route('/nodes/entrances/new/', methods=['GET','POST'])
@login_required
def entrances_new():
    form = EntranceNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        area = Area()
        form.populate_obj(area)
        area.area_type = 4
        db.session.add(area)
        db.session.commit()
        flash(u'添加接入点成功', 'success')
        return redirect(url_for('nodes.entrances'))
    return render_template('nodes/areas/entrances_new.html', form = form)

@nodeview.route('/nodes/entrances/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def entrances_edit(id):
    form = EntranceNewForm()
    area = Area.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        del form._fields["cityid"]
        del form._fields["town"]
        form.populate_obj(area)
        area.updated_at = datetime.now()
        db.session.add(area)
        db.session.commit()
        flash(u'修改接入点成功','success')
        return redirect(url_for('nodes.entrances'))
    form.process(obj=area)
    return render_template('/nodes/areas/entrances_edit.html', area=area, form=form)

@nodeview.route('/nodes/entrances/delete/', methods=['POST'])
def entrances_delete():
    if request.method == 'POST':
        ids = request.form.getlist('id')
        for id in ids:
            area = Area.query.get(id)
            db.session.delete(area)
        db.session.commit()
        flash(u'删除接入点成功','success')
        return redirect(url_for('nodes.entrances'))
