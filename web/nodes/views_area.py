# coding: utf-8

from datetime import datetime

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash,json, send_file

from sqlalchemy import func,or_
from sqlalchemy.orm import aliased

from tango import db, get_profile
from tango.ui import navbar, dashboard
from tango.ui.tables import make_table
from tango.login import current_user, login_required
from tango.models import Profile, Category
from tango.excel.CsvExport import CsvExport

from .models import Node, Area, Vendor, NODE_STATUS_DICT, Model
from .tables import CityTable, TownTable, BranchTable, EntranceTable
from .forms import CityNewForm, TownNewForm, BranchNewForm, EntranceNewForm
from .views import nodeview

@nodeview.route('/nodes/cities.csv/', methods=['POST', 'GET'])
@nodeview.route("/nodes/cities/", methods=['POST', 'GET'])
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
    export_columns = ['parent_id','name','alias','longitude','latitude','remark']
    query = 'db.session.query(Area.id,Area.name,Area.area_type,Area.alias,Area.longitude,Area.latitude,Area.parent_id,'
    for index,category in enumerate(['total']+[category.name for category in categories]):
        if category == "host": continue
        query += 'func.coalesce(sub_query_list[%(index)s].c.%(category)s_count,0).label("%(category)s_count"),' % {'index':index,'category': category}
        export_columns.append(category+"_count")
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
    if request.base_url.endswith(".csv/"):
        csv = CsvExport('cities',columns=export_columns)
        return send_file(csv.export(query,format={'parent_id': lambda value: Area.query.filter(Area.area_type==0).first().name}),as_attachment=True,attachment_filename='cities.csv')
    else:
        return render_template('nodes/areas/cities.html', table = table)

@nodeview.route('/nodes/towns.csv/', methods=['POST', 'GET'])
@nodeview.route("/nodes/towns/", methods=['POST', 'GET'])
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
    export_columns = ['parent_id','name','alias','longitude','latitude','remark']
    query = 'db.session.query(Area.id,Area.name,Area.area_type,Area.alias,Area.longitude,Area.latitude,Area.city_name.label("parent_id"),'
    for index,category in enumerate(['total']+[category.name for category in categories]):
        if category == "host": continue
        query += 'func.coalesce(sub_query_list[%(index)s].c.%(category)s_count,0).label("%(category)s_count"),' % {'index':index,'category': category}
        export_columns.append(category+"_count")
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
    if request.base_url.endswith(".csv/"):
        csv = CsvExport('towns',columns=export_columns)
        return send_file(csv.export(query),as_attachment=True,attachment_filename='towns.csv')
    else:
        return render_template('nodes/areas/towns.html', table = table)

@nodeview.route('/nodes/branches.csv/', methods=['POST', 'GET'])
@nodeview.route("/nodes/branches/", methods=['POST', 'GET'])
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
    export_columns = ['parent_id','name','alias','longitude','latitude','remark']
    query = 'db.session.query(Area.id,Area.name,Area.area_type,Area.alias,Area.longitude,Area.latitude,(Area.city_name+Area.town_name).label("parent_id"),'
    for index,category in enumerate(['total']+[category.name for category in categories]):
        if category == "host": continue
        query += 'func.coalesce(sub_query_list[%(index)s].c.%(category)s_count,0).label("%(category)s_count"),' % {'index':index,'category': category}
        export_columns.append(category+"_count")
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
    if request.base_url.endswith(".csv/"):
        csv = CsvExport('branches',columns=export_columns)
        return send_file(csv.export(query),as_attachment=True,attachment_filename='branches.csv')
    else:
        return render_template('nodes/areas/branches.html', table = table)

@nodeview.route('/nodes/entrances.csv/', methods=['POST', 'GET'])
@nodeview.route("/nodes/entrances/", methods=['POST', 'GET'])
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
    export_columns = ['parent_id','name','alias','longitude','latitude','remark']
    query = 'db.session.query(Area.id,Area.name,Area.area_type,Area.alias,Area.longitude,Area.latitude,(Area.city_name+Area.town_name+Area.branch_name).label("parent_id"),'
    for index,category in enumerate(['total']+[category.name for category in categories]):
        if category in ["host","olt","eoc"]: continue
        query += 'func.coalesce(sub_query_list[%(index)s].c.%(category)s_count,0).label("%(category)s_count"),' % {'index':index,'category': category}
        export_columns.append(category+"_count")
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
    if request.base_url.endswith(".csv/"):
        csv = CsvExport('entrances',columns=export_columns)
        return send_file(csv.export(query),as_attachment=True,attachment_filename='entrances.csv')
    else:
        return render_template('nodes/areas/entrances.html', table = table)

@nodeview.route('/nodes/cities/new/', methods=['GET','POST'])
@login_required
def cities_new():
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = CityNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        area = Area()
        form.populate_obj(area)
        if Area.query.filter(Area.area_type==1).filter(Area.name==area.name).count() > 0:
            flash(u'地市名称不能重复','error')
        elif Area.query.filter(Area.area_type==1).filter(Area.alias==area.alias).count() > 0:
            flash(u'地市别名不能重复','error')
        else:
            area.area_type = 1
            area.parent_id = Area.query.filter(Area.area_type==0).first().id
            db.session.add(area)
            db.session.commit()
            flash(u'添加地市 %s 成功'% area.name, 'success')
            return redirect(url_for('nodes.cities'))
    return render_template('nodes/areas/cities_new.html', form = form, next=next)

@nodeview.route('/nodes/cities/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def cities_edit(id):
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = CityNewForm()
    area = Area.query.get_or_404(id)
    if request.method == 'POST':
        if form.validate_on_submit():
            if area.name != form.name.data and Area.query.filter(Area.area_type==1).filter(Area.name==area.name).count() > 0:
                flash(u'地市名称不能重复','error')
            elif area.alias != form.alias.data and Area.query.filter(Area.area_type==1).filter(Area.alias==area.alias).count() > 0:
                flash(u'地市别名不能重复','error')
            else:
                form.populate_obj(area)
                area.updated_at = datetime.now()
                db.session.add(area)
                db.session.commit()
                flash(u'修改地市 %s 成功'% area.name,'success')
                return redirect(url_for('nodes.cities'))
    else:
        form.process(obj=area)
    return render_template('/nodes/areas/cities_edit.html', area=area, form=form, next=next)

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
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = TownNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        area = Area()
        form.populate_obj(area)
        if Area.query.filter(Area.area_type==2).filter(Area.name==area.name).count() > 0:
            flash(u'区县名称不能重复','error')
        elif Area.query.filter(Area.area_type==2).filter(Area.alias==area.alias).count() > 0:
            flash(u'区县别名不能重复','error')
        else:
            area.area_type = 2
            db.session.add(area)
            db.session.commit()
            flash(u'添加区县 %s 成功'% area.name, 'success')
            return redirect(url_for('nodes.towns'))
    return render_template('nodes/areas/towns_new.html', form = form, next=next)

@nodeview.route('/nodes/towns/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def towns_edit(id):
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = TownNewForm()
    area = Area.query.get_or_404(id)
    if request.method == 'POST':
        if form.validate_on_submit():
            if area.name != form.name.data and Area.query.filter(Area.area_type==2).filter(Area.name==area.name).count() > 0:
                flash(u'区县名称不能重复','error')
            elif area.alias != form.alias.data and Area.query.filter(Area.area_type==2).filter(Area.alias==area.alias).count() > 0:
                flash(u'区县别名不能重复','error')
            else:
                form.populate_obj(area)
                area.updated_at = datetime.now()
                db.session.add(area)
                db.session.commit()
                flash(u'修改区县 %s 成功'% area.name,'success')
                return redirect(url_for('nodes.towns'))
    else:
        form.process(obj=area)
    return render_template('/nodes/areas/towns_edit.html', area=area, form=form, next=next)

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
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = BranchNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        area = Area()
        form.populate_obj(area)
        if Area.query.filter(Area.area_type==3).filter(Area.name==area.name).count() > 0:
            flash(u'分局名称不能重复','error')
        elif Area.query.filter(Area.area_type==3).filter(Area.alias==area.alias).count() > 0:
            flash(u'分局别名不能重复','error')
        else:
            area.area_type = 3
            db.session.add(area)
            db.session.commit()
            flash(u'添加分局 %s 成功'% area.name, 'success')
            return redirect(url_for('nodes.branches'))
    return render_template('nodes/areas/branches_new.html', form = form, next=next)

@nodeview.route('/nodes/branches/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def branches_edit(id):
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = BranchNewForm()
    area = Area.query.get_or_404(id)
    if request.method == 'POST':
        if form.validate_on_submit():
            if area.name != form.name.data and Area.query.filter(Area.area_type==3).filter(Area.name==area.name).count() > 0:
                flash(u'分局名称不能重复','error')
            elif area.alias != form.alias.data and Area.query.filter(Area.area_type==3).filter(Area.alias==area.alias).count() > 0:
                flash(u'分局别名不能重复','error')
            else:
                del form._fields["cityid"]
                form.populate_obj(area)
                area.updated_at = datetime.now()
                db.session.add(area)
                db.session.commit()
                flash(u'修改分局 %s 成功'% area.name,'success')
                return redirect(url_for('nodes.branches'))
    else:
        form.process(obj=area)
    return render_template('/nodes/areas/branches_edit.html', area=area, form=form, next=next)

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
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = EntranceNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        area = Area()
        form.populate_obj(area)
        if Area.query.filter(Area.area_type==4).filter(Area.name==area.name).count() > 0:
            flash(u'接入点名称不能重复','error')
        elif Area.query.filter(Area.area_type==4).filter(Area.alias==area.alias).count() > 0:
            flash(u'接入点别名不能重复','error')
        else:
            area.area_type = 4
            db.session.add(area)
            db.session.commit()
            flash(u'添加接入点 %s 成功'% area.name, 'success')
            return redirect(url_for('nodes.entrances'))
    return render_template('nodes/areas/entrances_new.html', form = form, next=next)

@nodeview.route('/nodes/entrances/edit/<int:id>/', methods=['POST', 'GET'])
@login_required
def entrances_edit(id):
    next = request.form["next"] if request.form.get("next") else request.referrer
    form = EntranceNewForm()
    area = Area.query.get_or_404(id)
    if request.method == 'POST':
        if form.validate_on_submit():
            if area.name != form.name.data and Area.query.filter(Area.area_type==4).filter(Area.name==area.name).count() > 0:
                flash(u'接入点名称不能重复','error')
            elif area.alias != form.alias.data and Area.query.filter(Area.area_type==4).filter(Area.alias==area.alias).count() > 0:
                flash(u'接入点别名不能重复','error')
            else:
                del form._fields["cityid"]
                del form._fields["town"]
                form.populate_obj(area)
                area.updated_at = datetime.now()
                db.session.add(area)
                db.session.commit()
                flash(u'修改接入点 %s 成功'% area.name,'success')
                return redirect(url_for('nodes.entrances'))
    else:
        form.process(obj=area)
    return render_template('/nodes/areas/entrances_edit.html', area=area, form=form, next=next)

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


def validate_float(value):
    try:
        float(value)
        return True
    except ValueError:
        return False

import os
import operator
from flask import Markup
from werkzeug import secure_filename
from tango.excel.CsvImport import CsvImport,ImportColumn
@nodeview.route('/nodes/cities/import/', methods=['POST'])
@login_required
def cities_import():
    if request.method == 'POST':
        file = request.files['file']
        if file and file.filename.endswith('csv'):
            filename = secure_filename(file.filename)
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','static','file','upload')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file_path = os.path.join(root_path, filename.split('.')[0]+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
            file.save(file_path)
            reader = CsvImport(session=db.session.bind, table='areas')
            reader.addColumn(
                ImportColumn(u'所属区域', 'parent_id', 'integer',allow_null=False, existed_data=dict([(area.name, area.id) for area in Area.query.filter(Area.area_type==0)]),)
            ).addColumn(
                ImportColumn(u'地市名称', 'name', 'character varying(40)', is_key=True, allow_null=False)
            ).addColumn(
                ImportColumn(u'地市别名', 'alias', 'character varying(200)', allow_null=False)
            ).addColumn(
                ImportColumn(u'区域类型', 'area_type', 'integer', default=1)
            ).addColumn(
                ImportColumn(u'经度', 'longitude', 'double precision', format=validate_float)
            ).addColumn(
                ImportColumn(u'纬度', 'latitude', 'double precision', format=validate_float)
            ).addColumn(
                ImportColumn(u'备注', 'remark', 'character varying(200)')
            )
            update_dict = {}
            key_list = [column.name_en for column in reader.columns if column.is_key]
            attr_list = ['id',]+[column.name_en for column in reader.columns]
            for node in Area.query.filter(Area.area_type==1).all():
                f = operator.attrgetter(*key_list)
                key = (f(node),) if len(key_list) == 1 else f(node)
                f2 = operator.attrgetter(*attr_list)
                update_dict[key] = dict(zip(attr_list, f2(node)))
            reader.load_update(permit_update_dict=update_dict, update_dict=update_dict)
            info = reader.read(file=file_path)
            flash(Markup(info), 'success')
        else:
            flash(u"上传文件格式错误", 'error')
    return redirect(url_for('nodes.cities'))

@nodeview.route('/nodes/towns/import/', methods=['POST'])
@login_required
def towns_import():
    if request.method == 'POST':
        file = request.files['file']
        if file and file.filename.endswith('csv'):
            filename = secure_filename(file.filename)
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','static','file','upload')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file_path = os.path.join(root_path, filename.split('.')[0]+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
            file.save(file_path)
            reader = CsvImport(session=db.session.bind, table='areas')
            reader.addColumn(
                ImportColumn(u'所属区域', 'parent_id', 'integer',allow_null=False, existed_data=dict([(area.full_name, area.id) for area in Area.query.filter(Area.area_type==1)]),)
            ).addColumn(
                ImportColumn(u'区县名称', 'name', 'character varying(40)', is_key=True, allow_null=False)
            ).addColumn(
                ImportColumn(u'区县别名', 'alias', 'character varying(200)', allow_null=False)
            ).addColumn(
                ImportColumn(u'区域类型', 'area_type', 'integer', default=2)
            ).addColumn(
                ImportColumn(u'经度', 'longitude', 'double precision', format=validate_float)
            ).addColumn(
                ImportColumn(u'纬度', 'latitude', 'double precision', format=validate_float)
            ).addColumn(
                ImportColumn(u'备注', 'remark', 'character varying(200)')
            )
            update_dict = {}
            key_list = [column.name_en for column in reader.columns if column.is_key]
            attr_list = ['id',]+[column.name_en for column in reader.columns]
            for node in Area.query.filter(Area.area_type==2).all():
                f = operator.attrgetter(*key_list)
                key = (f(node),) if len(key_list) == 1 else f(node)
                f2 = operator.attrgetter(*attr_list)
                update_dict[key] = dict(zip(attr_list, f2(node)))
            reader.load_update(permit_update_dict=update_dict, update_dict=update_dict)
            info = reader.read(file=file_path)
            flash(Markup(info), 'success')
        else:
            flash(u"上传文件格式错误", 'error')
    return redirect(url_for('nodes.towns'))

@nodeview.route('/nodes/branches/import/', methods=['POST'])
@login_required
def branches_import():
    if request.method == 'POST':
        file = request.files['file']
        if file and file.filename.endswith('csv'):
            filename = secure_filename(file.filename)
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','static','file','upload')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file_path = os.path.join(root_path, filename.split('.')[0]+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
            file.save(file_path)
            reader = CsvImport(session=db.session.bind, table='areas')
            reader.addColumn(
                ImportColumn(u'所属区域', 'parent_id', 'integer',allow_null=False, existed_data=dict([(area.full_name, area.id) for area in Area.query.filter(Area.area_type==2)]),)
            ).addColumn(
                ImportColumn(u'分局名称', 'name', 'character varying(40)', is_key=True, allow_null=False)
            ).addColumn(
                ImportColumn(u'分局别名', 'alias', 'character varying(200)', allow_null=False)
            ).addColumn(
                ImportColumn(u'区域类型', 'area_type', 'integer', default=3)
            ).addColumn(
                ImportColumn(u'经度', 'longitude', 'double precision', format=validate_float)
            ).addColumn(
                ImportColumn(u'纬度', 'latitude', 'double precision', format=validate_float)
            ).addColumn(
                ImportColumn(u'备注', 'remark', 'character varying(200)')
            )
            update_dict = {}
            key_list = [column.name_en for column in reader.columns if column.is_key]
            attr_list = ['id',]+[column.name_en for column in reader.columns]
            for node in Area.query.filter(Area.area_type==3).all():
                f = operator.attrgetter(*key_list)
                key = (f(node),) if len(key_list) == 1 else f(node)
                f2 = operator.attrgetter(*attr_list)
                update_dict[key] = dict(zip(attr_list, f2(node)))
            reader.load_update(permit_update_dict=update_dict, update_dict=update_dict)
            info = reader.read(file=file_path)
            flash(Markup(info), 'success')
        else:
            flash(u"上传文件格式错误", 'error')
    return redirect(url_for('nodes.branches'))

@nodeview.route('/nodes/entrances/import/', methods=['POST'])
@login_required
def entrances_import():
    if request.method == 'POST':
        file = request.files['file']
        if file and file.filename.endswith('csv'):
            filename = secure_filename(file.filename)
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','static','file','upload')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file_path = os.path.join(root_path, filename.split('.')[0]+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
            file.save(file_path)
            reader = CsvImport(session=db.session.bind, table='areas')
            reader.addColumn(
                ImportColumn(u'所属区域', 'parent_id', 'integer',allow_null=False, existed_data=dict([(area.full_name, area.id) for area in Area.query.filter(Area.area_type==3)]),)
            ).addColumn(
                ImportColumn(u'接入点名称', 'name', 'character varying(40)', is_key=True, allow_null=False)
            ).addColumn(
                ImportColumn(u'接入点别名', 'alias', 'character varying(200)', allow_null=False)
            ).addColumn(
                ImportColumn(u'区域类型', 'area_type', 'integer', default=4)
            ).addColumn(
                ImportColumn(u'经度', 'longitude', 'double precision', format=validate_float)
            ).addColumn(
                ImportColumn(u'纬度', 'latitude', 'double precision', format=validate_float)
            ).addColumn(
                ImportColumn(u'备注', 'remark', 'character varying(200)')
            )
            update_dict = {}
            key_list = [column.name_en for column in reader.columns if column.is_key]
            attr_list = ['id',]+[column.name_en for column in reader.columns]
            for node in Area.query.filter(Area.area_type==4).all():
                f = operator.attrgetter(*key_list)
                key = (f(node),) if len(key_list) == 1 else f(node)
                f2 = operator.attrgetter(*attr_list)
                update_dict[key] = dict(zip(attr_list, f2(node)))
            reader.load_update(permit_update_dict=update_dict, update_dict=update_dict)
            info = reader.read(file=file_path)
            flash(Markup(info), 'success')
        else:
            flash(u"上传文件格式错误", 'error')
    return redirect(url_for('nodes.entrances'))