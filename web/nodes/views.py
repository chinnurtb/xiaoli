#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import db
from tango.ui import menus, Menu
from tango.ui import add_widget, Widget
from tango.login import current_user, login_required
from tango.models import Profile

from .models import Node, Board, Port
from .forms import NodeNewForm, NodeSearchForm
from .tables import NodeTable,PortTable,BoardTable

nodeview = Blueprint('nodes', __name__)

@nodeview.route('/nodes')
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
    profile = Profile.load(current_user.id, 'table-nodes')
    table = NodeTable(query).configure(profile)
    return render_template('nodes/index.html', table = table, form=form)

@nodeview.route('/nodes/<int:id>', methods=['GET'])
@login_required
def node_show(id):
    Node = Node.query.get_or_404(id)
    return render_template('nodes/show.html', node = node)

@nodeview.route('/nodes/new', methods=['GET','POST'])
@login_required
def node_new():
    form = NodeNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        # 父表插入记录出错，NotSupportedError: (NotSupportedError) 错误:  无法在关系"nodes"上执行INSERT RETURNING
        # HINT:  您需要一个无条件, 且带有RETURNING子句的ON INSERT DO INSTEAD的规则.
        #node = Node()
        #form.populate_obj(node)
        #node.status = 0
        #db.session.add(node)

        # 改用原始插入语句
        conn = db.engine.connect()
        statement = '''
            INSERT INTO nodes (
                name, addr, status, category, area_id, vendor_id, model_id, snmp_port,
                snmp_ver, snmp_comm, snmp_wcomm)
            VALUES (
                %(name)s, %(addr)s, %(status)s, %(category)s, %(area_id)s, %(vendor_id)s, %(model_id)s,
                %(snmp_port)s, %(snmp_ver)s, %(snmp_comm)s, %(snmp_wcomm)s
            )
        '''
        parameters = form.data
        parameters.update(status=0)
        conn.execute(statement,parameters)
        db.session.commit()
        flash(u'新建节点成功', 'info')
        return redirect(url_for('nodes.nodes'))
    return render_template('nodes/new.html', form = form)

@nodeview.route('/nodes/edit/<int:id>', methods=['POST', 'GET'])
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

@nodeview.route("/boards")
@login_required
def boards():
    profile = Profile.load(current_user.id, 'table-boards')
    table = BoardTable(Board.query).configure(profile)
    return render_template('boards/index.html', table = table)

@nodeview.route("/ports")
@login_required
def ports():
    profile = Profile.load(current_user.id, 'table-ports')
    table = PortTable(Port.query).configure(profile)
    return render_template('ports/index.html', table = table)

menus.append(Menu('nodes', u'资源', '/nodes'))

#col2
add_widget(Widget('dashboard3', 'Dashboard3', content='<div style="height:100px">Dashboard3</div>', column = 'side'))
add_widget(Widget('dashboard4', 'Dashboard4', content='<div style="height:100px">Dashboard4</div>', column = 'side'))
add_widget(Widget('dashboard5', 'Dashboard5', content='<div style="height:100px">Dashboard5</div>', column = 'side'))

