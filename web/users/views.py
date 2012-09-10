#!/usr/bin/env python 
# -*- coding: utf-8 -*-
import re
from hashlib import md5

from flask import json
from flask import (Blueprint, request, session, url_for,
                   redirect, render_template, g, flash, make_response)

from tango import db
from tango import login_mgr
from tango.ui import menus, Menu
from tango.login import logout_user, login_user, current_user, \
    login_required

from tango.models import Profile
from nodes.models import Area, AREA_CITY, AREA_TOWN, AREA_BRANCH, AREA_ENTRANCE
from .models import User, Role, Permission, Domain
from .forms import UserEditForm, UserNewForm, LoginForm, PasswordForm, RoleForm, DomainForm
from .tables import UserTable, RoleTable, DomainTable


userview = Blueprint('users', __name__)

#### Authenticating [BEGIN]
@userview.route('/login', methods=['GET', 'POST'])
def login():
    form = LoginForm(request.form)
    if request.method == 'POST':
        username = form.username.data
        password = form.password.data
        user, authenticated = User.authenticate(username, password)
        if user and authenticated:
            remember = form.remember.data == 'y'
            if login_user(user, remember = remember):
                return redirect('/')
        elif not user:
            flash(u'用户不存在', 'error')
        else: 
            flash(u'密码错误', 'error')
    return render_template('login.html', form = form)

    
@userview.route('/logout', methods=['GET'])
# @login_required
def logout():
    logout_user()
    return redirect('/')
#### Authenticating [END]

    
#### Setting [BEGIN]
@userview.route('/settings')
# @login_required
def profile():
    form = PasswordForm(request.form)
    if request.method == 'POST' and form.validate():
        passwd = md5(form.newpasswd.data).hexdigest()
        db.session.query(User).filter_by(id = current_user.id).update({password:passwd})
        db.session.commit()
        flash("密码修改成功", 'info')
    return render_template("settings.html", passwdForm = form)
#### Setting [END]    


#####  User [BEGIN]
@userview.route('/users/')
@login_required
def users():
    profile = Profile.load(current_user.id, "table-users")
    keyword = request.args.get('keyword', '')
    query = User.query
    if keyword:
        query = query.filter(db.or_(User.name.ilike('%' + keyword + '%'),
                                    User.email.ilike('%' + keyword + '%'),
                                    User.role.has(Role.name.ilike('%' + keyword + '%'))))
    
    table = UserTable(query).configure(profile, page=1)
    return render_template('users/index.html', table=table, keyword=keyword)

    
@userview.route('/users/new/', methods=['POST', 'GET'])
def user_new():
    form = UserNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        username = form.username.data
        user = User.query.filter_by(username=username).first()
        if user is None:
            #TODO: How to set password hash?
            user = User()
            form.populate_obj(user)
            user.role_id = 0
            user.domain_id = 0
            user.group_id = 0
            db.session.add(user)
            db.session.commit()
            flash(u'添加用户成功', 'info')
            return redirect(url_for('users.users'))
    return render_template('users/user_new.html', form=form)

    
@userview.route('/users/edit/<int:id>/', methods=['POST', 'GET'])
def user_edit(id):
    form = UserEditForm()
    user = User.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(user)
        db.session.add(user)
        db.session.commit()
        return redirect(url_for('users'))

    form.process(obj=user)
    return render_template('/users/user_edit.html', user=user, form=form)

@userview.route('/users/delete/<int:id>/')
def user_delete(id):
    pass
#### User [END]

    
#### Role [BEGIN]
def get_permissions(form):
    # print '-------BEGIN get_permissions--------'
    perms = []
    pattern = "^%s\[(.+)\]$" % 'permissions'
    # print 'form.keys()::', form.keys()
    # print 'form.values()::', form.values()
    for key in form.keys():
        if form[key] != 'on': continue
        # print 'key,form[key]::', key,form[key]
        m = re.match(pattern, key)
        if m:
            try:
                perm_id = int(m.group(1))
                perm = Permission.query.get(perm_id)
                perms.append(perm)
                # print 'perm.id::', perm.id
            except Exception, e:
                print 'Exception in get_permissions::', e
    return perms
    
def make_permission_tree(all_perms, role_perms=None):
    perm_tree = {}
    for p in all_perms:
        module_checked = ''
        name_checked = ''
        operation_checked = ''
        if role_perms is not None:
            for rp in role_perms:
                if p.module == rp.module:
                    module_checked = 'checked'
                if p.name == rp.name:
                    name_checked = 'checked'
                if p.id == rp.id:
                    operation_checked = 'checked'
        module_key = (p.module_text, module_checked)
        name_key = (p.name, name_checked)
        operation_key = (p.operation, operation_checked)
        
        if not perm_tree.get(module_key, None):
            perm_tree[module_key] = {}
            perm_tree[module_key][name_key] = {}
        if not perm_tree[module_key].get(name_key, None):
            perm_tree[module_key][name_key] = {}
        perm_tree[module_key][name_key][operation_key] = p.id
        
    return perm_tree


@userview.route('/roles')
def roles():
    profile = {}
    table = RoleTable(Role.query).configure(profile, page=1)
    return render_template('users/roles.html', table=table)
    
    
@userview.route('/roles/new', methods=['GET', 'POST'])
def role_new():
    perms = get_permissions(request.form)
    form = RoleForm()
    role = Role()
    if request.method == 'POST' and form.validate_on_submit():
        for p in perms:
            role.permissions.append(p)
            
        form.populate_obj(role)
        db.session.add(role)
        db.session.commit()
        return redirect(url_for('users.roles'))

    perm_all = Permission.query.all()
    perm_tree = make_permission_tree(perm_all)
    
    return render_template('users/role_new_edit.html',
                           action=url_for('users.role_new'),
                           form=form, perm_tree=perm_tree)
    

@userview.route('/roles/edit/<int:id>', methods=['POST', 'GET'])
def role_edit(id):
    perms = get_permissions(request.form)
    form = RoleForm()
    role = Role.query.get_or_404(id)
    
    if request.method == 'POST' and form.validate_on_submit():
        while len(role.permissions) > 0:
            role.permissions.pop(0)
        for p in perms:
            role.permissions.append(p)
            
        form.populate_obj(role)
        db.session.add(role)
        db.session.commit()
        return redirect(url_for('users.roles'))
    
    perm_all = Permission.query.all()
    perm_tree = make_permission_tree(perm_all, role.permissions)
    form.process(obj=role)
    return render_template('users/role_new_edit.html',
                           action=url_for('users.role_edit', id=id),
                           form=form,
                           perm_tree=perm_tree)
    

@userview.route('/roles/delete/<int:id>')
def role_delete(id):
    role = Role.query.get(id)
    db.session.delete(role)
    db.session.commit()
    return redirect(url_for('users.roles'))
    
#### Role [END]    

    
#### Permission [BEGIN]
## pass ##
#### Permission [END]
    
    
#### Domain [BEGIN]
@userview.route('/domains/load/nodes')
def domain_load_nodes():
    key = request.args.get('key', '')
    domain_areas = request.args.get('domain_areas', '')
    domain_areas = [int(area_id) for area_id in domain_areas.split(',') if area_id]\
                   if domain_areas else []

    root = Area.query.filter(Area.area_type==0).first()
    areas = []
    if key:
        key = int(key)
    if domain_areas:
        areas = [Area.query.get(area_id) for area_id in domain_areas]
        
    path_nodes = set([root.id])
    if not key:
        for area in areas:
            while area.parent_id != -1:
                path_nodes.add(area.parent_id)
                area = Area.query.get(area.parent_id)

    # print 'domain_areas::', domain_areas
    def make_node(area):
        node = {}
        node['title'] = area.name
        node['key'] = str(area.id)
        if area.id in domain_areas:
            node['select'] = True
        if len(area.children) > 0:
            node['isLazy'] = True
        return node

    def make_nodes(area_id):
        area = Area.query.get(area_id)
        node = make_node(area)
        if len(area.children) > 0:
            node['expand'] = True
            node['children'] = []
            for child in area.children:
                if child.id in path_nodes:
                    node['children'].append(make_nodes(child.id))
                else:
                    child_node = make_node(child)
                    node['children'].append(child_node)
        return node
    nodes = [make_node(area) for area in Area.query.filter(Area.parent_id==key)] \
            if key else [make_nodes(root.id)]
    # print 'key::', key
    # print 'domain_areas::', domain_areas
    # print 'nodes::', nodes
    return json.dumps(nodes)    
    
@userview.route('/domains')
def domains():
    profile = {}
    table = DomainTable(Domain.query).configure(profile, page=1)
    return render_template('users/domains.html', table=table)



def save_domain(domain, form, req):
    form.populate_obj(domain)
        
    domain_areas = [int(area_id) for area_id in req.form['domain_areas'].split(',') if area_id]
    areas = [Area.query.get(id) for id in domain_areas]
    city_list = []
    town_list = []
    branch_list = []
    entrance_list = []
    area_list = { AREA_CITY: city_list,
                  AREA_TOWN: town_list,
                  AREA_BRANCH: branch_list,
                  AREA_ENTRANCE: entrance_list}
    for area in areas:
        if area.area_type in (AREA_CITY, AREA_TOWN, AREA_BRANCH, AREA_ENTRANCE):
            area_list[area.area_type].append(str(area.id))
    domain.city_list = (',').join(city_list)
    domain.town_list = (',').join(town_list)
    domain.branch_list = (',').join(branch_list)
    domain.entrance_list = (',').join(entrance_list)
    db.session.add(domain)
    db.session.commit()

    
@userview.route('/domains/new', methods=['POST', 'GET'])
def domain_new():
    form = DomainForm()
    if request.method == 'POST' and form.validate_on_submit():
        domain = Domain()
        save_domain(domain, form, request)
        return redirect(url_for('users.domains'))
    return render_template('users/domain_new_edit.html',
                           action=url_for('users.domain_new'),
                           form=form)

@userview.route('/domains/edit/<int:id>', methods=['POST', 'GET'])
def domain_edit(id):
    form = DomainForm()
    domain = Domain.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        save_domain(domain, form, request)
        return redirect(url_for('users.domains'))
    domain_areas = ','.join([domain.city_list, domain.town_list,
                             domain.branch_list, domain.entrance_list])
    form.process(obj=domain)
    return render_template('users/domain_new_edit.html',
                           action=url_for('users.domain_edit', id=id),
                           domain_areas=domain_areas, form=form)
    

@userview.route('/domains/delete/<int:id>')
def domain_delete(id):
    domain = Domain.query.get_or_404(id)
    db.session.delete(domain)
    db.session.commit()
    return redirect(url_for('users.domains'))
    

#### Domain [END]

menus.append(Menu('users', u'用户', '/users'))
