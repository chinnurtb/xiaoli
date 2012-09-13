#!/usr/bin/env python 
# -*- coding: utf-8 -*-
import re
from flask import json
from flask import (Blueprint, request, url_for,
                   redirect, render_template, flash)

from tango import db
from tango.ui import menus, Menu
from tango.login import logout_user, login_user, current_user

from tango.models import Profile
from tango.base import NestedDict
from nodes.models import Area
from .models import User, Role, Permission, Domain
from .forms import (UserEditForm, UserNewForm, LoginForm, PasswordForm, RoleForm,
                    ProfileForm, DomainForm, ResetPasswordForm)
from .tables import UserTable, RoleTable, DomainTable


userview = Blueprint('users', __name__)


# ===============================================================================
#  Authenticating
# =============================================================================== 
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
                flash(u'登录成功', 'success')
                return redirect('/')
        elif not user:
            flash(u'用户不存在', 'error')
        else: 
            flash(u'密码错误', 'error')

        # if session.get('login_time', None) is None:
        #     session['login_time'] = 1
        # else:
        #     session['login_time'] += 1
        # if session['login_time'] > 10:
        #     abort(403)
            
    return render_template('login.html', form = form)

    
@userview.route('/logout', methods=['GET'])
def logout():
    logout_user()
    flash(u'退出成功', 'success')
    return redirect('/login')


    
# ==============================================================================
#  Setting
# ==============================================================================
@userview.route('/settings')
def settings():
    return redirect(url_for('users.user_info'))

@userview.route('/settings/profile', methods=['POST', 'GET'])
def profile():
    args = request.values
    grp = args.get('grp')
    key = args.get('key')
    value = args.get('value')
    profile = Profile(current_user.id, grp, key, value)
    profile.update()
    db.session.commit()
    if request.method == 'GET':
        return redirect(request.referrer)
    else:
        return 'Updated!'
    
@userview.route('/settings/info', methods=['POST', 'GET'])
def user_info():
    '''用户修改自己的个人信息'''
    
    form = ProfileForm()
    user = User.query.get_or_404(current_user.id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(user)
        db.session.commit()
        flash(u'个人信息修改成功', 'success')
        return redirect(url_for('users.user_info'))
    
    form.process(obj=user)
    form.role_name.data = user.role.name
    form.domain_name.data = user.domain.name
    return render_template('settings/user_info.html', form=form)

    
@userview.route('/settings/password', methods=['POST', 'GET'])
def change_password():
    '''用户修改自己的密码'''
    
    form = PasswordForm()
    if request.method == 'POST' and form.validate():
        user = User.query.get_or_404(current_user.id)
        if user.check_passwd(form.oldpasswd.data):
            user.password = form.newpasswd.data
            db.session.commit()
            flash(u"密码修改成功", 'success')
        else:
            flash(u'当前密码错误', 'error')
    return render_template("settings/password.html", form = form)




# ==============================================================================
#  User
# ==============================================================================     
@userview.route('/users/')
@userview.route('/users/<int:page>')
def users(page=1):
        
    profile = Profile.load(current_user.id, UserTable._meta.profile_grp)
    order_by = request.args.get('order_by', None)
    keyword = request.args.get('keyword', '')

    query = User.query
    if keyword:
        query = query.filter(db.or_(User.name.ilike('%' + keyword + '%'),
                                    User.email.ilike('%' + keyword + '%'),
                                    User.role.has(Role.name.ilike('%' + keyword + '%'))))
    
    table = UserTable(query).configure(profile, page=page, order_by=order_by)
    return render_template('users/index.html', table=table, keyword=keyword)

    
@userview.route('/users/new', methods=['POST', 'GET'])
def user_new():
    form = UserNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        username = form.username.data
        user = User.query.filter_by(username=username).first()
        if user is None:
            user = User()
            form.populate_obj(user)
            db.session.add(user)
            db.session.commit()
            flash(u'添加用户(%s)成功' % user.usernmae, 'success')
            return redirect(url_for('users.users'))
    return render_template('users/user_new.html', form=form)

    
@userview.route('/users/edit/<int:id>', methods=['POST', 'GET'])
def user_edit(id):
    form = UserEditForm()
    user = User.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(user)
        db.session.add(user)
        db.session.commit()
        flash(u'修改用户(%s)成功' % user.username, 'success')
        return redirect(url_for('users.users'))
    form.process(obj=user)
    return render_template('/users/user_edit.html', user=user, form=form)

    
@userview.route('/users/delete/<int:id>')
def user_delete(id):
    user = User.query.get_or_404(id)
    db.session.delete(user)
    db.session.commit()
    flash(u'用户(%s)删除成功' % user.username, 'success')
    return redirect(url_for('users.users'))

    
@userview.route('/users/reset-password/<int:id>', methods=['POST', 'GET'])
def reset_password(id):
    '''管理员重置用户密码'''
    
    form = ResetPasswordForm()
    user = User.query.get(id)
    if request.method == 'POST' and form.validate_on_submit():
        newpasswd = form.newpasswd.data
        user.password = newpasswd
        db.session.commit()
        flash(u'用户(%s)密码重置成功' % user.username , 'success')
        return redirect(url_for('users.users'))
        
    form.username.data = user.username
    return render_template('users/reset_password.html', form=form, id=id)


    
# ==============================================================================
#  Role
# ============================================================================== 
@userview.route('/roles/')
@userview.route('/roles/<int:page>')
def roles(page=1):
    order_by = request.args.get('order_by', None)
    
    profile = Profile.load(current_user.id, RoleTable._meta.profile_grp)
    table = RoleTable(Role.query).configure(profile, page=page, order_by=order_by)
    return render_template('users/roles.html', table=table)
    
    
@userview.route('/roles/new', methods=['GET', 'POST'])
def role_new():
    all_args = NestedDict(request)
    perms = all_args['permissions']
    form = RoleForm()
    role = Role()
    if request.method == 'POST' and form.validate_on_submit():
        for p in perms.keys():
            perm = Permission.query.get(int(p))
            role.permissions.append(perm)
            
        form.populate_obj(role)
        db.session.add(role)
        db.session.commit()
        flash(u'新建角色成功', 'success')
        return redirect(url_for('users.roles'))

    perm_tree = Permission.make_tree()
    
    return render_template('users/role_new_edit.html',
                           action=url_for('users.role_new'),
                           form=form, perm_tree=perm_tree)
    

@userview.route('/roles/edit/<int:id>', methods=['POST', 'GET'])
def role_edit(id):
    all_args = NestedDict(request)
    perms = all_args['permissions']
    form = RoleForm()
    role = Role.query.get_or_404(id)
    
    if request.method == 'POST' and form.validate_on_submit():
        while len(role.permissions) > 0:
            role.permissions.pop(0)
        for p in perms:
            perm = Permission.query.get(int(p))
            role.permissions.append(perm)

        form.populate_obj(role)
        db.session.add(role)
        db.session.commit()
        flash(u'修改角色(%s)成功' % role.name, 'success')
        return redirect(url_for('users.roles'))
    
    perm_tree = Permission.make_tree(role.permissions)
    form.process(obj=role)
    return render_template('users/role_new_edit.html',
                           action=url_for('users.role_edit', id=id),
                           form=form,
                           perm_tree=perm_tree)
    

@userview.route('/roles/delete/<int:id>')
def role_delete(id):
    user_cnt = User.query.filter(User.role_id == id).count()
    if user_cnt > 0:
        flash(u'删除失败: 有(%d)个用户依赖此角色' % user_cnt, 'error')
    else:
        role = Role.query.get(id)
        db.session.delete(role)
        db.session.commit()
        flash(u'删除角色(%s)成功' % role.name, 'success')
    return redirect(url_for('users.roles'))

    
    
# ==============================================================================
#  Domain
# ==============================================================================
    
@userview.route('/domains/')
@userview.route('/domains/<int:page>')
def domains(page=1):
    order_by = request.args.get('order_by', None)
    
    profile = Profile.load(current_user.id, DomainTable._meta.profile_grp)
    table = DomainTable(Domain.query).configure(profile, page=page, order_by=order_by)
    return render_template('users/domains.html', table=table)

    
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
    
    return json.dumps(nodes)    


@userview.route('/domains/new', methods=['POST', 'GET'])
def domain_new():
    form = DomainForm()
    if request.method == 'POST' and form.validate_on_submit():
        domain = Domain()
        form.populate_obj(domain)
        domain.dump_areas(request.form['domain_areas'])
        db.session.add(domain)
        db.session.commit()
        return redirect(url_for('users.domains'))
    return render_template('users/domain_new_edit.html',
                           action=url_for('users.domain_new'),
                           form=form)

    
@userview.route('/domains/edit/<int:id>', methods=['POST', 'GET'])
def domain_edit(id):
    form = DomainForm()
    domain = Domain.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(domain)
        domain.dump_areas(request.form['domain_areas'])
        db.session.add(domain)
        db.session.commit()
        flash(u'管理域(%s)修改成功' % domain.name, 'success')
        return redirect(url_for('users.domains'))
    domain_areas = ','.join([domain.city_list, domain.town_list,
                             domain.branch_list, domain.entrance_list])
    form.process(obj=domain)
    return render_template('users/domain_new_edit.html',
                           action=url_for('users.domain_edit', id=id),
                           domain_areas=domain_areas, form=form)
    

@userview.route('/domains/delete/<int:id>')
def domain_delete(id):
    user_cnt = User.query.filter(User.domain_id == id).count()
    if user_cnt > 0:
        flash(u'删除失败: 有(%d)个用户依赖此管理域!' % user_cnt, 'error')
    else:
        domain = Domain.query.get_or_404(id)
        db.session.delete(domain)
        db.session.commit()
        flash(u'管理域(%s)删除成功' % domain.name, 'success')
    return redirect(url_for('users.domains'))
    


# ==============================================================================
#  [OTHER]
# ==============================================================================
menus.append(Menu('users', u'用户', '/users/'))
