#!/usr/bin/env python 
# -*- coding: utf-8 -*-
from flask import json
from flask import (Blueprint, request, url_for, make_response, send_file,
                   redirect, render_template, flash)

from tango import db
from tango import user_profile
from tango.ui import menus, Menu
from tango.ui.tables import TableConfig
from tango.login import logout_user, login_user, current_user
from tango.models import Profile, QueryFilter 
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
        next = form.next.data
        user, authenticated = User.authenticate(username, password)
        DEBUG = True # For DEBUG
        print 'DEBUG::', DEBUG
        if user and authenticated or DEBUG: 
            remember = form.remember.data == 'y'
            if login_user(user, remember = remember):
                #flash(u'登录成功', 'success')
                if next:
                    return redirect(next)
                return redirect('/')
        elif not user:
            flash(u'用户不存在', 'error')
        else: 
            flash(u'密码错误', 'error')

        form.next.data = request.args.get('next', '')
    return render_template('login.html', form = form)

    
@userview.route('/logout', methods=['GET'])
def logout():
    logout_user()
    #flash(u'退出成功', 'success')
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
from tango.ui.queries import QueryForm, TextField, SelectField
from tango.models import QueryFilter
class UserQueryForm(QueryForm):
    username  = TextField(u'用户名', operator='ilike')
    name      = TextField(u'真实姓名', operator='ilike')
    domain_id = SelectField(u'管理域', operator='==',
                            choices=lambda: [('', u'请选择')] + [(unicode(d.id), d.name) for d in Domain.query])
    role_id   = SelectField(u'角色名', operator='==',
                            choices=lambda: [('', u'请选择')] + [(unicode(r.id), r.name) for r in Role.query])

    class Meta():
        model = User

        
@userview.route('/users/', methods=['GET', 'POST'])
def users():
    query = User.query
    query_form = UserQueryForm()
    keyword = ''
    filters = QueryFilter.query.filter(db.and_(QueryFilter.user_id==current_user.id,
                                           QueryFilter.table=='users')).all()
    if query_form.is_submitted():
        query_str = query_form.filters_str
        if 'save' in request.form.keys():
            filter = QueryFilter()
            filter.user_id = current_user.id
            filter.table = 'users'
            filter.query_str = query_str
            db.session.add(filter)
            db.sesssion.commit()
        query = query.filter(query_str)
    if keyword:
        query = query.filter(db.or_(User.name.ilike('%' + keyword + '%'),
                                    User.email.ilike('%' + keyword + '%'),
                                    User.role.has(Role.name.ilike('%' + keyword + '%'))))
    table = UserTable(query)
    profile = user_profile(UserTable._meta.profile)
    TableConfig(request, profile).configure(table)
    return render_template('users/index.html', table=table, keyword=keyword,
                           query_form=query_form, filters=filters)
    
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
            flash(u'添加用户(%s)成功' % user.username, 'success')
            return redirect(url_for('users.users'))
    return render_template('users/user_new.html', form=form)

    
@userview.route('/users/edit/<int:id>', methods=['POST', 'GET'])
def user_edit(id):
    form = UserEditForm()
    print form.data
    user = User.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(user)
        db.session.add(user)
        db.session.commit()
        flash(u'修改用户(%s)成功' % user.username, 'success')
        return redirect(url_for('users.users'))
    form.process(obj=user)
    return render_template('/users/user_edit.html', user=user, form=form)

    
@userview.route('/users/delete/<int:id>', methods=('GET', 'POST'))
def user_delete(id):
    user = User.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(user)
        db.session.commit()
        flash(u'用户(%s)删除成功' % user.username, 'success')
        return redirect(url_for('users.users'))
    return render_template('users/user_delete.html', user=user)

    
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
def roles():
    table = RoleTable(Role.query)
    profile = user_profile(RoleTable._meta.profile)
    TableConfig(request, profile).configure(table)
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
    print 'all_args::', all_args
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
    

@userview.route('/roles/delete/<int:id>', methods=('GET', 'POST'))
def role_delete(id):
    user_cnt = User.query.filter(User.role_id == id).count()
    role = Role.query.get(id)
    if user_cnt > 0:
        flash(u'删除失败: 有(%d)个用户依赖此角色' % user_cnt, 'error')
    elif request.method == 'GET':
        return render_template('users/role_delete.html', role=role)
    else:
        db.session.delete(role)
        db.session.commit()
        flash(u'删除角色(%s)成功' % role.name, 'success')
    return redirect(url_for('users.roles'))
    
# ==============================================================================
#  Domain
# ==============================================================================
@userview.route('/domains/')
def domains():
    profile = user_profile(DomainTable._meta.profile)
    table = DomainTable(Domain.query)
    TableConfig(request, profile).configure(table)
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
    domain_areas = [domain.city_list, domain.town_list,domain.branch_list, domain.entrance_list]
    domain_areas = ','.join([d for d in domain_areas if d])
    form.process(obj=domain)
    return render_template('users/domain_new_edit.html',
                           action=url_for('users.domain_edit', id=id),
                           domain_areas=domain_areas, form=form)
    

@userview.route('/domains/delete/<int:id>', methods=('GET', 'POST'))
def domain_delete(id):
    user_cnt = User.query.filter(User.domain_id == id).count()
    domain = Domain.query.get_or_404(id)
    if user_cnt > 0:
        flash(u'删除失败: 有(%d)个用户依赖此管理域!' % user_cnt, 'error')
    elif request.method == 'GET':
        return render_template('users/domain_delete.html', domain=domain)        
    else:
        db.session.delete(domain)
        db.session.commit()
        flash(u'管理域(%s)删除成功' % domain.name, 'success')
    return redirect(url_for('users.domains'))
    

# ==============================================================================
#  [OTHER]
# ==============================================================================
menus.append(Menu('users', u'用户', '/users/'))

@userview.route('/just-test')
def just_test():
    return render_template('just_test.html')
