#!/usr/bin/env python 
# coding: utf-8

from flask import json
from flask import (Blueprint, request, url_for, redirect, render_template, flash)

from tango import db, cache, update_profile

from tango.ui import navbar
from tango.base import NestedDict
from tango.ui.tables import make_table
from tango.login import logout_user, login_user, current_user
from tango.models import Profile
from tango.forms import SearchForm

from nodes.models import Area
from .models import User, Role, Permission, Domain
from .forms import (UserEditForm, UserNewForm, LoginForm, PasswordForm, RoleForm,
                    ProfileForm, DomainForm, ResetPasswordForm)
from .tables import UserTable, RoleTable, DomainTable

userview = Blueprint('users', __name__)

@userview.context_processor
def inject_navid():
    return dict(navid = 'users')

# ===============================================================================
#  Authenticating
# =============================================================================== 
@userview.route('/login', methods=['GET', 'POST'])
def login():
    form = LoginForm(next=request.args.get('next', ''))
    if request.method == 'POST':
        username = form.username.data
        password = form.password.data
        next = form.next.data
        user, authenticated = User.authenticate(username, password)
        if user and authenticated:
            remember = (form.remember.data == 'y')
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
    return redirect('/login')

    
# ==============================================================================
#  Setting
# ==============================================================================
@userview.route('/settings')
def settings():
    return redirect(url_for('users.user_info'))
    

@userview.route('/settings/profile', methods=['POST', 'GET'])
def profile():
    uid = current_user.id
    args = request.values
    update_profile(args['grp'], args['key'], args['value'])
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
def users():
    query = User.query
    form = SearchForm(formdata=request.args)
    keyword = form.keyword.data
    if keyword and keyword != '':
        kw = '%' + keyword + '%'
        query = query.filter(db.or_(User.name.ilike(kw),
                                    User.email.ilike(kw),
                                    User.role.has(Role.name.ilike(kw))))        
    table = make_table(query, UserTable)
    return render_template('users/index.html', table=table, form=form)
    
@userview.route('/users/new', methods=['POST', 'GET'])
def users_new():
    form = UserNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        username = form.username.data
        user = User.query.filter_by(username=username).first()
        if user is None:
            user = User()
            form.populate_obj(user)
            db.session.add(user)
            db.session.commit()
            flash(u'用户(%s)添加成功' % user.username, 'success')
            return redirect(url_for('users.users'))
    return render_template('users/new.html', form=form)

    
@userview.route('/users/edit/<int:id>', methods=['POST', 'GET'])
def users_edit(id):
    form = UserEditForm()
    user = User.query.get_or_404(id)
    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(user)
        db.session.add(user)
        db.session.commit()
        cache.delete("user-"+str(id))
        flash(u'用户(%s)修改成功' % user.username, 'success')
        return redirect(url_for('users.users'))
        
    form.process(obj=user)
    kwargs = {
        'title'  : u'修改用户',
        'menuid' : 'users',
        'action' : url_for('users.users_edit', id=id),
        'form'   : form,
    }
    return render_template('users/edit.html', **kwargs)

    
@userview.route('/users/delete/<int:id>', methods=('GET', 'POST'))
def users_delete(id):
    user = User.query.get_or_404(id)
    if request.method == 'POST':
        db.session.delete(user)
        db.session.commit()
        cache.delete("user-"+str(id))
        flash(u'用户(%s)删除成功' % user.username, 'success')
        return redirect(url_for('users.users'))
        
    kwargs = {
        'title'  : u'删除用户',
        'action' : url_for('users.users_delete', id=id),
        'fields' : [(u'用户名', user.username), (u'真实姓名', user.name)],
        'type'   : 'delete'
    }
    return render_template('tango/_modal.html', **kwargs)

    
@userview.route('/users/delete/all', methods=['GET', 'POST'])
def users_delete_all():
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            cache.delete("user-"+str(i))
            db.session.delete(User.query.get(int(i)))
        db.session.commit()
        flash(u'成功删除 %d 个用户!' % len(ids) , 'success')
        return redirect(url_for('users.users'))

    ids = dict(request.values.lists()).get('id[]', [])
    users = User.query.filter(User.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title': u'批量删除用户',
        'action': url_for('users.users_delete_all'),
        'fields': [(u.id, u'用户名', u.username) for u in users],
        'type' : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)

    
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
    return render_template('users/reset_password.html', form=form, user=user)



    
# ==============================================================================
#  Role
# ============================================================================== 
@userview.route('/roles/')
def roles():
    query = Role.query
    form = SearchForm(formdata=request.args)
    keyword = form.keyword.data
    if keyword and keyword != '':
        query = query.filter(Role.name.ilike('%'+keyword+'%'))
    table = make_table(query, RoleTable)
    return render_template('users/roles/index.html', table=table, form=form)
    
@userview.route('/roles/new', methods=['GET', 'POST'])
def roles_new():
    all_args = NestedDict(request)
    perms = all_args['permissions']
    form = RoleForm()
    role = Role()
    if request.method == 'POST' and form.validate_on_submit() and perms:
        for p in perms.keys():
            perm = Permission.query.get(int(p))
            role.permissions.append(perm)
            
        form.populate_obj(role)
        db.session.add(role)
        db.session.commit()
        flash(u'角色(%s)添加成功' % role.name , 'success')
        return redirect(url_for('users.roles'))

    if request.method == 'POST' and not perms:
        flash(u'权限选项为必选!', 'error')
    perm_tree = Permission.make_tree()
    
    kwargs = {
        'title'     : u'添加角色',
        'menuid'    : 'roles-new',
        'action'    : url_for('users.roles_new'),
        'form'      : form,
        'perm_tree' : perm_tree,
    }
    return render_template('users/roles/new-edit.html', **kwargs)
    

@userview.route('/roles/edit/<int:id>', methods=['POST', 'GET'])
def roles_edit(id):
    all_args = NestedDict(request)
    perms = all_args['permissions']
    form = RoleForm()
    role = Role.query.get_or_404(id)
    print perms
    if request.method == 'POST' and form.validate_on_submit() and perms:
        # 请空原来的数据
        while len(role.permissions) > 0:
            role.permissions.pop(0)
        for p in perms:
            perm = Permission.query.get(int(p))
            role.permissions.append(perm)

        form.populate_obj(role)
        db.session.add(role)
        db.session.commit()
        flash(u'角色(%s)修改成功' % role.name, 'success')
        return redirect(url_for('users.roles'))

    if request.method == 'POST' and not perms:
        flash(u'权限选项为必选!', 'error')
    perm_tree = Permission.make_tree(role.permissions)
    form.process(obj=role)
    
    kwargs = {
        'title'     : u'修改角色',
        'menuid'    : 'roles',
        'action'    : url_for('users.roles_edit', id=id),
        'form'      : form,
        'perm_tree' : perm_tree,
    }
    return render_template('users/roles/new-edit.html', **kwargs)

    

@userview.route('/roles/delete/<int:id>', methods=('GET', 'POST'))
def roles_delete(id):
    user_cnt = User.query.filter(User.role_id == id).count()
    role = Role.query.get(id)
    if user_cnt > 0:
        kwargs = {
            'title'  : u'删除失败!!!',
            'method' : 'GET',
            'action' : url_for('.roles'),
            'fields' : [(u'原因', u'删除失败: 有(%d)个用户依赖此角色' % user_cnt)],
        }
        return render_template('tango/_modal.html', **kwargs)
    elif request.method == 'GET':
        kwargs = {
            'title'  : u'删除角色',
            'action' : url_for('users.roles_delete', id=id),
            'fields' : [(u'角色名', role.name), (u'描述', role.description)],
            'type'   : 'delete'
        }
        return render_template('tango/_modal.html', **kwargs)
    else:
        db.session.delete(role)
        cache.delete("role-"+str(id))
        db.session.commit()
        flash(u'删除角色(%s)成功' % role.name, 'success')
        return redirect(url_for('users.roles'))
    
@userview.route('/roles/delete/all', methods=['GET', 'POST'])
def roles_delete_all():
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            db.session.delete(Role.query.get(int(i)))
            cache.delete("role-"+str(i))
        db.session.commit()
        flash(u'成功删除 %d 个角色!' % len(ids) , 'success')
        return redirect(url_for('users.roles'))

    ids = dict(request.values.lists()).get('id[]', [])
    roles = Role.query.filter(Role.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title': u'批量删除角色',
        'action': url_for('users.roles_delete_all'),
        'fields': [(r.id, u'角色名', r.nameb) for r in roles],
        'type' : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)

    
# ==============================================================================
#  Domain
# ==============================================================================
@userview.route('/domains/')
def domains():
    query = Domain.query
    form = SearchForm(formdata=request.args)
    keyword = form.keyword.data
    if keyword and keyword != '':
        query = query.filter(Domain.name.ilike('%'+keyword+'%'))
    table = make_table(query, DomainTable)
    return render_template('users/domains/index.html', table=table, form=form)
    
@userview.route('/domains/load/nodes')
def domains_load_nodes():
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
def domains_new():
    form = DomainForm()
    if request.method == 'POST' and form.validate_on_submit():
        domain = Domain()
        form.populate_obj(domain)
        domain.dump_areas(request.form['domain_areas'])
        db.session.add(domain)
        db.session.commit()
        flash(u'管理域(%s)添加成功!' % domain.name, 'success')
        return redirect(url_for('users.domains'))
    return render_template('users/domains/new_edit.html',
                           action=url_for('users.domains_new'),
                           form=form)

    
@userview.route('/domains/edit/<int:id>', methods=['POST', 'GET'])
def domains_edit(id):
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
    return render_template('users/domains/new_edit.html',
                           action=url_for('users.domains_edit', id=id),
                           domain_areas=domain_areas, form=form)
    

@userview.route('/domains/delete/<int:id>', methods=('GET', 'POST'))
def domains_delete(id):
    user_cnt = User.query.filter(User.domain_id == id).count()
    domain = Domain.query.get_or_404(id)
    if user_cnt > 0:
        kwargs = {
            'title'  : u'删除失败!!!',
            'method' : 'GET',
            'action' : url_for('.domains'),
            'fields' : [(u'原因', u'删除失败: 有(%d)个用户依赖此管理域' % user_cnt)],
        }
        return render_template('tango/_modal.html', **kwargs)
    elif request.method == 'GET':
        kwargs = {
            'title'  : u'删除管理域',
            'action' : url_for('users.domains_delete', id=id),
            'fields' : [(u'管理域名', domain.name), (u'描述', domain.description)],
            'type'   : 'delete'
        }
        return render_template('tango/_modal.html', **kwargs)
    else:
        db.session.delete(domain)
        db.session.commit()
        flash(u'管理域(%s)删除成功' % domain.name, 'success')
        return redirect(url_for('users.domains'))
    
    
@userview.route('/domains/delete/all', methods=['GET', 'POST'])
def domains_delete_all():
    if request.method == 'POST':
        ids = dict(request.values.lists()).get('id', [])
        for i in ids:
            cache.delete("domain-"+str(i))
            db.session.delete(Domain.query.get(int(i)))
        db.session.commit()
        flash(u'成功删除 %d 个管理域!' % len(ids) , 'success')
        return redirect(url_for('users.domains'))

    ids = dict(request.values.lists()).get('id[]', [])
    domains = Domain.query.filter(Domain.id.in_([int(i) for i in ids])).all()
    kwargs = {
        'title': u'批量删除管理域',
        'action': url_for('users.domains_delete_all'),
        'fields': [(d.id, u'管理域名', d.name) for d in domains],
        'type' : 'delete'
    }
    return render_template('tango/_modal_del_all.html', **kwargs)
    

# ==============================================================================
#  [OTHER]
# ==============================================================================
navbar.add('users', u'用户', 'user', '/users/')

# ==============================================================================
#  Test
# ==============================================================================
@userview.route('/just-test/<name>/')
def just_test(name='a'):
    print '\n'.join([item for item in dir(request) if item.find('__') == -1])
    args =  request.args.to_dict()
    args.update(request.view_args)
    print args
    return name

@userview.route('/test-modal')
def test_modal():
    return render_template('users/test_modal.html')

