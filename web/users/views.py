#!/usr/bin/env python 
# -*- coding: utf-8 -*-
import re
from hashlib import md5

from flask import (Blueprint, request, session, url_for,
                   redirect, render_template, g, flash)

from tango import db

from tango import login_mgr

from tango.ui import menus, Menu

from tango.login import logout_user, login_user, current_user, \
    login_required

from tango.models import Profile
from .models import User, Role, Permission, Domain
from .forms import UserEditForm, UserNewForm, LoginForm, PasswordForm
from .tables import UserTable




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
def users():
    profile = Profile.load(1)
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
    return render_template('users/new.html', form=form)

    
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
    return render_template('/users/edit.html', user=user, form=form)

@userview.route('/users/delete/<int:id>/')
def user_delete(id):
    return 'Delete::' + User.query.get_or_404(id).name
#### User [END]


    
#### Role [BEGIN]
@userview.route('/roles')
def roles():
    roles = ', <hr /></br>'.join([' ---- '.join([role.name, '_'.join([p.name for p in role.permissions])])
                       for role in Role.query])
    return roles
#### Role [END]    

    
#### Permission [BEGIN]
@userview.route('/permissions')
def permissions():
    permissions = ', <hr /></br>'.join([' ---- '.join([permission.name, '_'.join([r.name for r in permission.roles])])
                       for permission in Permission.query])
    return permissions
#### Permission [END]
    
    
#### Domain [BEGIN]
@userview.route('/domains')
def domains():
    domains = ', '.join([domain.name for domain in Domain.query])
    return domains
#### Domain [END]

menus.append(Menu('users', u'用户', '/users'))
