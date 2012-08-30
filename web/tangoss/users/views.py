#!/usr/bin/env python 
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango import menus, Menu, login_mgr

from tango.login import logout_user, login_user, current_user, \
    login_required

from .models import User, Role

from .forms import SignupForm, LoginForm, PasswordForm 

from hashlib import md5

from tango import db

userview = Blueprint('users', __name__)

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

@userview.route('/settings')
@login_required
def profile():
    form = PasswordForm(request.form)
    if request.method == 'POST' and form.validate():
        passwd = md5(form.newpasswd.data).hexdigest()
        db.session.query(User).filter_by(id = current_user.id).update({password:passwd})
        db.session.commit()
        flash("密码修改成功", 'info')
    return render_template("settings.html", passwdForm = form)

@userview.route('/logout', methods=['GET'])
@login_required
def logout():
    logout_user(),
    return redirect('/')

@userview.route('/users')
@login_required
def users():
    users = User.query.all()
    return render_template("users/index.html", users = users)

@userview.route('/users/search', methods=['POST'])
def search():
    username = request.form['search']
    users = User.query.filter_by(username=username).all()
    return render_template('users/index.html', menuid = 'users', submenuid = 'users', users=users)
    
@userview.route('/users/new', methods=['GET', 'POST'])
def create():
    form = SignupForm(request.form)
    if request.method == 'POST' and form.validate():
        username = form.username.data
        user = User.query.filter_by(username=username).first()
        if user is None:
            hash_passwd = md5(form.password.data).hexdigest()
            user = User(form.username.data, form.email.data, password=hash_passwd)
            db.session.add(user)
            db.session.commit()
            flash(u'添加用户成功', 'info')
            return redirect(url_for('users'))
        flash(u'用户名已存在', 'error')
    return render_template('users/new.html', form=form)

menus.append(Menu('users', u'用户', '/users'))

