#!/usr/bin/env python
#coding=utf-8


import re
from random import Random
from flask import Flask, request, render_template, redirect, url_for
from flask_sqlalchemy import SQLAlchemy

from flask_wtf import (Form, TextField, SubmitField, PasswordField, RadioField,
                          SelectMultipleField, SelectField, HiddenField, DateField,
                          IntegerField, TextAreaField, SubmitField, RecaptchaField,
                          ValidationError, validators, required, equal_to, email)
import tables

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://root:yawen00@localhost/test_table2'
app.config['DEBUG'] = True
app.config['SECRET_KEY'] = 'asdf_ASDF'
db = SQLAlchemy(app)


#### Models
class User(db.Model):
    __tablename__ = 'users'

    id                        = db.Column(db.Integer, primary_key=True)
    login                     = db.Column(db.String(40), unique=True)
    name                      = db.Column(db.String(100))
    email                     = db.Column(db.String(100))
    role_id                   = db.Column(db.Integer, db.ForeignKey('roles.id'))
    domain_id                 = db.Column(db.Integer, db.ForeignKey('domains.id'))
    department                = db.Column(db.String(100))
    telephone                 = db.Column(db.String(30))
    mobile                    = db.Column(db.String(30))
    memo                      = db.Column(db.String(1024))
    status                    = db.Column(db.String(40))
    crypted_password          = db.Column(db.String(40))
    salt                      = db.Column(db.String(40))
    created_at                = db.Column(db.DateTime)
    updated_at                = db.Column(db.DateTime)
    expired_at                = db.Column(db.DateTime)
    remember_token            = db.Column(db.String(40))
    remember_token_expires_at = db.Column(db.DateTime)

    role   = db.relation('Role')
    domain = db.relation('Domain')

class Domain(db.Model):
    __tablename__ = 'domains'

    id          = db.Column(db.Integer, primary_key=True)
    name        = db.Column(db.String(255), nullable=False, unique=True)  
    base        = db.Column(db.String(255), nullable=False)
    description = db.Column(db.String(255))


roles_permissions = db.Table('roles_permissions', db.Model.metadata,
                             db.Column('role_id', db.Integer,
                                       db.ForeignKey('roles.id', ondelete='CASCADE')),
                             db.Column('permission_id', db.Integer,
                                       db.ForeignKey('permissions.id', ondelete='CASCADE')))


class Role(db.Model):
    __tablename__ = 'roles'

    id          = db.Column(db.Integer, primary_key=True)    
    name        = db.Column(db.String(255), nullable=False, unique=True)
    description = db.Column(db.String(255))
    updated_at  = db.Column(db.DateTime)
    created_at  = db.Column(db.DateTime)

    permissions = db.relation('Permission', backref="roles", secondary=roles_permissions)


class Permission(db.Model):
    __tablename__ = 'permissions'

    id                 = db.Column(db.Integer, primary_key=True)
    name               = db.Column(db.String(255), nullable=False)  
    text               = db.Column(db.String(255), nullable=False)  
    module_name        = db.Column(db.String(255))
    module_text        = db.Column(db.String(255))
    controller_name    = db.Column(db.String(255))
    action_name        = db.Column(db.String(255))
    method             = db.Column(db.String(255), nullable=False, default='GET')
    format             = db.Column(db.String(255))
    description        = db.Column(db.String(255))
    default_permission = db.Column(db.Integer(1), default=0)
    order_seq          = db.Column(db.Integer)


def validate_mobile(message=None):
    def _validate_mobile(form, field):
        number = field.data
        mobile_patt = re.compile('^((13[0-9])|(15[^4,\\D])|(18[0,5-9]))\\d{8}$')
        if number and mobile_patt.match(number) is None:
            raise ValidationError(message or u'手机号码不合法')
    return _validate_mobile

#### Forms
class UserNewForm(Form):
    login            = TextField(u'用户名', validators=[required(message=u'必填')])
    name             = TextField(u'真实姓名', validators=[required(message=u'必填')])
    password         = PasswordField(u'密码', validators=[required(message=u'必填')])
    password_confirm = PasswordField(u'重复密码', validators=[required(message=u'必填'), equal_to('password', message=u'两次输入的密码不同')])
    role_id          = SelectField(u'角色', validators=[required(message=u'必填')],
                                   choices=[('', u'请选择角色')] + [(unicode(r.id), r.name) for r in Role.query])
    domain_id        = SelectField(u'管理域', validators=[required(message=u'必填')],
                                   choices=[('', u'请选择管理域')] + [(unicode(d.id), d.name) for d in Domain.query])
    department       = TextField(u'部门')
    email            = TextField(u'邮箱', validators=[required(message=u'必填'), email(message=u'不是合法的邮箱地址')])
    telephone        = TextField(u'电话')
    mobile           = TextField(u'手机', validators=[validate_mobile()])
    memo             = TextAreaField(u'备注')


class UserEditForm(Form):
    name             = TextField(u'真实姓名', validators=[required(message=u'必填')])
    role_id          = SelectField(u'角色', validators=[required(message=u'必填')],
                                   choices=[('', u'请选择角色')] + [(unicode(r.id), r.name) for r in Role.query])
    domain_id        = SelectField(u'管理域', validators=[required(message=u'必填')],
                                   choices=[('', u'请选择管理域')] + [(unicode(d.id), d.name) for d in Domain.query])
    department       = TextField(u'部门')
    email            = TextField(u'邮箱', validators=[required(message=u'必填'), email(message=u'不是合法的邮箱地址')])
    telephone        = TextField(u'电话')
    mobile           = TextField(u'手机', validators=[validate_mobile()])
    memo             = TextAreaField(u'备注')


#### Tables
class UserTable(tables.Table):
    check      = tables.CheckBoxColumn()
    edit_btn   = tables.EditBtnColumn(endpoint='user_edit')
    delete_btn = tables.DeleteBtnColumn(endpoint='user_delete')

    login          = tables.Column(verbose_name=u'用户名', orderable=True)
    name           = tables.Column(verbose_name=u'真实姓名')
    role_name      = tables.Column(verbose_name=u'角色名称', accessor='role.name')
    domain         = tables.Column(verbose_name=u'管理域', accessor='domain.name')
    email          = tables.EmailColumn(verbose_name=u'邮箱')
    department     = tables.Column(verbose_name=u'部门')
    telephone      = tables.Column(verbose_name=u'电话')
    # mobile         = tables.Column()
    # memo           = tables.Column()
    # status         = tables.Column()
    # created_at     = tables.DateTimeColumn(verbose_name=u'注册时间',format='%Y-%m-%d %H:%M')
    # remember_token = tables.Column()

    class Meta():
        model = User
        per_page = 3
        order_by = '-login'


#### Views
@app.route('/user/new/', methods=['POST', 'GET'])
def user_new():
    form = UserNewForm()
    if request.method == 'POST' and form.validate_on_submit():
        user = User()
        form.populate_obj(user)
        db.session.add(user)
        try:
            db.session.commit()
        except Exception:
            db.session.rollback()
        return redirect(url_for('users', error='YES'))
    return render_template('test_new_user.html',
                           form=form)


@app.route('/user/edit/<int:id>/', methods=['POST', 'GET'])
def user_edit(id):
    form = UserEditForm()
    user = User.query.get_or_404(id)

    if request.method == 'POST' and form.validate_on_submit():
        form.populate_obj(user)
        db.session.add(user)
        db.session.commit()
        return redirect(url_for('users'))

    form.process(obj=user)
    return render_template('test_edit_user.html',
                           user=user,
                           form=form)

@app.route('/users')
def users():
    keyword = request.args.get('keyword', '')

    query = User.query
    if keyword:
        query = query.filter(db.or_(User.name.ilike('%' + keyword + '%'),
                                    User.email.ilike('%' + keyword + '%'),
                                    User.role.has(Role.name.ilike('%' + keyword + '%'))))
    
    table = UserTable(query, request)
    return render_template('test_table.html',
                           table=table,
                           keyword=keyword)


@app.route('/user/delete/<int:id>/')
def user_delete(id):
    return 'Delete::' + User.query.get_or_404(id).name

@app.route('/user_s')
def user_s():
    users = ', '.join(['-'.join([user.name, user.role.name, user.domain.name])
                       for user in User.query])
    return users

@app.route('/roles')
def roles():
    roles = ', <hr /></br>'.join([' ---- '.join([role.name, '_'.join([p.name for p in role.permissions])])
                       for role in Role.query])
    return roles

@app.route('/permissions')
def permissions():
    permissions = ', <hr /></br>'.join([' ---- '.join([permission.name, '_'.join([r.name for r in permission.roles])])
                       for permission in Permission.query])
    return permissions

@app.route('/simple-permissions')
def simple_permissions():
    permissions = ', '.join([permission.name for permission in Permission.query])
    return permissions

@app.route('/domains')
def domains():
    domains = ', '.join([domain.name for domain in Domain.query])
    return domains

    
if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5002)

