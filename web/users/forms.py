#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re

from wtforms import BooleanField 
from wtforms import validators as v
from tango.ui.form.fields import SelectFieldPro
from .models import Domain, Role

from flask_wtf import (Form, TextField, PasswordField, HiddenField,
                       TextAreaField, ValidationError, required, equal_to, email)

def validate_mobile(message=None):
    def _validate_mobile(form, field):
        number = field.data
        mobile_patt = re.compile('^((13[0-9])|(15[^4,\\D])|(18[0,5-9]))\\d{8}$')
        if number and mobile_patt.match(number) is None:
            raise ValidationError(message or u'手机号码不合法')
    return _validate_mobile

class UserNewForm(Form):
    username         = TextField(u'用户名', validators=[required(message=u'必填')])
    name             = TextField(u'真实姓名', validators=[required(message=u'必填')])
    password         = PasswordField(u'密码', validators=[required(message=u'必填'), v.Length(min=6, max=20)])
    password_confirm = PasswordField(u'重复密码', validators=[required(message=u'必填'), equal_to('password', message=u'两次输入的密码不同')])
    
    role_id          = SelectFieldPro(u'角色', validators=[required(message=u'必填')],
                                      choices=lambda: [('', u'请选择角色')] + [(unicode(r.id), r.name) for r in Role.query])
    
    domain_id        = SelectFieldPro(u'管理域', validators=[required(message=u'必填')],
                                      choices=lambda: [('', u'请选择管理域')] + [(unicode(d.id), d.name) for d in Domain.query])
    
    department       = TextField(u'部门')
    email            = TextField(u'邮箱', validators=[required(message=u'必填'), email(message=u'不是合法的邮箱地址')])
    telephone        = TextField(u'电话')
    mobile           = TextField(u'手机', validators=[validate_mobile()])
    memo             = TextAreaField(u'备注')


class UserEditForm(Form):
    name             = TextField(u'真实姓名', validators=[required(message=u'必填')])
    
    role_id          = SelectFieldPro(u'角色', validators=[required(message=u'必填')],
                                   choices=lambda: [('', u'请选择角色')] + [(unicode(r.id), r.name) for r in Role.query])
    
    domain_id        = SelectFieldPro(u'管理域', validators=[required(message=u'必填')],
                                   choices=lambda: [('', u'请选择管理域')] + [(unicode(d.id), d.name) for d in Domain.query])
    
    department       = TextField(u'部门')
    email            = TextField(u'邮箱', validators=[required(message=u'必填'), email(message=u'不是合法的邮箱地址')])
    telephone        = TextField(u'电话')
    mobile           = TextField(u'手机', validators=[validate_mobile()])
    memo             = TextAreaField(u'备注')

    
class ResetPasswordForm(Form):
    username = TextField(u'用户名')
    newpasswd = PasswordField(u'新密码', [
        v.Required(message=u'必填'),
        v.Length(min=6, max=20),
        v.EqualTo('confirm', message=u'确认密码不匹配')
    ])
    confirm = PasswordField(u"确认密码", [v.Required(message=u'必填')])

class RoleForm(Form):
    name        = TextField(u'角色名', validators=[required(message=u'必填')])
    description = TextField(u'描述')

    
class DomainForm(Form):
    name        = TextField(u'名称', validators=[required(message=u'必填')])
    description = TextField(u'描述')

    
class LoginForm(Form):
    username = TextField(u'用户名')
    password = PasswordField(u'密码')
    next     = HiddenField()
    remember = BooleanField(u'记住我')


class ProfileForm(Form):
    name             = TextField(u'真实姓名', validators=[required(message=u'必填')])
    role_name        = TextField(u'角色名')
    domain_name      = TextField(u'管理域名')
    department       = TextField(u'部门')
    email            = TextField(u'邮箱', validators=[required(message=u'必填'), email(message=u'不是合法的邮箱地址')])
    telephone        = TextField(u'电话')
    mobile           = TextField(u'手机', validators=[validate_mobile()])
    memo             = TextAreaField(u'备注')
    
    
class PasswordForm(Form):
    oldpasswd = PasswordField(u"当前密码", [v.Required(message=u'必填')]) 
    newpasswd = PasswordField(u'新密码', [
        v.Required(message=u'必填'),
        v.Length(min=6, max=20),
        v.EqualTo('confirm', message=u'确认密码不匹配')
    ])
    confirm = PasswordField(u"确认密码", [v.Required(message=u'必填')])


