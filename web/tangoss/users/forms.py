#!/usr/bin/env python
# -*- coding: utf-8 -*-

from wtforms import Form, TextField, PasswordField, BooleanField 

from wtforms import validators as v

class SignupForm(Form):
    username = TextField(u'用户名', [
        v.Required(),
        v.Length(min=4, max=20),
        v.Regexp('[a-zA-Z0-9]+', message = u'只允许字母和数字')

    ])
    email = TextField(u'邮箱', [
        v.Required(),
        v.email(u'邮箱格式错误')
    ])
    password = PasswordField(u'密码', [
        v.Required(),
        v.Length(min=6, max=20),
        v.EqualTo('confirm', message=u'确认密码不匹配')
    ])
    confirm = PasswordField(u'确认密码', [
        v.Required()
    ])

class LoginForm(Form):
    username = TextField(u'用户名')
    password = PasswordField(u'密码')
    remember = BooleanField(u'记住我')

class AccountForm(Form):
    pass

class PasswordForm(Form):
    oldpasswd = PasswordField(u"当前密码", [
        v.Required()
    ]) 
    newpasswd = PasswordField(u'新密码', [
        v.Required(),
        v.Length(min=6, max=20),
        v.EqualTo('confirm', message=u'确认密码不匹配')
    ])
    confirm = PasswordField(u"确认密码", [
        v.Required()
    ])
    
