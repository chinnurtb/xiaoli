#coding=utf-8

from flask_wtf import (Form, TextField, PasswordField, HiddenField, SelectField,
                       TextAreaField, ValidationError, required, equal_to, email)

class CategoryForm(Form):
    id       = TextField(validators=[required(message=u'必填')])
    obj      = TextField(u'分组', [required(message=u'必填')])
    name     = TextField(u'名称', [required(message=u'必填')])
    alias    = TextField(u'显示名')
    is_valid = SelectField(u'有效性', [required(message=u'必填')], choices=[(u'0', u'无效'),(u'1', u'有效')])
