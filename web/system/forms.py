# coding: utf-8

from flask_wtf import Form, TextField, DateTimeField 

from users.models import User

from tango.form.fields import SelectFieldPro

from wtforms.ext.sqlalchemy.fields import QuerySelectField

class SearchForm(Form):
    keyword = TextField()

class OplogFilterForm(Form):
    uid         = QuerySelectField(query_factory=lambda: User.query, allow_blank=True, blank_text=u'选择用户')
    start_date  = DateTimeField(u'开始时间', format='%Y-%m-%d')
    end_date    = DateTimeField(u'结束时间', format='%Y-%m-%d')
    keyword     = TextField()
    
