# coding: utf-8

from wtforms.ext.sqlalchemy.fields import QuerySelectField
from flask_wtf import Form, TextField, DateTimeField, SelectField, TextAreaField, required 

from tango.models import DictType
from tango.form.fields import SelectFieldPro

from users.models import User

class SearchForm(Form):
    keyword = TextField()

class SettingEditForm(Form):
    name  = TextField(u'参数名')
    alias = TextField(u'说明')
    value = TextField(u'参数值', [required(message=u'必填'),])

class DictCodeNewEditForm(Form):
    type       = QuerySelectField(u'字典类型', [required(message=u'必填'),],
                                  query_factory=lambda: DictType.query, get_label='type_label', allow_blank=False)
    code_name = TextField(u'字典编码', [required(message=u'必填'),])
    code_label = TextField(u'字典值', [required(message=u'必填'),])
    is_valid   = SelectField(u'有效性', choices=[(u'0', u'无效'),(u'1', u'有效')])
    remark     = TextAreaField(u'备注')
    
    
class OplogFilterForm(Form):
    uid         = QuerySelectField(query_factory=lambda: User.query, allow_blank=True, blank_text=u'选择用户')
    start_date  = DateTimeField(u'开始时间', format='%Y-%m-%d')
    end_date    = DateTimeField(u'结束时间', format='%Y-%m-%d')
    keyword     = TextField()

    
