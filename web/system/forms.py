# coding: utf-8

from wtforms.ext.sqlalchemy.fields import QuerySelectField
from flask_wtf import (Form, TextField, DateTimeField, SelectField, SelectMultipleField,
                       TextAreaField, required )

from tango.models import DictType
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
    code_name  = TextField(u'字典编码', [required(message=u'必填'),])
    code_label = TextField(u'字典值', [required(message=u'必填'),])
    is_valid   = SelectField(u'有效性', choices=[(u'0', u'无效'),(u'1', u'有效')])
    remark     = TextAreaField(u'备注')

class DictCodeFilterForm(Form):
    type      = QuerySelectField(u'字典类型', query_factory=lambda: DictType.query,
                                 get_label='type_label', allow_blank=True, blank_text=u'选择字典类型')
    is_valid  = SelectField(u'有效性', choices=[(u'', u'选择有效性'), (u'0', u'无效'), (u'1', u'有效')],
                            filters=(lambda data: data if data != u'None' else '',))

    
WEEK = (u'星期日', u'星期一', u'星期二', u'星期三', u'星期四', u'星期五', u'星期六')
class TimePeriodNewEditForm(Form):
    name       = TextField(u'名称', [required(message=u'必填'),])
    alias      = TextField(u'显示名')
    hour       = SelectMultipleField(u'小时', choices=[(u'*', u'所有')] + [(unicode(i), unicode(i)) for i in range(24)])
    dayofmonth = SelectMultipleField(u'日期', choices=[(u'*', u'所有')] + [(unicode(1), unicode(1)) for i in range(1, 32)])
    month      = SelectMultipleField(u'月份', choices=[(u'*', u'所有')] + [(unicode(i), unicode(i)) for i in range(1, 13)])
    dayofweek  = SelectMultipleField(u'星期', choices=[(u'*', u'所有')] + [(unicode(i), WEEK[i]) for i in range(7)])

    
class OplogFilterForm(Form):
    uid         = QuerySelectField(query_factory=lambda: User.query, allow_blank=True, blank_text=u'选择用户')
    start_date  = DateTimeField(u'开始时间', format='%Y-%m-%d')
    end_date    = DateTimeField(u'结束时间', format='%Y-%m-%d')
    keyword     = TextField()


class NodeHostEditForm(Form):
    name  = TextField(u'名称')
    alias = TextField(u'显示名')
    ifaces = TextField(u'接口地址')
    remark = TextAreaField(u'备注')
