# coding: utf-8

from wtforms.ext.sqlalchemy.fields import QuerySelectField
from flask_wtf import (Form, TextField, DateTimeField, SelectField, SelectMultipleField,IntegerField, 
                       TextAreaField, required )

from tango.models import db, Category, DictType

from users.models import User

from alarms.models import AlarmSeverity, AlarmClass

from .models import Metric

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

def valid_node_categoryies():
    return Category.query.filter(db.and_(Category.is_valid==1,
                                         Category.obj=='node'))

class ThresholdEditForm(Form):
    alias       = TextField(u'显示名')
    category    = QuerySelectField(u'设备', query_factory=valid_node_categoryies, get_label='alias')
    metric      = QuerySelectField(u'指标', query_factory=lambda:Metric.query, get_label='alias')
    alarm_class = QuerySelectField(u'告警类型', query_factory=lambda:AlarmClass.query, get_label='alias')
    
    occur_cond1   = TextField(u'产生条件')
    restore_cond1 = TextField(u'恢复条件')
    severity1     = QuerySelectField(u'产生告警', get_label='alias',
                                     query_factory=lambda:AlarmSeverity.query.order_by(AlarmSeverity.id.desc()))

    occur_cond2   = TextField(u'产生条件')
    restore_cond2 = TextField(u'恢复条件')
    severity2     = QuerySelectField(u'产生告警', get_label='alias',
                                     query_factory=lambda:AlarmSeverity.query.order_by(AlarmSeverity.id.desc())) 
    
class ThresholdNewForm(ThresholdEditForm):
    enabled = SelectField(u'有效性', choices=[(u'0', u'无效'),(u'1', u'有效')])
    occur_count = IntegerField(u'产生数量')
    summary = TextAreaField(u'摘要')


class MetricNewEditForm(Form):
    grp    = TextField(u'分组', [required(message=u'必填')])
    name   = TextField(u'名称', [required(message=u'必填')])
    alias  = TextField(u'显示名', [required(message=u'必填')])
    calc   = SelectField(u'计算方法', [required(message=u'必填')], choices=[(u'counter', u'counter'), (u'gauge', u'gauge')])
    unit   = TextField(u'单位')
    format = TextField(u'格式')
    descr  = TextAreaField(u'说明')


class TimePeriodNewEditForm(Form):
    name       = TextField(u'名称', [required(message=u'必填'),])
    alias      = TextField(u'显示名')
    hour       = SelectMultipleField(u'小时', default="*",
                                     choices=[(u'*', u'所有')] + [(unicode(i), u'%d : 00' % i) for i in range(24)])
    dayofmonth = SelectMultipleField(u'日期', default="*",
                                     choices=[(u'*', u'所有')] + [(unicode(i), unicode(i)) for i in range(1, 32)])
    month      = SelectMultipleField(u'月份', default="*",
                                     choices=[(u'*', u'所有')] + [(unicode(i), unicode(i)) for i in range(1, 13)])
    dayofweek  = SelectMultipleField(u'星期', default="*",
                                     choices=[(u'*', u'所有')] + [(unicode(i), WEEK[i]) for i in range(7)])
    remark     = TextAreaField(u'备注')
    
    
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
