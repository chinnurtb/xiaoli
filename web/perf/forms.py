#coding=utf-8


from wtforms.ext.sqlalchemy.fields import QuerySelectField
from flask_wtf import Form, TextField, SelectField, IntegerField, TextAreaField, required

from tango.models import Category, db
from alarms.models import AlarmSeverity, AlarmClass
from .models import Metric

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
    
