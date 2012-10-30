#coding=utf-8


from wtforms.ext.sqlalchemy.fields import QuerySelectField, QuerySelectMultipleField
from flask_wtf import Form, TextField, SelectField, SelectMultipleField, IntegerField, TextAreaField, required

import operator
from tango.models import Category, db
from alarms.models import AlarmSeverity, AlarmClass
from datetime import datetime
import calendar
from .models import Metric

from .constants import DATES

from nodes.models import Vendor, Model


def pull_intervals(key):
    now = datetime.now()
    WEEK = [u'星期一', u'星期二', u'星期三', u'星期四', u'星期五', u'星期六', u'星期日']
    res = []
    
    if key == 'today':
        res = [[str(i), u'%d点'%i] for i in range(now.hour + 1)]
    elif key == 'yesterday':
        res = [[str(i), u'%d点'%i] for i in range(24)]
    elif key == 'thisweek':
        res = [[str(i), WEEK[i]] for i in range(now.weekday() + 1)]
    elif key == 'lastweek':
        res = [[str(i), WEEK[i]] for i in range(7)]
    elif key == 'thismonth':
        res = [[str(i), u'%d号'%i] for i in range(1, now.day+1)]
    elif key == 'lastmonth':
        t = (now.year-1, 12) if now.month == 1 else (now.year, now.month-1)
        day_num = calendar.monthrange(*t)[1]
        res = [[str(i), u'%d号'%i] for i in range(1, day_num+1)]
        
    res.insert(0, ['all', u'全部时段'])
    return res


def vendor_choices():
    vendors = Vendor.query.all()
    blank_vendor = Vendor()
    blank_vendor.id = 'all'
    blank_vendor.alias = u'全部厂商'
    vendors.insert(0, blank_vendor)
    return vendors
    
def model_choices(vendors):
    def query_factory():
        query = Model.query
        if vendors and 'all' not in vendors:
            query = query.filter(Model.vendor_id.in_(vendors))
        models = query.order_by(Model.vendor_id).all()
        blank_model = Model()
        blank_model.id = 'all'
        blank_model.alias = u'全部型号'
        models.insert(0, blank_model)
        return models
    return query_factory


    
class PerfFilterForm(Form):
    sampletime  = SelectField(u'时间', choices=DATES, default='all')
    intervals   = SelectMultipleField(u'时段', choices=pull_intervals('all'), default='all')
    vendors     = QuerySelectMultipleField(u'厂商', query_factory=vendor_choices, get_label='alias')
    models      = QuerySelectMultipleField(u'型号', query_factory=model_choices(['all']), get_label='alias')


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
    
