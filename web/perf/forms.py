#coding=utf-8

from datetime import datetime
import calendar

from wtforms.ext.sqlalchemy.fields import  QuerySelectMultipleField
from flask_wtf import Form, SelectField, SelectMultipleField

from nodes.models import Vendor, Model
from .constants import DATES


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
