#coding=utf-8

from datetime import datetime, timedelta
import calendar

from wtforms.ext.sqlalchemy.fields import  QuerySelectMultipleField
from flask_wtf import Form, TextField, SelectField, SelectMultipleField

from nodes.models import Vendor, Model, Node
from tango.models import db, Category
from .constants import DATES

def pull_intervals(key):
    now = datetime.now()
    WEEK = [u'星期一', u'星期二', u'星期三', u'星期四',
            u'星期五', u'星期六', u'星期日']
    res = []
    
    if not key or key == 'today':
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
        days_of_month = calendar.monthrange(*t)[1]
        res = [[str(i), u'%d号'%i] for i in range(1, days_of_month+1)]
        
    res.insert(0, ['__None', u'全部时段'])
    return res


# def vendor_choices():
#     vendors = Vendor.query.all()
#     blank_vendor = Vendor()
#     blank_vendor.id = '__None'
#     blank_vendor.alias = u'全部厂商'
#     vendors.insert(0, blank_vendor)
#     return vendors
    
def model_choices(vendors):
    def query_factory():
        query = Model.query
        if vendors and '__None' not in vendors:
            query = query.filter(Model.vendor_id.in_(vendors))
        models = query.order_by(Model.vendor_id).all()
        blank_model = Model()
        blank_model.id = '__None'
        blank_model.alias = u'全部型号'
        models.insert(0, blank_model)
        return models
    return query_factory

class QuerySelectMultipleFieldFix(QuerySelectMultipleField):
    def iter_choices(self):
        if self.allow_blank:
            # print self.data, self.blank_text
            yield (u'__None', self.blank_text,  not self.data or '__None' in self.data)
            
        for pk, obj in self._get_object_list():
            yield (pk, self.get_label(obj), obj in self.data)


    
    
def sampletime_filter(model, query, sampletime, intervals):
    now = datetime.now()
    one_day = timedelta(1)
    
    today_begin     = datetime(now.year, now.month, now.day)
    yesterday_begin = today_begin - one_day
    week_begin      = today_begin - one_day * now.weekday()
    lastweek_begin  = week_begin - one_day * 7
    month_begin     = today_begin - one_day * (now.day - 1)
    t_year, t_month = (now.year-1, 12) if now.month == 1 else (now.year, now.month-1)
    lastmonth_begin = datetime(t_year, t_month, 1)

    QUERY_DICT = {
        'today'     : (today_begin,     now,         model.samplehour),
        'yesterday' : (yesterday_begin, today_begin, model.samplehour),
        'thisweek'  : (week_begin,      now,         model.sampleweekday),
        'lastweek'  : (lastweek_begin,  week_begin,  model.sampleweekday),
        'thismonth' : (month_begin,     now,         model.sampleday),
        'lastmonth' : (lastmonth_begin, month_begin, model.sampleday),
    }
    
    begin_time, end_time, model_field = QUERY_DICT[sampletime]
    # print 'begin_time, end_time, model_field::', begin_time, end_time, model_field
    query = query.filter(db.and_(model.sampletime >= begin_time,
                                 model.sampletime < end_time))
    if intervals and '__None'not in intervals: # sampletime is NOT __None
        query = query.filter(model_field.in_(intervals))
    # print '========================================\n'
    return query

    
class PerfFilterForm(Form):
    keyword     = TextField()
    sampletime  = SelectField(u'时间', choices=DATES, default='today')
    intervals   = SelectMultipleField(u'时段', choices=pull_intervals('__None'), default=['__None'])
    vendors     = QuerySelectMultipleFieldFix(u'厂商', query_factory=lambda: Vendor.query,
                                              get_label='alias', allow_blank=True, blank_text=u'全部厂商')
    models      = QuerySelectMultipleFieldFix(u'型号', query_factory=lambda: Model.query, get_label='alias')

    def refresh_choices(self, args):
        self.intervals.choices = pull_intervals(args.get('sampletime'))
        self.models.query_factory = model_choices(args.get('vendors'))


    def filter(self, model):
        query = model.query
        # 'today' ...
        sampletime = self.sampletime.data 
        # [] or ['__None'] or ['1', '2'] or ['__None', '1', '2'] ...    
        intervals = self.intervals.data
        # the same as intervals
        vendor_ids = [v.id for v in self.vendors.data] if self.vendors.data else []
        # the same as intervals
        model_ids = [m.id for m in self.models.data]
        category_ids = [c.id for c in self.categories.data] if \
                       hasattr(self, 'categories') and self.categories.data else []
        
        # print '========================================'
        # print 'self.sampletime.data::', sampletime
        # print 'self.intervals.data::', intervals
        # print 'self.vendors.data::', vendor_ids
        # print 'self.models.data::', model_ids
        # print 'self.categories.data::', category_ids
        # print '========================================'
        # Filter Datetime
        query = sampletime_filter(model, query, sampletime, intervals)

        # Filter Category
        if category_ids and '__None' not in category_ids:
            query = query.filter(model.node.has(Node.category_id.in_(category_ids)))

        # Filter Model
        # *Except*: (vendor_ids is empty has '__None') AND (model_ids is empty OR has '__None')
        if (vendor_ids and '__None' not in vendor_ids) or \
           (model_ids and '__None' not in model_ids):
            if not model_ids or '__None' in model_ids:
                query =query.filter(model.node.has(Node.vendor_id.in_(vendor_ids)))
                # model_ids = [m.id for m in Model.query.filter(Model.vendor_id.in_(vendor_ids))]
            else:
                query = query.filter(model.node.has(Node.model_id.in_(model_ids)))
        return query


class NodePerfFilterForm(PerfFilterForm):
    categories  = QuerySelectMultipleFieldFix(u'分类', query_factory=lambda: Category.query.filter_by(obj='node'),
                                              get_label='alias', allow_blank=True, blank_text=u'全部类别')
