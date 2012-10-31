#coding=utf-8

from datetime import datetime, timedelta
import calendar

from wtforms.ext.sqlalchemy.fields import  QuerySelectMultipleField
from flask_wtf import Form, SelectField, SelectMultipleField

from nodes.models import Vendor, Model, Node
from tango.models import db
from .constants import DATES

def merge_queue(l):
    """
    Test:
    ========================================
    ll = ([1],
          [1, 2, 3, 4, 5, 6],
          [1, 4, 6, 9],
          [1, 2, 3, 6, 9],
          [1, 3, 6, 7, 8],
          [1, 3, 6, 7, 8, 11, 15])
    for l in ll:
        print merge_queue(l)
    ========================================
    >>> Output:
        [[1, 1]]
        [[1, 6]]
        [[1, 1], [4, 4], [6, 6], [9, 9]]
        [[1, 3], [6, 6], [9, 9]]
        [[1, 1], [3, 3], [6, 8]]
        [[1, 1], [3, 3], [6, 8], [11, 11], [15, 15]]
    """
    l.sort()
    q = []
    b = e = l[0]                # Begin, End
    ll = len(l)
    for i in range(ll):
        if i == ll - 1:
            if l[i] > l[i-1] + 1:
                q.append([b, e])
                q.append([l[i], l[i]])
            else:
                q.append([b, l[i]])
            break
            
        if l[i] > l[i-1] + 1:
            q.append([b, e])
            b = e = l[i]
        else:
            e = l[i]
    return q

    
def make_time_spaces(sampletime, intervals):
    now = datetime.now()
    one_hour = timedelta(0, 3600)
    one_day = timedelta(1)
    one_week = timedelta(7)
    
    today_begin = datetime(now.year, now.month, now.day)
    week_begin = today_begin - timedelta(now.weekday())
    month_begin = datetime(now.year, now.month, 1)
    
    yesterday_begin = today_begin - one_day
    lastweek_begin = week_begin - one_week
    t_year, t_month = (now.year-1, 12) if now.month == 1 else (now.year, now.month-1)
    lastmonth_begin = datetime(t_year, t_month, 1)
    
    TIME_DICT = {
        'today'     : (today_begin, now),
        'yesterday' : (yesterday_begin, today_begin),
        'thisweek'  : (week_begin, now),
        'lastweek'  : (lastweek_begin, week_begin),
        'thismonth' : (month_begin, now),
        'lastmonth' : (lastmonth_begin, month_begin)
    }
    # select All
    if not intervals or 'all' in intervals:
        return [TIME_DICT[sampletime]]
        
    # Not All
    time_unit = one_hour if sampletime in ['today', 'yesterday'] else one_day
    base_time = None
    BASE_TIME_DICT = {
        'today': today_begin,
        'yesterday': yesterday_begin,
        'thisweek' : week_begin,
        'lastweek' : lastweek_begin,
        'thismonth': month_begin,
        'lastmonth': lastmonth_begin,
    }
    base_time = BASE_TIME_DICT[sampletime]
    queue = merge_queue(intervals)
    time_spaces = [[base_time + t_begin*time_unit, base_time + (t_end+1)*time_unit] \
                   for t_begin, t_end in queue]
    return time_spaces

    
def pull_intervals(key):
    now = datetime.now()
    WEEK = [u'星期一', u'星期二', u'星期三', u'星期四',
            u'星期五', u'星期六', u'星期日']
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
        days_of_month = calendar.monthrange(*t)[1]
        res = [[str(i), u'%d号'%i] for i in range(1, days_of_month+1)]
        
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

    
def sampletime_filter(model, query, sampletime, intervals):
    now = datetime.now()
    one_day = timedelta(1)
    
    today_begin = datetime(now.year, now.month, now.day)
    yesterday_begin = today_begin - one_day
    week_begin = today_begin - one_day * now.weekday()
    lastweek_begin = week_begin - one_day * 7
    month_begin = today_begin - one_day * (now.day - 1)
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
    if intervals and 'all'not in intervals: # sampletime is NOT all
        query = query.filter(model_field.in_(intervals))
    # print '========================================\n'
    return query

    
class PerfFilterForm(Form):
    sampletime  = SelectField(u'时间', choices=DATES, default='all')
    intervals   = SelectMultipleField(u'时段', choices=pull_intervals('all'), default='all')
    vendors     = QuerySelectMultipleField(u'厂商', query_factory=vendor_choices, get_label='alias')
    models      = QuerySelectMultipleField(u'型号', query_factory=model_choices(['all']), get_label='alias')

    def refresh_choices(self, args):
        self.intervals.choices = pull_intervals(args.get('sampletime'))
        self.models.query_factory = model_choices(args.get('vendors'))


    def filter(self, model, query):
        # 'all' or 'today' ...
        sampletime = self.sampletime.data 
        # [] or ['all'] or ['1', '2'] or ['all', '1', '2'] ...    
        intervals = self.intervals.data 
        # the same as intervals
        vendor_ids = [v.id for v in self.vendors.data] if self.vendors.data else []
        # the same as intervals
        model_ids = [m.id for m in self.models.data]
        
        # print '========================================'
        # print 'self.sampletime.data::', sampletime
        # print 'self.intervals.data::', intervals
        # print 'self.vendors.data::', vendor_ids
        # print 'self.models.data::', model_ids
        # print '========================================'

        if sampletime != 'all':
            # print 'sampletime::', sampletime
            # print 'Filter::', intervals
            query = sampletime_filter(model, query, sampletime, intervals)

        # *Except*: (vendor_ids is empty has 'all') AND (model_ids is empty OR has 'all')
        if (vendor_ids and 'all' not in vendor_ids) or \
           (model_ids and 'all' not in model_ids):
            # print 'vendor_ids::', vendor_ids
            # print 'Filter::', model_ids
            if 'all' in model_ids:
                model_ids = [m.id for m in Model.query.filter(Model.vendor_id.in_(vendor_ids))]
            query = query.filter(model.node.has(Node.model_id.in_(model_ids)))
        return query
